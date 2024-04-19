#![allow(unused_imports)]
extern crate deb;
use deb::{*, error::*, elf::*, log::*, symbols::*, types::*, arena::*, util::*};
use std::{fs::{File}, io::{self, Write}, sync::Arc, mem, sync::atomic::{AtomicBool, Ordering}, cell::UnsafeCell, thread, thread::JoinHandle};
use memmap2::Mmap;

fn main() -> Result<()> {
    std::env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} binary", args[0]);
        std::process::exit(1);
    }
    let path = args[1].clone();

    dbg!(mem::size_of::<TypeInfo>());
    dbg!(mem::size_of::<StructField>());
    dbg!(mem::size_of::<Enumerand>());
    dbg!(mem::size_of::<FunctionInfo>());
    dbg!(mem::size_of::<Subfunction>());
    dbg!(mem::size_of::<SubfunctionPcRange>());
    dbg!(mem::size_of::<LocalVariable>());
    dbg!(mem::size_of::<LineInfo>());
    dbg!(mem::size_of::<FileInfo>());

    let file = File::open(&path)?;
    let mmap = unsafe { Mmap::map(&file)? };
    let elf = Arc::new(ElfFile::from_mmap(path.clone(), mmap)?);
    let symbols;

    {
        let _prof = ProfileScope::new("loading symbols".to_string());

        let status = Arc::new(SymbolsLoadingStatus::new());
        let num_threads = thread::available_parallelism().map_or(8, |n| n.get()) - 1;
        let loader = Arc::new(SyncUnsafeCell::new(SymbolsLoader::new(elf, num_threads, status.clone())?));
        let num_shards = unsafe {&*loader.get()}.num_shards;
        let mut stage_idx = 1usize;
        while unsafe {&mut *loader.get()}.prepare_stage(stage_idx)? {
            eprintln!("stage {}", stage_idx);
            let threads: Vec<JoinHandle<()>> = (0..num_shards).map(|shard_idx| {
                let loader_copy = loader.clone();
                let status_copy = status.clone();
                thread::spawn(move || if let Err(e) = unsafe {&*loader_copy.get()}.run(stage_idx, shard_idx) {
                    status_copy.cancel.store(true, Ordering::Relaxed);
                    eprintln!("error: {}", e);
                })
            }).collect();
            for t in threads {
                t.join().unwrap();
            }
            if status.cancel.load(Ordering::Relaxed) {
                return err!(Dwarf, "failed to load");
            }
            stage_idx += 1;
        }
        symbols = Arc::into_inner(loader).unwrap().into_inner().into_result();
        eprint!("\r");
    }

    //dbg!(symbols.types.types.len());
    //dbg!(symbols.types.name_to_type.len());
    //dbg!(symbols.types.type_names.calculate_used_bytes());
    //dbg!(symbols.types.misc_strings.calculate_used_bytes());
    //dbg!(symbols.types.type_names.calculate_count());
    //dbg!(symbols.types.misc_strings.calculate_count());
    //dbg!(symbols.types.fields.len());
    //dbg!(symbols.types.enumerands.len());
    //dbg!(symbols.types.descriptor_to_type.len());
    //dbg!(symbols.local_variables.len());

    let mut out = io::BufWriter::new(io::stdout());
    writeln!(out, "{} functions", symbols.functions.len())?;
    //for i in 0..symbols.types.types.len() {
        //writeln!(out, "{:?}", DumpType {types: &symbols.types, idx: TypeIdx(i)})?;
    //}

    /*
    for i in 0..symbols.local_variables.len() {
        let v = &symbols.local_variables[i];
        writeln!(out, "{}: {} {:?}", i, unsafe {v.name()}, v)?;
    }
    for f in &symbols.addr_to_function {
        writeln!(out, "{:x}: {}  {:?}", f.addr, unsafe {symbols.function_mangled_names.get_str_or(StringRef(f.mangled_name), "")}, f.local_variables)?;
    }*/

    Ok(())
}
