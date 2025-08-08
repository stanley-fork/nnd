// I gave up on this for now.
// Problems:
//  * The mangling scheme is way too complicated.
//  * Even if we just want function names, we have to parse pretty much the whole grammar, because template arguments may have arbitrary complicated type names and expressions.
//  * Even if we want to ignore parts of the name (e.g. function arguments), we have to parse them anyway because they can be referenced by substitutions later.
fn main() {}
/*
use std::{fmt, str};

type Result<T> = std::result::Result<T, fmt::Error>;

struct Ctx {
    out: String,
}

macro_rules! return_err {
    () => (
        return Err(fmt::Error::default());
    );
}

macro_rules! require {
    ($pred:expr) => (
        if !($pred) {
            return_err!()
        }
    );
}

impl From<str::Utf8Error> for fmt::Error { fn from(error: str::Utf8Error) -> Self { Self::default(); } }

// Function names are grammar symbols from https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling

fn mangled_name(ctx: &mut Ctx, s: &mut &[u8]) -> Result<()> {
    // <mangled-name> ::= _Z <encoding> [ . <vendor-specific suffix> ]

    require!(s.starts_with(b"_Z"));
    *s = &s[2..];
    encoding(ctx, s)?;

    if !s.is_empty() {
        // <vendor-specific suffix>
        require!(s[0] == b'.')
            self.out.write_str(str::from_utf8(*s)?)?;
    }

    Ok(())
}

fn encoding(ctx: &mut Ctx, s: &mut &[u8]) -> Result<()> {
    // <encoding> ::= <name> [ <bare-function-type> ]
    //            ::= <special-name>

    // <special-name> ::= TV <type>  # virtual table
    //                ::= TT <type>  # VTT structure (construction vtable index)
    //                ::= TI <type>  # typeinfo structure
    //                ::= TS <type>  # typeinfo name (null-terminated byte string)
    //                ::= T <call-offset> <base encoding>
    //                ::= Tc <call-offset> <call-offset> <base encoding>
    //                ::= GV <object name>	# Guard variable for one-time initialization
    //                ::= GR <object name> _             # First temporary
    //                ::= GR <object name> <seq-id> _    # Subsequent temporaries
    //                ::= GTt <encoding>
    // <call-offset> ::= h <nv-offset> _
	//               ::= v <v-offset> _
    // <nv-offset> ::= <offset number>  # non-virtual base override
    // <v-offset>  ::= <offset number> _ <virtual offset number>  # virtual base override, with vcall offset

    require!(s.len() >= 2);
    let original = *s;
    let token = s[..2];
    *s = &s[2..];
    match token {
        b"TV" => { ctx.out.push_str("vtable "); type_(ctx, s)?; }
        b"TT" => { ctx.out.push_str("vtt "); type_(ctx, s)?; }
        b"TI" => { ctx.out.push_str("typeinfo "); type_(ctx, s)?; }
        b"TS" => { ctx.out.push_str("typeinfo name "); type_(ctx, s)?; }
        b"Th" | b"Tv" => {
            *s = original[1..];
            ctx.out.push_str("virtual override thunk ");
            call_offset(ctx, s)?;
            ctx.out.push_str(", ");
            encoding(ctx, s)?;
        }
        b"Tc" => {
            ctx.out.push_str("virtual override thunk ");
            call_offset(ctx, s)?;
            ctx.out.push_str(", ");
            call_offset(ctx, s)?;
            ctx.out.push_str(", ");
            encoding(ctx, s)?;
        }
        b"GV" => { ctx.out.push_str("guard variable "); name(ctx, s)?; }
        b"GR" => {
            ctx.out.push_str("guard temporary ");
            name(ctx, s)?;
            if !s.starts_with(b"_") {
                let seq = seq_id(ctx, s)?;
                write!(ctx.out, " {}", seq);
            }
            require!(s.starts_with(b"_"));
            *s = &s[1..];
        }
        //etc
        _ => return_err!()
    }
}

fn name(ctx: &mut Ctx, s: &mut &[u8]) -> Result<()> {
    // <name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E        # nested-name
    //        ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E  # nested-name
    //        ::= <unscoped-name>
    //        ::= <unscoped-template-name> <template-args>
    //        ::= <local-name>
    // <prefix> ::= <unqualified-name>+
	//          ::= <template-prefix> <template-args>    # class template specialization
    //          ::= <template-prefix> <template-args> M  # <closure-prefix>, variable template, initializer of a variable or data member
    //          ::= [ <prefix> ] <variable or member unqualified-name> M  # <closure-prefix>, initializer of a variable or data member
    //          ::= <template-param>                   # template type parameter
    //          ::= <decltype>                         # decltype qualifier
	//          ::= <substitution>
    // <template-prefix> ::= <unqualified-name>           # global template
    //                   ::= <prefix> <unqualified-name>  # nested template
    //                   ::= <template-param>             # template template parameter
    //                   ::= <substitution>
    // <unqualified-name> ::= <operator-name> <abi-tag>*
    //                    ::= <ctor-dtor-name>  
    //                    ::= <source-name>   
    //                    ::= <unnamed-type-name>   
    //                    ::= DC <source-name>+ E      # structured binding declaration
    // <abi-tag> ::= B <source-name>
    // <ctor-dtor-name> ::= C1  # complete object constructor
	//                  ::= C2  # base object constructor
	//                  ::= C3  # complete object allocating constructor
	//                  ::= CI1 <base class type>  # complete object inheriting constructor
	//                  ::= CI2 <base class type>  # base object inheriting constructor
	//                  ::= D0  # deleting destructor
	//                  ::= D1  # complete object destructor
	//                  ::= D2  # base object destructor
    // <unnamed-type-name> ::= Ut [ <nonnegative number> ] _ 
    //                     ::= Ul <lambda-sig> E [ <nonnegative number> ] _
    // <lambda-sig> ::= <parameter type>+  # Parameter types or "v" if the lambda has no parameters
    // <unscoped-name> ::= <unqualified-name>
	//                 ::= St <unqualified-name>   # ::std::
    // <unscoped-template-name> ::= <unscoped-name>
	//                          ::= <substitution>
    // <local-name> ::= Z <function encoding> E <entity name> [<discriminator>]
    //              ::= Z <function encoding> E s [<discriminator>]
    //              ::= Z <function encoding> Ed [ <parameter number> ] _ <entity name>
    // <discriminator> ::= _ <non-negative number>      # when number < 10
    //                 ::= __ <non-negative number> _   # when number >= 10

    // <source-name> ::= <positive length number> <identifier>
    // <identifier> ::= <unqualified source code identifier>

    // <CV-qualifiers> ::= [r] [V] [K]  # restrict (C99), volatile, const
    // <ref-qualifier>      ::= (R | O)  # &, &&

    // <template-args> ::= I <template-arg>+ E
    // <template-arg> ::= <type>  # type or template
    //                ::= X <expression> E      # expression
    //                ::= <expr-primary>        # simple expressions
    //                ::= J <template-arg>* E   # argument pack

    // <template-param> ::= T [<parameter-2 non-negative number>] _

    // <template-template-param> ::= <template-param>
    //                           ::= <substitution>

    // <seq-id> ::= <0-9A-Z>+  $ base 36

    // <number> ::= [n] <non-negative decimal integer>  # 'n' is a minus sign

    // <bare-function-type> ::= <signature type>+  # types are possible return type, then parameter types

    // <type> ::= <builtin-type>
    //        ::= <qualified-type>
    //        ::= <function-type>
    //        ::= <class-enum-type>
    //        ::= <array-type>
    //        ::= <pointer-to-member-type>
    //        ::= <template-param>
    //        ::= <template-template-param> <template-args>
    //        ::= <decltype>
    //        ::= P <type>        # pointer
    //        ::= R <type>        # l-value reference
    //        ::= O <type>        # r-value reference (C++11)
    //        ::= C <type>        # complex pair (C99)
    //        ::= G <type>        # imaginary (C99)
    //        ::= <substitution>
    //        ::= Dp <type>       # pack expansion (C++11)

    // <decltype>  ::= Dt <expression> E  # decltype of an id-expression or class member access (C++11)
    //             ::= DT <expression> E  # decltype of an expression (C++11)

    // <substitution> ::= S <seq-id> _
	//                ::= S_
    //                ::= St  # ::std::
    //                ::= Sa  # ::std::allocator
    //                ::= Sb  # ::std::basic_string
    //                ::= Ss  # ::std::basic_string<char, ::std::char_traits<char>, ::std::allocator<char> >
    //                ::= Si  # ::std::basic_istream<char,  std::char_traits<char> >
    //                ::= So  # ::std::basic_ostream<char,  std::char_traits<char> >
    //                ::= Sd  # ::std::basic_iostream<char, std::char_traits<char> >

    // <operator-name> ::= nw  # new           
	//                 ::= na  # new[]
    //                 ::= dl  # delete        
    //                 ::= da  # delete[]      
    //                 ::= aw  # co_await      
    //                 ::= ps  # + (unary)
    //                 ::= ng  # - (unary)     
    //                 ::= ad  # & (unary)     
    //                 ::= de  # * (unary)     
    //                 ::= co  # ~             
    //                 ::= pl  # +             
    //                 ::= mi  # -             
    //                 ::= ml  # *             
    //                 ::= dv  # /             
    //                 ::= rm  # %             
    //                 ::= an  # &             
    //                 ::= or  # |             
    //                 ::= eo  # ^             
    //                 ::= aS  # =             
    //                 ::= pL  # +=            
    //                 ::= mI  # -=            
    //                 ::= mL  # *=            
    //                 ::= dV  # /=            
    //                 ::= rM  # %=            
    //                 ::= aN  # &=            
    //                 ::= oR  # |=            
    //                 ::= eO  # ^=            
    //                 ::= ls  # <<            
    //                 ::= rs  # >>            
    //                 ::= lS  # <<=           
    //                 ::= rS  # >>=           
    //                 ::= eq  # ==            
    //                 ::= ne  # !=            
    //                 ::= lt  # <             
    //                 ::= gt  # >             
    //                 ::= le  # <=            
    //                 ::= ge  # >=            
    //                 ::= ss  # <=>           
    //                 ::= nt  # !             
	//                 ::= aa  # &&            
	//                 ::= oo  # ||            
	//                 ::= pp  # ++ (postfix in <expression> context)
	//                 ::= mm  # -- (postfix in <expression> context)           
	//                 ::= cm  # ,             
	//                 ::= pm  # ->*           
	//                 ::= pt  # ->            
	//                 ::= cl  # ()            
	//                 ::= ix  # []            
	//                 ::= qu  # ?             
	//                 ::= cv <type>  # (cast)
    //                 ::= li <source-name>  # operator ""
	//                 ::= v <digit> <source-name>  # vendor extended operator
}

fn main() -> Result<()> {
    let mut ctx = Ctx {out: String::new()};
    let s = b"_Z2hi";
    let mut p = &s;
    mangled_name(&mut ctx, &mut p)?;
    println!("{}", ctx.out);
    Ok(())
}
*/
