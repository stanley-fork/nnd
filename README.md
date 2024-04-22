A debugger. Currently in a closed pre-alpha. See doc.txt for very rough documentation.

The executable is available at https://www.dropbox.com/scl/fo/gucqoo89tf5v3qzz57rwg/AMahQWfSveymlUB3SiCg9K8?rlkey=qo3hlslw2hvm7ezdj39cvyr8r

Build:
```
rustup target add x86_64-unknown-linux-musl  # run this once to install musl
cargo build -r --bin nnd
# the executable is at target/x86_64-unknown-linux-musl/release/nnd
```
