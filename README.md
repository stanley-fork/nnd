A debugger. Currently in a closed pre-alpha. See --help for documentation.

Distributed as a single executable file (statically linked), available at https://al13n.itch.io/nnd
Or: `curl 'https://www.dropbox.com/scl/fi/tc2fnxjt7ksi8n5qq9602/nnd?rlkey=ovplk7w1e0rtrjgxodu8u3c59&dl=1' -o nnd -L && chmod +x nnd`

Build:
```
rustup target add x86_64-unknown-linux-musl  # run this once to install musl
cargo build --profile dbgo --bin nnd
# the executable is at target/x86_64-unknown-linux-musl/dbgo/nnd
```
