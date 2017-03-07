extern crate gettext_po;
extern crate combine;

use combine::primitives::State;
use std::io::{Read};


fn main() {
    let mut content = Vec::new();
    std::io::stdin().read_to_end(&mut content).unwrap();

    let result = gettext_po::entries(State::new(&content[..]));

    match result {
        Ok((mut entries, _)) => {
            entries.sort_by_key(|e| e.obsolete as u8);
            let mut first = true;
            for entry in entries {
                if !first {
                    println!("");
                }
                first = false;
                print!("{}", entry);
            }
        }
        Err(err) => {
            println!("{:?}", err.into_inner());
        }
    }

}
