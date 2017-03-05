extern crate gettext_po;
extern crate combine;

use std::io::{Read};


fn main() {
    let mut content = Vec::new();
    std::io::stdin().read_to_end(&mut content).unwrap();

    let result = gettext_po::entries(&content[..]);

    match result {
        Ok((entries, _)) => {
            for entry in entries {
                print!("{}", entry);
            }
        }
        Err(err) => {
            println!("{:?}", err.into_inner());
        }
    }

}
