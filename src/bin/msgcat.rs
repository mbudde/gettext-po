extern crate gettext_po;
extern crate combine;

use std::io::Read;

fn main() {
    let mut content = Vec::new();
    std::io::stdin().read_to_end(&mut content).unwrap();

    let result = gettext_po::parse(&content[..]);

    match result {
        Ok(mut entries) => {
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
            println!("{}", err);
        }
    }

}
