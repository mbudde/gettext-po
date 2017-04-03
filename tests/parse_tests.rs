extern crate gettext_po;

use std::fs;
use std::io::Read;
use std::path::PathBuf;

fn po_files_in_dir(path: &str) -> Box<Iterator<Item=(PathBuf, Vec<u8>)>> {
    Box::new(fs::read_dir(path)
        .unwrap()
        .filter_map(|entry| {
            entry.ok().into_iter()
                .filter(|e| e.file_type().map(|ft| ft.is_file()).unwrap_or(false))
                .filter(|e| e.path().extension().map(|e| e == "po").unwrap_or(false))
                .next()
        })
        .map(|entry| {
            let path = entry.path();
            let mut content = Vec::new();
            fs::File::open(&path).unwrap().read_to_end(&mut content).unwrap();
            (path, content)
        }))
}

#[test]
fn parse_pass() {
    for (path, content) in po_files_in_dir("tests/parse-pass") {
        println!("Parsing expecting success: {:?}", path);
        let res = gettext_po::parse(&content[..]);
        println!("{:?}", res);
        assert!(res.is_ok());
    }
}

#[test]
fn parse_fail() {
    for (path, content) in po_files_in_dir("tests/parse-fail") {
        println!("Parsing expecting error: {:?}", path);
        let res = gettext_po::parse(&content[..]);
        println!("{:?}", res);
        assert!(res.is_err());
    }
}
