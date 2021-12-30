extern crate rust_toys_lang;

use std::fs::File;
use std::io::prelude::*;

use rust_toys_lang::interpreter::*;
use rust_toys_lang::parser::*;

#[cfg(test)]
mod tests {
    use super::*;

    fn run(filename: &str) -> i32 {
        let mut f = File::open(filename).expect("file not found");
        let mut source = String::new();
        f.read_to_string(&mut source)
            .expect("error while reading the file");
        let top_levels = parse(source + "__EOF__").unwrap();
        let mut input = Vec::new();
        for top in &top_levels {
            input.push(top);
        }
        let mut interpreter = Interpreter::new();
        interpreter.call_main(input)
    }

    #[test]
    fn test_factorial() {
        let ret = run("./tests/factorial.toys");
        assert_eq!(120, ret);
    }

    #[test]
    fn test_for_in() {
        let ret = run("./tests/for-in.toys");
        assert_eq!(45, ret);
    }
}
