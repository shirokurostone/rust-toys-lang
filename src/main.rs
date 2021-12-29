pub mod interpreter;
pub mod parser;

use std::env;
use std::fs::File;
use std::io::prelude::*;

use crate::interpreter::*;
use crate::parser::*;

fn main() {
    let args: Vec<String> = env::args().collect();

    let filename = &args[1];

    let mut f = File::open(filename).expect("file not found");
    let mut source = String::new();

    f.read_to_string(&mut source)
        .expect("error while reading the file");

    let result = parse(source.as_bytes());
    match result {
        Ok((_, top_levels)) => {
            let mut input = Vec::new();
            for top in &top_levels {
                input.push(top);
            }
            let mut interpreter = Interpreter::new();
            println!("{:?}", input);
            let ret = interpreter.call_main(input);
            println!("{}", ret);
        }
        Err(e) => {
            panic!("{}", e);
        }
    }
}
