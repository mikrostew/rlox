pub trait Reporter {
    fn report(&self, message: &str, line_num: u64, column: u64, _length: u64) -> String;
}

pub struct BasicReporter {}

impl BasicReporter {
    pub fn new() -> Self {
        BasicReporter {}
    }
}

impl Reporter for BasicReporter {
    fn report(&self, message: &str, line_num: u64, column: u64, _length: u64) -> String {
        let error_report = format!("Error: {} [line {}:{}]", message, line_num, column);
        eprintln!("{}", error_report);
        error_report
    }
}

// TODO: better error reporting
// the book doesn't implement it, but given those args it should be possible to do this:
//
// Error: Unexpected "," in argument list.
//
//     15:23 | function(first, second,);
//                                   ^-- here
//
//  OR, like cargo does:
//
// error[E0615]: attempted to take value of method `had_error` on type `&Lox`
//   --> src/main.rs:99:14
//    |
// 99 |         self.had_error
//    |              ^^^^^^^^^ help: use parentheses to call the method: `had_error()`
//
// fn error(&mut self, message: &str, line_num: u64, column: u64, _length: u64) {
//     println!("[line {}:{}] Error: {}", line_num, column, message);
// }
