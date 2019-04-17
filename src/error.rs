use crate::token::Position;

pub trait Reporter {
    fn report(&self, error_msg: &str, positional_msg: &str, pos: &Position) -> String;
}

// pub struct BasicReporter {}

// impl BasicReporter {
//     pub fn new() -> Self {
//         BasicReporter {}
//     }
// }

// impl Reporter for BasicReporter {
//     fn report(&self, message: &str, pos: &Position, _length: u64) -> String {
//         let error_report = format!("Error: {} [line {}:{}]", message, pos.line, pos.column);
//         eprintln!("{}", error_report);
//         error_report
//     }
// }

pub struct BetterReporter {
    source: String,
}

impl BetterReporter {
    pub fn new(source: String) -> Self {
        BetterReporter {
            source: source.trim().to_string(),
        }
    }
}

// report an error similar to cargo's format:
//
// error: attempted to take value of method `had_error` on type `&Lox`
//   --> somefile.lox:99:14
//    |
// 99 |         self.had_error
//    |              ^^^^^^^^^ add `()`?
//
impl Reporter for BetterReporter {
    fn report(&self, error_msg: &str, positional_msg: &str, pos: &Position) -> String {
        let file = match &pos.file {
            Some(f) => f.clone(),
            None => "(input)".to_string(),
        };
        let length = pos.length;
        let line = pos.line;
        let mut col = pos.column;

        // TODO: to prevent accidental subtraction panic later on
        if col <= 0 {
            col = 1;
        }

        eprintln!("error: {}", error_msg);
        eprintln!("  --> {}:{}:{}", file, line, col);

        // TODO: figure out the number of digits in the line number
        // (for now, just assume max of 3 digits)
        // TODO: print a line other than the first one from source
        eprintln!("    |");
        eprintln!("{:>3} |  {}", line, self.source);
        eprintln!(
            "    |  {:3$}{:^<4$} {}",
            "",
            "",
            positional_msg,
            (col - 1) as usize,
            length as usize
        );
        eprintln!("");

        error_msg.to_string()
    }
}
