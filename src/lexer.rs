use std::iter::Peekable;

macro_rules! try_getch {
    ($ch:expr) => ({
        match $ch {
            None => return None,
            Some(ch) => ch
        }
    })
}

macro_rules! one_of {
    ($e:expr: $($var:expr),+) => {
        $($e == $var) ||+
    }
}

#[deriving(PartialEq, Show)]
pub enum Token {
    LCurly,
    RCurly,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Slash,
    Space,
    Colon,
    Float(String),
    Int(String),
    Name(String),
    String(String),
}

impl Token {
    fn delim(ch: char) -> Token {
        match ch {
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LSquare,
            ']' => Token::RSquare,
            _ => unreachable!()
        }
    }

    #[allow(dead_code)]
    pub fn is_name(&self) -> bool {
        match *self {
            Token::Name(_) => true,
            _ => false
        }
    }

    pub fn is_closing_delim(&self) -> bool {
        match *self {
            Token::RCurly | Token::RParen | Token::RSquare => true,
            _ => false
        }
    }

    pub fn human_readable(&self) -> &'static str {
        match *self {
            Token::LCurly => "`{`",
            Token::RCurly => "`}`",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::LSquare => "`[`",
            Token::RSquare => "`]`",
            Token::Slash => "`/`",
            Token::Colon => "`:`",
            Token::Space => "<whitespace>",
            Token::Float(_) => "float literal",
            Token::Int(_) => "integer literal",
            Token::Name(_) => "identifier",
            Token::String(_) => "string"
        }
    }
}

fn is_whitespace(ch: char) -> bool {
    // The EDN "spec" is very vague about what constitutes whitespace, but
    // tools.reader.edn uses Java's Character.isWhitespace method which has the same
    // behavior as Rust's char::is_whitespace method (*cough* hopefully).
    // FIXME: verify that ^^^
    ch.is_whitespace() || ch == ','
}

fn is_delim(ch: char) -> bool {
    one_of!(ch: '(', ')', '[', ']', '{', '}')
}

fn is_digit(ch: char) -> bool {
    ch >= '0' && ch <= '9'
}

fn is_num_start(ch: char) -> bool {
    ch == '+' || ch == '-' || is_digit(ch)
}

fn is_string_escape(ch: char) -> bool {
    ch == '"' || ch == 'n' || ch == 't' || ch == 'r'
}

fn is_letter(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}

fn is_alphanumeric(ch: char) -> bool {
    is_letter(ch) || is_digit(ch)
}

fn is_name_start1(ch: char) -> bool {
    (one_of!(ch: '!', '*', '?', '_', '$', '%', '&', '=', '<', '>') || is_letter(ch))
}

fn is_name_start2(ch: char) -> bool {
    one_of!(ch: '.', '-', '+')
}

fn is_name_constituent(ch: char) -> bool {
    is_name_start1(ch) || is_name_start2(ch) || is_digit(ch) || one_of!(ch: '#', ':')
}

/// Converts a stream of `char` values into a stream of terminal `Token` values.
pub struct Lexer<T: Iterator<char>> {
    ch: Option<char>,
    inp: Peekable<char, T>,
}

impl<T: Iterator<char>> Lexer<T> {
    pub fn new(inp: T) -> Lexer<T> {
        let mut inp = inp.peekable();
        Lexer {
            ch: inp.next(),
            inp: inp,
        }
    }

    fn bump(&mut self) {
        self.ch = self.inp.next();
    }

    fn get_and_bump(&mut self) -> char {
        let ch = self.ch.unwrap();
        self.bump();
        ch
    }

    fn peek(&mut self) -> Option<char> {
        self.inp.peek().map(|&x| x)
    }

    fn accept(&mut self, x: &str, buf: &mut String) -> bool {
        match self.ch {
            Some(ch) if x.contains_char(ch) => {
                buf.push(self.get_and_bump());
                true
            },
            _ => false
        }
    }

    fn at_whitespace(&self) -> bool {
        match self.ch {
            Some(ch) => is_whitespace(ch),
            _ => false
        }
    }

    fn skip_spaces(&mut self) {
        while self.at_whitespace() { self.bump() }
    }

    fn read_string(&mut self) -> Option<Token> {
        let mut val = String::with_capacity(16);
        let mut escape = false;
        self.bump(); // skip past the opening "
        loop {
            let ch = try_getch!(self.ch);
            if escape {
                if !is_string_escape(ch) {
                    panic!("invalid string escape sequence \\{}", ch)
                }
                let esc = match ch {
                    '"' => '"',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    _ => unreachable!()
                };
                val.push(esc);
                escape = false;
            } else {
                if ch == '\\' {
                    escape = true;
                } else if ch == '"' {
                    self.bump(); // skip past the closing "
                    break;
                } else {
                    val.push(ch);
                }
            }
            self.bump()
        }
        Some(Token::String(val))
    }

    fn read_number(&mut self) -> Option<Token> {
        const DIGITS: &'static str = "0123456789";

        // ugh
        let cur = self.ch.unwrap();
        if !is_digit(cur) {
            match self.peek() {
                None => return self.read_name2(),
                Some(ch) => {
                    if !is_digit(ch) && ch != '.' && ch != 'M' && ch != 'N' {
                        return self.read_name2()
                    }
                }
            }
        }

        let mut num = String::with_capacity(16);
        let mut is_float = false;

        // optional leading sign
        self.accept("+-", &mut num);
        if !self.accept("0", &mut num) {
            while self.accept(DIGITS, &mut num) {}
        }
        if self.accept(".", &mut num) {
            is_float = true;
            while self.accept(DIGITS, &mut num) {}
        }
        if self.accept("eE", &mut num) {
            self.accept("+-", &mut num);
            is_float = true;
            while self.accept(DIGITS, &mut num) {}
        }
        // optional arbitrary/exact precision
        if self.accept("M", &mut num) {
            is_float = true;
            // FIXME: exact precision
        } else if self.accept("N", &mut num) && !is_float {
            // FIXME: arbitrary precision
        }

        match self.ch {
            Some(ch) if is_alphanumeric(ch) => {
                panic!("uh oh (found alphanumeric at end of number literal)");
            }
            _ => if is_float {
                Some(Token::Float(num))
            } else {
                Some(Token::Int(num))
            }
        }

    }

    fn read_name1(&mut self) -> Option<Token> {
        let mut val = String::with_capacity(16);
        val.push(self.get_and_bump());

        loop {
            let ch = match self.ch {
                Some(ch) => ch,
                None => break
            };
            if is_name_constituent(ch) {
                val.push(ch);
                self.bump()
            } else {
                break
            }
        }
        Some(Token::Name(val))
    }

    fn read_name2(&mut self) -> Option<Token> {
        let mut val = String::with_capacity(16);

        val.push(self.get_and_bump());
        let ch = match self.ch {
            Some(ch) => ch,
            None => return Some(Token::Name(val))
        };

        let mut invalid = false;
        if is_letter(ch) {
            val.push(ch);
            self.bump()
        } else if is_name_constituent(ch) {
            // This is illegal following these characters, but keep munching
            invalid = true;
        } else {
            return Some(Token::Name(val))
        }

        loop {
            let ch = match self.ch {
                Some(ch) => ch,
                None => break
            };
            if is_name_constituent(ch) {
                val.push(ch);
                self.bump()
            } else {
                break
            }
        }

        if invalid {
            panic!("invalid token: {}", val) // FIXME
        } else {
            Some(Token::Name(val))
        }
    }
}

impl<T: Iterator<char>> Iterator<Token> for Lexer<T> {
    fn next(&mut self) -> Option<Token> {
        let ch = try_getch!(self.ch);

        if is_whitespace(ch) {
            self.skip_spaces();
            Some(Token::Space)
        } else if ch == ':' {
            self.bump();
            Some(Token::Colon)
        } else if is_delim(ch) {
            self.bump();
            Some(Token::delim(ch))
        } else if is_num_start(ch) {
            self.read_number()
        } else if is_name_start1(ch) {
            self.read_name1()
        } else if is_name_start2(ch) {
            self.read_name2()
        } else if ch == '"' {
            self.read_string()
        } else if ch == '/' {
            self.bump();
            Some(Token::Slash)
        } else {
            panic!()
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, Token};
    use super::Token::*;

    macro_rules! lex {
        ($str:expr) => ({
            let inp = $str.into_string();
            let mut lexer = Lexer::new(inp.chars());
            for _ in lexer {
            }
        })
    }

    macro_rules! assert_eof {
        ($str:expr) => ({
            let inp = $str.into_string();
            let mut lexer = Lexer::new(inp.chars());
            assert_eq!(lexer.next(), None)
        })
    }

    macro_rules! assert_tokens {
        ($str:expr, $($token:expr),+) => ({
            let inp = $str.into_string();
            let mut lexer = Lexer::new(inp.chars());
            $(assert_eq!(lexer.next(), Some($token)));*
            assert!(lexer.next().is_none())
        })
    }

    fn s(x: &'static str) -> Token {
        Token::String(x.into_string())
    }

    fn name(x: &'static str) -> Token {
        Token::Name(x.into_string())
    }

    fn float(x: &'static str) -> Token {
        Token::Float(x.into_string())
    }

    fn i(x: &'static str) -> Token {
        Token::Int(x.into_string())
    }

    #[test]
    fn test_eof() {
        assert_eof!("");
    }

    #[test]
    fn test_delim() {
        assert_tokens!("{}", LCurly, RCurly);
        assert_tokens!("[]", LSquare, RSquare);
        assert_tokens!("()", LParen, RParen);
    }

    #[test]
    fn test_space() {
        assert_tokens!("    { } ", Space, LCurly, Space, RCurly, Space);
    }

    #[test]
    fn test_str() {
        assert_tokens!(r#""foo""#, s("foo"));
        assert_tokens!(r#""foo\"bar""#, s("foo\"bar"));
        assert_tokens!(r#""foo\nbar""#, s("foo\nbar"));
        assert_tokens!(r#""foo\tbar""#, s("foo\tbar"));
        assert_tokens!(r#""foo\rbar""#, s("foo\rbar"));
    }

    #[test]
    fn test_num() {
        assert_tokens!("+123", i("+123"));
        assert_tokens!("-123", i("-123"));
        assert_tokens!("123", i("123"));
        assert_tokens!("123N", i("123N"));
        // assert_tokens!("0123", num("0123")); // illegal FIXME: reenable with err
        assert_tokens!("{12 34}", LCurly, i("12"), Space, i("34"), RCurly);
        assert_tokens!("(+ 34)", LParen, name("+"), Space, i("34"), RParen);
        assert_tokens!("1.2", float("1.2"));
        assert_tokens!("1.2e34", float("1.2e34"));
        assert_tokens!("1.2E34", float("1.2E34"));
        assert_tokens!("1.2e-10", float("1.2e-10"));
        assert_tokens!("1.2e-10M", float("1.2e-10M"));
        assert_tokens!("1.2M", float("1.2M"));
        assert_tokens!("0M", float("0M"));
    }

    #[test]
    fn test_sym() {
        // symbol-like token sequences
        assert_tokens!("foo", name("foo"));
        assert_tokens!("-f#o:o", name("-f#o:o"));
        assert_tokens!("foo/bar", name("foo"), Slash, name("bar"));
        assert_tokens!("foo / bar", name("foo"), Space, Slash, Space, name("bar"));
        assert_tokens!("+", name("+"));
        assert_tokens!("+abc", name("+abc"));
        assert_tokens!("<foo>", name("<foo>"));
        assert_tokens!(">>=", name(">>="));
        assert_tokens!("NaN", name("NaN"));

        // vvv should be rejected by parser
        assert_tokens!("foo/ bar", name("foo"), Slash, Space, name("bar"));
    }

    #[test]
    fn test_name2() {
        // cases for names starting with -|+|.
        // FIXME: re-enable
        // assert_tokens!("-_", invalid("-_"))
        // assert_tokens!("-_foobar{", invalid("-_foobar"), LCurly)
        assert_tokens!("+{", name("+"), LCurly)
    }

    #[test]
    fn test_slash() {
        assert_tokens!("/", Slash);
        assert_tokens!("/ {", Slash, Space, LCurly);
        assert_tokens!("/{", Slash, LCurly);
    }
}
