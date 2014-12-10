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

    pub fn human_readable(&self) -> &'static str {
        match *self {
            Token::LCurly => "`{`",
            Token::RCurly => "`}`",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::LSquare => "`[`",
            Token::RSquare => "`]`",
            Token::Slash => "`/`",
            Token::Space => "<whitespace>",
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

fn is_string_escape(ch: char) -> bool {
    ch == '"' || ch == 'n' || ch == 't' || ch == 'r'
}

fn is_letter(ch: char) -> bool {
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
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
    inp: T,
    // TODO: savagely optimize by building up tokens here
}

impl<T: Iterator<char>> Lexer<T> {
    pub fn new(mut inp: T) -> Lexer<T> {
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
            panic!("invalid token: {}", val)
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
        } else if is_delim(ch) {
            self.bump();
            Some(Token::delim(ch))
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
    fn test_sym() {
        // symbol-like token sequences
        assert_tokens!("foo", name("foo"));
        assert_tokens!("-foo", name("-foo"));
        assert_tokens!("foo/bar", name("foo"), Slash, name("bar"));
        assert_tokens!("foo / bar", name("foo"), Space, Slash, Space, name("bar"));
        assert_tokens!("+", name("+"));
        assert_tokens!("+abc", name("+abc"));
        assert_tokens!("<foo>", name("<foo>"));
        assert_tokens!(">>=", name(">>="));

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
