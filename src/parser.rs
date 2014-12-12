#![allow(dead_code)]

use std::error::Error;

use {Ident, Value};
use lexer::{Lexer, Token};

#[deriving(Copy, PartialEq, Show)]
/// Possible errors encountered when parsing EDN.
pub enum ParserError {
    /// The parser reached the end of the input unexpectedly.
    Eof,
    /// The parser read an unexpected token
    UnexpectedToken {
        /// A human-readable description of the expected token
        expected: &'static str,
        /// A human-readable description of the token that was found
        found: &'static str
    },
    /// The parser found a mismatched pair of delimeters
    MismatchedDelim {
        /// The delimiter expected
        expected: &'static str,
        /// The delimiter found
        found: &'static str
    }
}

impl Error for ParserError {
    fn description(&self) -> &str { "EDN parser error" }
}

/// Convenient type alias for results of parsing.
pub type ParserResult = Result<Value, ParserError>;

pub struct Parser<T: Iterator<char>> {
    lex: Lexer<T>,
    tok: Option<Token>
}

impl<T: Iterator<char>> Parser<T> {
    pub fn new(inp: T) -> Parser<T> {
        let mut lexer = Lexer::new(inp);
        let tok = lexer.next();
        Parser {
            lex: lexer,
            tok: tok
        }
    }

    fn is_eof(&self) -> bool {
        self.tok.is_none()
    }

    /// Advance the parser one token.
    fn bump(&mut self) {
        self.tok = self.lex.next()
    }

    /// Advance the parser one token, returning the token on the stream before the bump.
    fn get_and_bump(&mut self) -> Option<Token> {
        let tok = self.tok.take();
        self.bump();
        tok
    }

    /// If the token `token` is available, consume it and return true.  Otherwise,
    /// returns false.
    fn eat(&mut self, token: &Token) -> bool {
        match self.tok {
            Some(ref tok) => token == tok,
            _ => false
        }
    }

    fn at_space(&self) -> bool {
        match self.tok {
            Some(Token::Space) => true,
            _ => false
        }
    }

    fn at_string(&self) -> bool {
        match self.tok {
            Some(Token::String(_)) => true,
            _ => false
        }
    }

    fn at_name(&self) -> bool {
        match self.tok {
            Some(Token::Name(_)) => true,
            _ => false
        }
    }

    fn at_keyword(&self) -> bool {
        match self.tok {
            Some(Token::Colon) => true,
            _ => false
        }
    }

    fn at_number(&self) -> bool {
        match self.tok {
            Some(Token::Float(_)) => true,
            Some(Token::Int(_)) => true,
            _ => false
        }
    }

    fn parse_string(&mut self) -> ParserResult {
        let s = self.get_and_bump().unwrap();
        match s {
            Token::String(s) => Ok(Value::String(s)),
            _ => panic!("logic error")
        }
    }

    fn parse_ident(&mut self) -> Result<Ident, ParserError> {
        let name = self.get_and_bump().unwrap();
        debug_assert!(name.is_name())

        if self.eat(&Token::Slash) {
            // prefixed ident
            let prefix = match name {
                Token::Name(n) => n,
                _ => panic!("logic error")
            };
            self.bump();
            let name = match self.get_and_bump() {
                Some(Token::Name(n)) => n,
                Some(Token::Slash) => "/".into_string(),
                Some(tok) => return Err(ParserError::UnexpectedToken {
                    expected: "identifier",
                    found: tok.human_readable()
                }),
                None => return Err(ParserError::Eof)
            };
            Ok(Ident::Prefixed { name: name, prefix: prefix })
        } else {
            Ok(match name {
                Token::Name(n) => Ident::Simple { name: n },
                _ => panic!("logic error")
            })
        }
    }

    fn parse_symbol(&mut self) -> ParserResult {
        let ident = try!(self.parse_ident());
        Ok(match ident {
            Ident::Simple { ref name } if *name == "true" => Value::Bool(true),
            Ident::Simple { ref name } if *name == "false" => Value::Bool(false),
            Ident::Simple { ref name } if *name == "nil" => Value::Nil,
            _ => Value::Symbol(ident)
        })
    }

    fn parse_keyword(&mut self) -> ParserResult {
        self.bump(); // skip past :

        if self.is_eof() {
            return Err(ParserError::Eof)
        }

        if self.at_name() {
            let ident = try!(self.parse_ident());
            Ok(Value::Keyword(ident))
        } else {
            Err(ParserError::UnexpectedToken {
                expected: "identifier",
                found: self.tok.take().unwrap().human_readable()
            })
        }
    }

    fn parse_number(&mut self) -> ParserResult {
        use std::str::FromStr;
        let num = self.get_and_bump().unwrap();

        match num {
            Token::Int(ref s) => {
                if s.as_slice().ends_with("N") {
                    panic!("arbitrary precision integers not yet implemented");
                }
                Ok(Value::Integer(FromStr::from_str(s.as_slice()).unwrap()))
            },
            Token::Float(ref s) => {
                if s.as_slice().ends_with("M") {
                    panic!("exact precision floats not yet implemented");
                }
                Ok(Value::Float(FromStr::from_str(s.as_slice()).unwrap()))
            },
            _ => panic!("logic error")
        }
    }

    fn at_vec(&self) -> bool {
        match self.tok {
            Some(Token::LSquare) => true,
            _ => false
        }
    }

    fn at_list(&self) -> bool {
        match self.tok {
            Some(Token::LParen) => true,
            _ => false
        }
    }

    fn parse_sequence(&mut self, close: &Token) -> Result<Vec<Value>, ParserError> {
        self.bump(); // advance past opening delim
        let mut vec = Vec::with_capacity(16);
        loop {
            self.consume_spaces();
            debug!("parse_sequence: {}", self.tok);
            match self.tok {
                Some(ref tok) if tok.is_closing_delim() => {
                    if tok == close {
                        break
                    } else {
                        return Err(ParserError::MismatchedDelim {
                            expected: close.human_readable(),
                            found: tok.human_readable()
                        })
                    }
                }
                _ => vec.push(try!(self.parse_value()))
            }
        }
        vec.shrink_to_fit();
        Ok(vec)
    }

    fn parse_vec(&mut self) -> ParserResult {
        let vec = try!(self.parse_sequence(&Token::RSquare));
        Ok(Value::Vector(vec))
    }

    fn parse_list(&mut self) -> ParserResult {
        let vec = try!(self.parse_sequence(&Token::RParen));
        Ok(Value::List(vec))
    }

    fn consume_spaces(&mut self) {
        while self.at_space() { self.bump() }
    }

    pub fn parse_value(&mut self) -> ParserResult {
        self.consume_spaces();
        if self.is_eof() {
            return Err(ParserError::Eof);
        }

        if self.at_name() {
            self.parse_symbol()
        } else if self.at_keyword() {
            self.parse_keyword()
        } else if self.at_string() {
            self.parse_string()
        } else if self.at_number() {
            self.parse_number()
        } else if self.eat(&Token::Slash) {
            self.bump();
            if self.at_space() || self.is_eof() {
                Ok(Value::Symbol(Ident::Simple { name: "/".into_string() }))
            } else {
                return Err(ParserError::UnexpectedToken {
                    expected: Token::Space.human_readable(),
                    found: self.tok.take().unwrap().human_readable()
                })
            }
        } else if self.at_vec() {
            self.parse_vec()
        } else if self.at_list() {
            self.parse_list()
        } else {
            panic!("unknown token at this state: {}", self.tok)
        }
    }
}

#[cfg(test)]
mod test {
    use {Ident, Value};
    use super::*;

    macro_rules! assert_val {
        ($str:expr, $val:expr) => ({
            let inp = $str.into_string();
            let mut parser = Parser::new(inp.chars());
            assert_eq!(parser.parse_value(), Ok($val))
        })
    }

    macro_rules! assert_err {
        ($str:expr) => ({
            let inp = $str.into_string();
            let mut parser = Parser::new(inp.chars());
            assert!(parser.parse_value().is_err())
        });
        ($str:expr, $pat:pat) => ({
            let inp = $str.into_string();
            let mut parser = Parser::new(inp.chars());
            match parser.parse_value() {
                Err($pat) => {},
                x => { panic!("error assert failed, parse_value({}) was {}",
                              stringify!($str), x) }
            }
        })
    }

    #[test]
    fn test_parse_eof() {
        assert_err!("", ParserError::Eof);
        assert_err!("    ", ParserError::Eof);
        assert_err!(",", ParserError::Eof);
    }

    #[test]
    fn test_parse_nil() {
        assert_val!("nil", Value::Nil);
        assert_val!("nil ", Value::Nil);
    }

    #[test]
    fn test_parse_bool() {
        assert_val!("true", Value::Bool(true));
        assert_val!("false", Value::Bool(false));
        assert_val!("false,", Value::Bool(false));
    }

    #[test]
    fn test_parse_str() {
        assert_err!(r#" "foo "#, ParserError::Eof);
        assert_val!(r#" "" "#, s(""));
        assert_val!(r#" "foo" "#, s("foo"));
        assert_val!(r#" "\n" "#, s("\n"));
        assert_val!(r#" "\"" "#, s("\""));
        assert_val!(" \"foo\nbar\" ", s("foo\nbar"));
    }

    #[test]
    fn test_parse_num() {
        assert_val!("0", i(0));
        assert_val!("-1", i(-1));
        assert_val!("1234", i(1234));
        assert_val!("1.234", float(1.234));
        assert_val!("1e23", float(1e23));
        assert_val!("-1e23", float(-1e23));
    }

    #[test]
    fn test_parse_symbol() {
        assert_val!("foo", sym_simple("foo"));
        assert_val!("foo#", sym_simple("foo#"));
        assert_val!("foo9", sym_simple("foo9"));
        assert_val!("-foo", sym_simple("-foo"));
        assert_val!("foo/bar", sym_prefixed("bar", "foo"));
        assert_val!("foo/true", sym_prefixed("true", "foo"));
        assert_val!("foo//", sym_prefixed("/", "foo"));
        assert_val!("/ foo", sym_simple("/"))

        assert_err!("foo/", ParserError::Eof);
        assert_err!("/foo", ParserError::UnexpectedToken {
            expected: "<whitespace>",
            ..
        });
        assert_err!(r#"foo/"bar""#, ParserError::UnexpectedToken {
            expected: "identifier",
            ..
        });

        // tools.reader.edn parses this as a symbol
        // assert_val!("ᛰ", Value::Symbol(Ident::simple("ᛰ")));
    }

    #[test]
    fn test_parse_keyword() {
        assert_err!(":", ParserError::Eof);
        assert_err!(": foo", ParserError::UnexpectedToken {
            expected: "identifier",
            ..
        });
        assert_val!(":foo", Value::Keyword(ident_simple("foo")));
        assert_val!(":-foo", Value::Keyword(ident_simple("-foo")));
        // assert_val!(":1234", Value::Keyword(ident_simple("1234")));
        // assert_val!(":12aaa", Value::Keyword(ident_simple("12aaa")));
    }

    #[test]
    fn test_parse_vector() {
        assert_val!("[]", Value::Vector(vec![]));
        assert_val!("[nil]", Value::Vector(vec![Value::Nil]));
        assert_val!("[5]", Value::Vector(vec![Value::Integer(5)]));
        assert_val!("[nil  ]", Value::Vector(vec![Value::Nil]));
        assert_val!("[  nil  ]", Value::Vector(vec![Value::Nil]));
        assert_val!(r#"[nil "hello"  ]"#,
                    Value::Vector(vec![Value::Nil, s("hello")]));
        assert_val!(r#"[nil, "hello"]"#,
                    Value::Vector(vec![Value::Nil, s("hello")]));
        assert_val!("[[]]", Value::Vector(vec![Value::Vector(vec![])]));
    }

    #[test]
    fn test_parse_list() {
        assert_val!("()", Value::List(vec![]));
        assert_val!("(nil)", Value::List(vec![Value::Nil]));
        assert_val!("((nil,))", Value::List(
                vec![Value::List(vec![Value::Nil])]
        ));
    }

    #[test]
    fn test_mismatched_delim() {
        assert_err!("[}", ParserError::MismatchedDelim {
            found: "`}`",
            expected: "`]`"
        });
        assert_err!("[[}]", ParserError::MismatchedDelim {
            found: "`}`",
            expected: "`]`"
        });
    }

    fn ident_simple(x: &'static str) -> Ident {
        Ident::Simple { name: x.into_string() }
    }

    fn ident_prefixed(x: &'static str, y: &'static str) -> Ident {
        Ident::Prefixed { name: x.into_string(), prefix: y.into_string() }
    }

    fn s(x: &'static str) -> Value {
        Value::String(x.into_string())
    }

    fn i(x: i64) -> Value {
        Value::Integer(x)
    }

    fn float(x: f64) -> Value {
        Value::Float(x)
    }

    fn sym_simple(x: &'static str) -> Value {
        Value::Symbol(ident_simple(x))
    }

    fn sym_prefixed(x: &'static str, y: &'static str) -> Value {
        Value::Symbol(ident_prefixed(x, y))
    }
}
