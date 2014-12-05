use std::error::Error;

use {Ident, Value};

#[deriving(PartialEq, Show)]
/// Possible errors encountered when parsing EDN.
pub enum ParserError {
    /// The parser reached the end of the input unexpectedly.
    Eof,
    /// The parser read an unknown string escape
    InvalidEscape,
    /// The parser found a token invalid for this position.
    InvalidToken(char)
}

impl Error for ParserError {
    fn description(&self) -> &str { "EDN parser error" }
}

pub type ParserResult = Result<Value, ParserError>;

pub struct Parser<T: Iterator<char>> {
    inp: T,
    ch: Option<char>
}

impl<T: Iterator<char>> Parser<T> {
    pub fn new(mut inp: T) -> Parser<T> {
        let ch = inp.next();
        Parser {
            inp: inp,
            ch: ch
        }
    }

    fn is_eof(&self) -> bool {
        self.ch.is_none()
    }

    fn bump(&mut self) {
        self.ch = self.inp.next()
    }

    fn consume_ws(&mut self) {
        loop {
            if self.is_eof() { break }
            let ch = self.ch.unwrap();
            // The EDN "spec" is very vague about what constitutes whitespace, but
            // tools.reader.edn uses Java's Character.isWhitespace method which has
            // identical semantics to Rust's char::is_whitespace method (*cough*
            // hopefully).
            // FIXME: verify that ^^^
            if ch.is_whitespace() || ch == ',' {
                self.bump();
            } else {
                break
            }
        }
    }

    fn at_symbol(&self) -> bool {
        if let Some(ch) = self.ch {
            is_ident_start_ch(ch)
        } else {
            false
        }
    }

    fn at_keyword(&self) -> bool {
        if let Some(':') = self.ch { true } else { false }
    }

    fn parse_ident(&mut self) -> Result<Ident, ParserError> {
        if self.is_eof() { return Err(ParserError::Eof) }

        let mut ident = String::with_capacity(16);
        let mut prefix = None;
        let ch = self.ch.unwrap();
        // It's important that parse_symbol respects the symbol starting character
        // restrictions itself, because parse_ident is also needed for parsing
        // keywords (which don't have those restrictions).
        if !is_ident_ch(ch) { return Err(ParserError::InvalidToken(ch)) }

        loop {
            if self.is_eof() { break; }
            let ch = self.ch.unwrap();
            if ch == '/' {
                prefix = Some(ident);
                ident = String::with_capacity(16);
                self.bump();
                continue;
            }
            if !is_ident_ch(ch) { break; }
            ident.push(self.ch.unwrap());
            self.bump();
        }
        if ident.len() == 0 { return Err(ParserError::InvalidToken('/')) }
        Ok(Ident {
            name: ident,
            prefix: prefix
        })
    }

    fn parse_string_escape(&mut self) -> Result<char, ParserError> {
        self.bump();
        if self.is_eof() { return Err(ParserError::Eof) }
        let ch = self.ch.unwrap();
        match ch {
            't' => Ok('\t'),
            'r' => Ok('\r'),
            'n' => Ok('\n'),
            '\\' => Ok('\\'),
            '"' => Ok('"'),
            _ => Err(ParserError::InvalidEscape)
        }
    }

    fn parse_string(&mut self) -> ParserResult {
        let mut out = String::with_capacity(32);
        loop {
            self.bump();
            if self.is_eof() { return Err(ParserError::Eof) }
            let ch = self.ch.unwrap();
            if ch == '"' { break }
            if ch == '\\' {
                out.push(try!(self.parse_string_escape()));
                continue;
            }
            out.push(self.ch.unwrap())
        }
        Ok(Value::String(out))
    }

    fn parse_symbol(&mut self) -> ParserResult {
        let ident = try!(self.parse_ident());
        if ident.prefix.is_none() {
            Ok(match &*ident.name {
                "nil" => Value::Nil,
                "true" => Value::Bool(true),
                "false" => Value::Bool(false),
                _ => Value::Symbol(ident)
            })
        } else {
            Ok(Value::Symbol(ident))
        }
    }

    fn parse_keyword(&mut self) -> ParserResult {
        self.bump();
        Ok(Value::Keyword(try!(self.parse_ident())))
    }

    pub fn parse_value(&mut self) -> ParserResult {
        self.consume_ws();

        if self.is_eof() { return Err(ParserError::Eof) }
        let ch = self.ch.unwrap();
        if ch == '"' { return self.parse_string() }
        if self.at_symbol() { return self.parse_symbol() }
        if self.at_keyword() { return self.parse_keyword() }

        unimplemented!()
    }
}

fn is_ident_start_ch(ch: char) -> bool {
    match ch {
        'a'...'z' | 'A'...'Z' => true,
        '*' | '!' | '_' | '?' | '$' | '%' | '&' | '=' | '<' | '>' => true,
        '-' | '+' | '.' => true, // can also start a number literal, watch out!
        _ => false
    }
}

fn is_ident_ch(ch: char) -> bool {
    is_ident_start_ch(ch) || ch == ':' || ch == '#' || (ch >= '0' && ch <= '9')
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
        ($str:expr, $val:expr) => ({
            let inp = $str.into_string();
            let mut parser = Parser::new(inp.chars());
            assert_eq!(parser.parse_value(), Err($val))
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
        assert_val!(r#" "" "#, Value::String("".into_string()));
        assert_val!(r#" "foo" "#, Value::String("foo".into_string()));
        assert_val!(r#" "\n" "#, Value::String("\n".into_string()));
        assert_val!(r#" "\"" "#, Value::String("\"".into_string()));
        // multi-line
        assert_val!(" \"foo\nbar\" ", Value::String("foo\nbar".into_string()));
    }

    #[test]
    fn test_parse_symbol() {
        assert_val!("foo", Value::Symbol(Ident::simple("foo")));
        assert_val!("foo#", Value::Symbol(Ident::simple("foo#")));
        assert_val!("foo9", Value::Symbol(Ident::simple("foo9")));
        assert_val!("-foo", Value::Symbol(Ident::simple("-foo")));
        assert_val!("foo/bar", Value::Symbol(Ident::prefixed("bar", "foo")));
        assert_err!("foo/", ParserError::InvalidToken('/'));
        // FIXME: the parser doesn't handle this correctly right now
        // assert_err!("/bar", ParserError::InvalidToken('/'));

        // tools.reader.edn parses this as a symbol
        // assert_val!("ᛰ", Value::Symbol(Ident::simple("ᛰ")));
    }

    #[test]
    fn test_parse_keyword() {
        assert_err!(":", ParserError::Eof);
        assert_err!(": foo", ParserError::InvalidToken(' '));
        assert_val!(":foo", Value::Keyword(Ident::simple("foo")));
        assert_val!(":-foo", Value::Keyword(Ident::simple("-foo")));
        assert_val!(":1234", Value::Keyword(Ident::simple("1234")));
        assert_val!(":12aaa", Value::Keyword(Ident::simple("12aaa")));
    }
}
