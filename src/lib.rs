#![feature(globs, macro_rules, phase)]
// #![deny(dead_code)]
// #![warn(missing_docs)]

//! A native Rust library for working with
//! [EDN (Extensible Data Notation)](https://github.com/edn-format/edn).

#[phase(plugin, link)] extern crate log;
extern crate serialize;

// pub use parser::{ParserError, ParserResult};

mod lexer;
mod parser;

/// The representation of an EDN identifier: part of a symbol or keyword.  Always has a
/// name, but may have a prefix (typically a namespace) if written as `prefix/name`.
///
// FIXME: re-enable this
/// ```norun
/// use edn::Ident;
/// let val = edn::parse_str("foo").unwrap();
/// let simple_ident = val.as_symbol().unwrap();
/// assert_eq!(simple_ident, Ident::Simple { name: "foo".into_string() })
/// ```
#[deriving(PartialEq, Show)]
pub enum Ident {
    /// A simple identifier, like `foo`
    Simple { name: String },
    /// A prefixed or namespaced identifier, like `foo`
    Prefixed { name: String, prefix: String }
}

/// An EDN value.
#[deriving(PartialEq, Show)]
pub enum Value {
    /// Represents nil, null, or nothing.
    Nil,
    /// `true` or `false`
    Bool(bool),
    /// A string of characters.
    String(String),
    /// A Unicode codepoint.
    Char(char),
    /// Used as identifiers.
    Symbol(Ident),
    /// Identifiers which designate themselves.
    Keyword(Ident),
    /// A 64-bit signed integer.
    Integer(i64),
    // FIXME: need to support arbitrary precision integers
    /// A 64-bit floating point number.
    Float(f64),
    // FIXME: need to support "exact precision"
    /// A sequence of values.
    List(EdnList),
    /// A sequence of values with cheap random access.
    Vector(EdnVec),
    // FIXME: need to support maps
    // FIXME: need to support sets
    // FIXME: need to support tagged values
}

impl Value {
    // FIXME: Need these for the other Value variants.

    /// If the `Value` is a `Keyword` or `Symbol`, returns a reference to its `Ident`.
    pub fn as_ident(&self) -> Option<&Ident> {
        self.as_keyword().or_else(|| self.as_symbol())
    }

    /// If the `Value` is a `Keyword`, returns a reference to its `Ident`.
    pub fn as_keyword(&self) -> Option<&Ident> {
        match *self {
            Value::Keyword(ref id) => Some(id),
            _ => None
        }
    }

    /// If the `Value` is a `Symbol`, returns a reference to its `Ident`.
    pub fn as_symbol(&self) -> Option<&Ident> {
        match *self {
            Value::Symbol(ref id) => Some(id),
            _ => None
        }
    }
}

/// Rust representation of an EDN list.
pub type EdnList = Vec<Value>;
/// Rust representation of an EDN vector.
pub type EdnVec = Vec<Value>;

/*
/// Parses an input `&str` and returns the result as a single EDN value or any parser
/// errors encountered.
pub fn parse_str(input: &str) -> ParserResult {
    parse_chars(input.chars())
}

/// Given an `Iterator` of `char` values, parses a single EDN value from it and returns
/// the value or any parser errors encountered.
pub fn parse_chars<T: Iterator<char>>(input: T) -> ParserResult {
    let mut parser = parser::Parser::new(input);
    parser.parse_value()
}

*/
