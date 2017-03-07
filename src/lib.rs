#![allow(unused_imports)]

#[macro_use]
extern crate combine;
use std::fmt::{self, Display};
use std::str::from_utf8;
use combine::parser;
use combine::combinator::*;
use combine::byte::*;
use combine::primitives::{Stream, ParseResult, Parser};

#[derive(Debug, PartialEq)]
pub enum Translation {
    Singular {
        msgid: String,
        msgstr: String,
    },
    Plural {
        msgid: String,
        msgid_plural: String,
        msgstr: Vec<(u32, String)>,
    }
}

#[derive(Debug, PartialEq)]
pub enum Comment {
    Translator(String),
    Extracted(String),
    References(Vec<String>),
    Flags(Vec<String>),
}

impl Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Comment::Translator(ref s) => write!(f, "# {}", s),
            Comment::Extracted(ref s)  => write!(f, "#. {}", s),
            Comment::References(ref s) => write!(f, "#: {}", s.join(" ")),
            Comment::Flags(ref s)      => write!(f, "#, {}", s.join(" ")),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Entry {
    pub msgctxt: Option<String>,
    pub translation: Translation,
    pub comments: Vec<Comment>,
    pub obsolete: bool,
}

fn write_multiline_str(f: &mut fmt::Formatter, s: &str, obsolete: bool) -> Result<(), fmt::Error> {
    if s.contains('\n') {
        write!(f, "\"\"\n")?;
        for line in s.split('\n') {
            if obsolete {
                write!(f, "#~ ")?;
            }
            write!(f, "{:?}\n", line)?;
        }
    } else {
        write!(f, "{:?}\n", s)?;
    }
    Ok(())
}

macro_rules! write_obsolete {
    ( $x:ident, $f:ident, $( $arg:expr ),* ) => {
        {
            if $x.obsolete {
                write!($f, "#~ ")?;
            }
            write!($f, $($arg),*)
        }
    }
}

impl Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for comment in &self.comments {
            write!(f, "{}\n", comment)?;
        }
        if let Some(ref msgctxt) = self.msgctxt {
            write_obsolete!(self, f, "msgctxt ")?;
            write_multiline_str(f, msgctxt, self.obsolete)?;
        }
        match self.translation {
            Translation::Singular { ref msgid, ref msgstr } => {
                write_obsolete!(self, f, "msgid ")?;
                write_multiline_str(f, msgid, self.obsolete)?;
                write_obsolete!(self, f, "msgstr ")?;
                write_multiline_str(f, msgstr, self.obsolete)
            }
            Translation::Plural { ref msgid, ref msgid_plural, ref msgstr } => {
                write_obsolete!(self, f, "msgid ")?;
                write_multiline_str(f, msgid, self.obsolete)?;
                write_obsolete!(self, f, "msgid_plural ")?;
                write_multiline_str(f, msgid_plural, self.obsolete)?;
                for &(i, ref msg) in msgstr {
                    write_obsolete!(self, f, "msgstr[{}] ", i)?;
                    write_multiline_str(f, msg, self.obsolete)?;
                }
                Ok(())
            }
        }
    }
}

/// ```bnf
/// quoted_str = '"' str_char '"'
/// str_char   = escape | char
/// escape     = "\n" | '\"' | "\\"
/// ```
fn quoted_str<I>(input: I) -> ParseResult<String, I>
    where I: Stream<Item=u8>
{
    const ESCAPES: &'static str = "n\"\\";

    let escape = token(b'\\')
        .with(one_of(ESCAPES.bytes()))
        .map(|c| match c {
            b'n' => b'\n',
            c => c,
        });

    let mut quoted_str = between(
        token(b'"'), token(b'"'),
        many(
            choice!(
                escape,
                none_of("\"".bytes())
            )
        ).and_then(|bytes: Vec<u8>| String::from_utf8(bytes))
    );
    quoted_str.parse_stream(input)
}

/// ```bnf
/// multiline_str = quoted_str *( newlquoted_str )
/// ```
fn multiline_str<I>(input: I) -> ParseResult<String, I>
    where I: Stream<Item=u8>
{
    sep_by1(parser(quoted_str), try((newline(), spaces(), look_ahead(token(b'"')))))
        .parse_stream(input)
}

fn comments<'a, I>(input: I) -> ParseResult<Vec<Comment>, I>
    where I: Stream<Item=u8, Range=&'a [u8]>
{
    let ws = || satisfy(|b| b == b' ' || b == b'\t');
    let many_ws = || skip_many(ws());

    let rest_of_line = || {
        many(satisfy(|b| b != b'\n'))
            .and_then(|bytes| String::from_utf8(bytes))
    };
    let word = || {
        many1(satisfy(|b| b != b' ' && b != b'\t' && b != b'\n'))
            .and_then(String::from_utf8)
    };
    let flags = token(b',')
        .skip(many_ws())
        .with(many(word().skip(many_ws())));
    let references = token(b':')
        .skip(many_ws())
        .with(many(word().skip(many_ws())));
    let translator = optional(token(b'.'))
        .and(optional(token(b' ')).with(rest_of_line()))
        .map(|(extracted, s)| {
            if extracted.is_some() {
                Comment::Extracted(s)
            } else {
                Comment::Translator(s)
            }
        });
    let comment = try(optional(try(bytes(b"#~")).skip(many_ws())).and(token(b'#')))
        .with(
            choice! {
                flags.map(Comment::Flags),
                references.map(Comment::References),
                translator
            }
        )
        .skip(newline().skip(spaces()));

    many(comment)
        .map(|mut comments: Vec<Comment>| {
            comments.sort_by_key(|c| {
                match *c {
                    Comment::Translator(_) => 1,
                    Comment::Extracted(_)  => 2,
                    Comment::References(_) => 3,
                    Comment::Flags(_)      => 4,
                }
            });
            comments
        })
        .parse_stream(input)
}

fn obsolete<'a, I>(input: I) -> ParseResult<bool, I>
    where I: Stream<Item=u8, Range=&'a [u8]>
{
    optional(
        try(bytes(b"#~")
            .skip(skip_many(satisfy(|b| b == b' ' || b == b'\t'))))
        )
        .map(|opt| opt.is_some())
        .parse_stream(input)
}

fn translation<'a, I>(input: I) -> ParseResult<(bool, Translation), I>
    where I: Stream<Item=u8, Range=&'a [u8]>
{
    let section = |keyword| {
        parser(obsolete)
            .and(keyword)
            .skip(skip_many1(space()))
            .and(parser(multiline_str))
    };

    let msgid = section(bytes(b"msgid")).skip(newline());

    msgid.then(move |((obs, _), msgid)| {
        let msgid_plural = try(parser(obsolete).and(bytes(b"msgid_plural")))
            .skip(skip_many1(space()))
            .and(parser(multiline_str))
            .skip(newline());

        let msgstr_plural = parser(obsolete)
            .and(
                bytes(b"msgstr")
                    .with(
                        between(
                            token(b'[') , token(b']'),
                            many1(digit())
                                .and_then(|s| String::from_utf8(s))
                                .and_then(|s| s.parse())))
            )
            .skip(skip_many1(space()))
            .and(parser(multiline_str))
            .skip(newline())
            .map(|((_, n), s)| (n, s));

        let msgstr = section(bytes(b"msgstr"));

        let msgid2 = msgid.clone();
        msgid_plural.and(many1(msgstr_plural))
            .map(move |(msgid_plural, msgstr_plurals)| {
                Translation::Plural {
                    msgid: msgid.clone(),
                    msgid_plural: msgid_plural.1,
                    msgstr: msgstr_plurals,
                }
            })
            .or(
                msgstr.map(move |(_, msgstr)| {
                    Translation::Singular {
                        msgid: msgid2.clone(),
                        msgstr: msgstr,
                    }
                })
            )
            .map(move |t| (obs, t))
    }).parse_stream(input)

    // (msgid, spaces(), msgstr)
        // .map(|(((obs, _), msgid), _, ((obs2, _), msgstr))| {
            // (obs || obs2, Translation::Singular { msgid: msgid, msgstr: msgstr })
        // })
        // .parse_stream(input)
}

pub fn entry<'a, I>(input: I) -> ParseResult<Entry, I>
    where I: Stream<Item=u8, Range=&'a [u8]>
{
    let msgctxt = try(parser(obsolete).skip(try(bytes(b"msgctxt"))))
        .skip(skip_many1(space()))
        .with(parser(multiline_str))
        .skip(newline());

    (spaces(), parser(comments), optional(msgctxt), parser(translation))
        .map(|(_, comments, msgctxt, (obs, trans))| {
            Entry {
                translation: trans,
                msgctxt: msgctxt,
                comments: comments,
                obsolete: obs,
            }
        })
        .parse_stream(input)
}

pub fn entries<'a, I>(input: I) -> ParseResult<Vec<Entry>, I>
    where I: Stream<Item=u8, Range=&'a [u8]>
{
    sep_end_by(parser(entry), newline().and(spaces()))
        .skip(eof())
        .parse_stream(input)
}


#[cfg(test)]
mod tests {
    use super::*;
    use combine::parser;

    #[test]
    fn test_multiline_str() {
        let res = parser(multiline_str).parse("\"\"".as_bytes());
        assert_eq!(res, Ok(("".to_string(), &[][..])));

        let res = parser(multiline_str).parse("\"\"\n    \"\"".as_bytes());
        assert_eq!(res, Ok(("".to_string(), &[][..])));

        let res = parser(multiline_str)
            .parse("\"foo\"\n\
                    \"bar\"".as_bytes());
        assert_eq!(res, Ok(("foobar".to_string(), &[][..])));

        let res = parser(multiline_str)
            .parse("\"foo\\n\"\n\
                   \"bar\"".as_bytes());
        assert_eq!(res, Ok(("foo\nbar".to_string(), &[][..])));
    }

    #[test]
    fn escapes() {
        let res = parser(entry)
            .parse("msgid   \"fo\\\"ob\\nar\"\n\
                    msgstr \"\"".as_bytes());
        let exp = Entry {
            translation: Translation::Singular { msgid: "fo\"ob\nar".to_string(), msgstr: "".to_string() },
            comments: vec![],
            obsolete: false,
        };
        assert_eq!(res, Ok((exp, &[][..])));
    }

    #[test]
    fn test_entry() {
        let res = parser(entry)
            .parse("msgid \"\"\n\
                   \"foo\"\n\
                   \"bar\"\n\
                   msgstr \"\"".as_bytes());
        let exp = Entry {
            translation: Translation::Singular { msgid: "foobar".to_string(), msgstr: "".to_string() },
            comments: vec![],
            obsolete: false,
        };
        assert_eq!(res, Ok((exp, &[][..])));
    }

    #[test]
    fn test_comments() {
        let res = parser(entry)
            .parse("\
            #a comment\n\
            #  another comment   \n\
            #.\textracted comment\n\
            #:    ref1   ref2    \n\
            #,    flag1\tflag2   \n\
            msgid \"\"\n\
            msgstr \"\"".as_bytes());
        let exp = Entry {
            translation: Translation::Singular { msgid: "".to_string(), msgstr: "".to_string() },
            obsolete: false,
            comments: vec![
                Comment::Translator("a comment".to_string()),
                Comment::Translator(" another comment   ".to_string()),
                Comment::Extracted("\textracted comment".to_string()),
                Comment::References(vec!["ref1".to_string(), "ref2".to_string()]),
                Comment::Flags(vec!["flag1".to_string(), "flag2".to_string()]),
            ],
        };
        assert_eq!(res, Ok((exp, &[][..])));
    }

    #[test]
    fn test_obsolete() {
        let res = parser(entry)
            .parse("\
            #~ # a\n\
            #~# b\n\
            # c\n\
            #~ msgid \"\"\n\
            #~ msgstr \"\"".as_bytes());
        let exp = Entry {
            translation: Translation::Singular { msgid: "".to_string(), msgstr: "".to_string() },
            obsolete: true,
            comments: vec![
                Comment::Translator("a".to_string()),
                Comment::Translator("b".to_string()),
                Comment::Translator("c".to_string()),
            ],
        };
        assert_eq!(res, Ok((exp, &[][..])));
    }
}
