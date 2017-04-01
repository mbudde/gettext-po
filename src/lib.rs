#[macro_use] extern crate combine;

use std::fmt::{self, Display, Write};

use combine::State;

mod parser;

#[derive(Debug, PartialEq, Eq)]
pub struct Message {
    pub obsolete: bool,
    pub comments: Vec<Comment>,
    pub msgctxt: Option<String>,
    pub msgid: String,
    pub translation: Translation,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Translation {
    Singular {
        msgstr: String,
    },
    Plural {
        msgid_plural: String,
        msgstr: Vec<(u32, String)>,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Comment {
    Translator(String),
    Extracted(String),
    References(Vec<String>),
    Flags(Vec<String>),
}

impl Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Comment::Translator(ref s) => {
                if s.is_empty() { write!(f, "#") } else { write!(f, "# {}", s) }
            }
            Comment::Extracted(ref s)  => write!(f, "#. {}", s),
            Comment::References(ref s) => write!(f, "#: {}", s.join(" ")),
            Comment::Flags(ref s)      => write!(f, "#, {}", s.join(" ")),
        }
    }
}

fn write_quoted_str(f: &mut fmt::Formatter, s: &str) -> Result<(), fmt::Error> {
    f.write_char('"')?;
    let mut from = 0;
    for (i, c) in s.char_indices() {
        let esc = match c {
            '"' => Some("\\\""),
            '\n' => Some("\\n"),
            '\\' => Some("\\\\"),
            _ => None,
        };
        if let Some(esc) = esc {
            f.write_str(&s[from..i])?;
            f.write_str(esc)?;
            from = i + c.len_utf8();
        }
    }
    f.write_str(&s[from..])?;
    f.write_char('"')
}

fn write_multiline_str(f: &mut fmt::Formatter, s: &str, obsolete: bool) -> Result<(), fmt::Error> {
    if s.contains('\n') {
        write!(f, "\"\"\n")?;

        let mut from = 0;
        for (i, _) in s.match_indices('\n') {
            if obsolete {
                f.write_str("#~ ")?;
            }
            if i > from || i + 1 <= s.len() {
                write_quoted_str(f, &s[from..i+1])?;
                f.write_char('\n')?;
            }
            from = i + 1;
        }
        if from < s.len() {
            write_quoted_str(f, &s[from..])?;
            f.write_char('\n')?;
        }
    } else {
        write_quoted_str(f, s)?;
        f.write_char('\n')?;
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

impl Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for comment in &self.comments {
            write!(f, "{}\n", comment)?;
        }
        if let Some(ref msgctxt) = self.msgctxt {
            write_obsolete!(self, f, "msgctxt ")?;
            write_multiline_str(f, msgctxt, self.obsolete)?;
        }
        write_obsolete!(self, f, "msgid ")?;
        write_multiline_str(f, &self.msgid, self.obsolete)?;
        match self.translation {
            Translation::Singular { ref msgstr } => {
                write_obsolete!(self, f, "msgstr ")?;
                write_multiline_str(f, msgstr, self.obsolete)
            }
            Translation::Plural { ref msgid_plural, ref msgstr } => {
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

pub fn parse(content: &[u8]) -> Result<Vec<Message>, String> {
    parser::entries(State::new(&content[..]))
        .map(|(entries, _)| entries)
        .map_err(|err| format!("{:?}", err))
}

