use std::fmt::Debug;

use nom::{bytes::complete::tag, error::Error as NomInternalError, IResult};

pub trait FormatSpecifier {
    const TEMPLATE_SYMBOL: char = '%';

    fn symbol(&self) -> char;

    fn from_symbol(ch: char) -> Option<Self>
    where
        Self: Sized;
}

#[derive(Debug)]
enum FullFormatSpecifier<FS> {
    Domain(FS),
    Literal(String),
    // TODO: whitespaces
}

impl<FS> FullFormatSpecifier<FS> {
    fn parse_format_string(format: &str) -> Result<Vec<Self>, Error>
    where
        FS: FormatSpecifier,
    {
        let percent = FS::TEMPLATE_SYMBOL;

        let mut specifiers = Vec::new();
        let mut chars = format.chars().peekable();

        while let Some(c) = chars.next() {
            if c == percent {
                if let Some(next_ch) = chars.next() {
                    if next_ch == percent {
                        specifiers.push(Self::Literal(percent.to_string()));
                    } else if let Some(spec) = FS::from_symbol(next_ch) {
                        specifiers.push(Self::Domain(spec));
                    } else {
                        return Err(Error::InvalidFormatSpecifier(next_ch));
                    }
                } else {
                    return Err(Error::UnexpectedEol);
                }
            } else {
                let mut literal = c.to_string();
                while let Some(&next_c) = chars.peek() {
                    if next_c == percent {
                        break;
                    }
                    literal.push(chars.next().unwrap());
                }
                specifiers.push(Self::Literal(literal));
            }
        }

        Ok(specifiers)
    }

    fn parse_single<'i, T>(&self, input: &'i str) -> Result<(&'i str, Option<T>), Error>
    where
        T: PartParser<ItemSpecifier = FS> + Debug,
        FS: Debug,
    {
        match self {
            Self::Domain(format) => {
                let (rest, value) = T::parse_domain(input, format)?;
                if value.is_valid() {
                    Ok((rest, Some(value)))
                } else {
                    Err(Error::InvalidValue {
                        msg: format!("Value {value:?} does not match requirements of {format:?}"),
                    })
                }
            }
            Self::Literal(literal) => {
                if input.len() < literal.len() {
                    return Err(Error::UnexpectedEol);
                }
                let (rest, _) = tag::<_, _, NomInternalError<_>>(literal.as_str())(input)?;
                Ok((rest, None))
            }
        }
    }

    fn parse_many<'i, T>(selves: &[Self], input: &'i str) -> Result<(&'i str, Vec<T>), Error>
    where
        T: PartParser<ItemSpecifier = FS> + Debug,
        FS: Debug,
    {
        let mut results = Vec::new();
        let mut remainder = input;

        for specifier in selves {
            let (rest, value) = specifier.parse_single::<T>(remainder)?;
            if let Some(val) = value {
                results.push(val);
            }
            remainder = rest;
        }

        Ok((remainder, results))
    }
}

pub trait PartParser {
    type ItemSpecifier: FormatSpecifier;

    fn get_specifier(&self) -> Self::ItemSpecifier;

    fn is_valid(&self) -> bool;

    fn parse_domain<'i>(input: &'i str, format: &Self::ItemSpecifier) -> IResult<&'i str, Self>
    where
        Self: Sized;

    fn parse<'i>(input: &'i str, format: &str) -> Result<(&'i str, Vec<Self>), Error>
    where
        Self: Sized + Debug,
        Self::ItemSpecifier: Debug,
    {
        let format_specifiers = FullFormatSpecifier::parse_format_string(format)?;
        FullFormatSpecifier::parse_many(&format_specifiers, input)
    }
}

type NomError<E> = nom::Err<NomInternalError<E>>;

#[derive(Debug, PartialEq)]
pub enum Error {
    Nom(NomError<String>),
    InvalidValue { msg: String },
    InvalidFormatSpecifier(char),
    UnexpectedEol,
}

impl From<NomError<String>> for Error {
    fn from(value: NomError<String>) -> Self {
        Self::Nom(value)
    }
}

impl<'a> From<NomError<&'a str>> for Error {
    fn from(value: NomError<&'a str>) -> Self {
        Self::Nom(value.map_input(str::to_string))
    }
}
