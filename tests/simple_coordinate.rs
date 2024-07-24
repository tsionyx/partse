use std::fmt::Debug;

use nom::{
    character::complete::{alphanumeric1, digit1},
    combinator::map_res,
    number::complete::double,
    IResult,
};
use partse::{gen_parts, AtomParser, Error};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Pole {
    North,
    South,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum RotationalDirection {
    East,
    West,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(non_camel_case_types)]
enum CoordinateReferenceSystem {
    CRSWGS_84,
}

// TODO: valid combinations:
enum Latitude {
    // u (m (s|S)? | M)? (p | P)?
    // i (m (s|S)? | M)?
    // U (p | P)?
    // I
}

enum Longitude {
    // d (n (t|T)? | N)? (r | R)?
    // f (n (t|T)? | N)?
    // D (r | R)?
    // F
}

enum Coordinate {
    // Lat Long (h | z | H | Z)? c?
}

gen_parts! {
    #[derive(Debug, Copy, Clone, PartialEq)]
    enum CoordinatePart: CoordinateAtom: CoordinateAtomName {
        // Latitude-specific
        LatDegrees(f64) as oneof {
            LatDegreesU(u8) = 'u' => parse_u8; |x: &u8| *x <= 90,
            LatDegreesI(i8) = 'i' => parse_i8; |x: &i8| (-90..=90).contains(x),
            LatDegreesDecimalU(f64) = 'U' => parse_f64_non_neg; |x: &f64| *x <= 90.0,
            LatDegreesDecimalI(f64) = 'I' => parse_f64; |x: &f64| *x >= -90.0 && *x <= 90.0,
        }

        LatMinutes(f64) as oneof {
            LatMinutes(u8) = 'm' => parse_u8; |x: &u8| *x < 60,
            LatMinutesFractional(f64) = 'M' => parse_f64_non_neg; |x: &f64| *x <= 60.0,
        }

        LatSeconds(f64) as oneof {
            LatSeconds(u8) = 's' => parse_u8; |x: &u8| *x < 60,
            LatSecondsFractional(f64) = 'S' => parse_f64_non_neg; |x: &f64| *x <= 60.0,
        }

        LatHemisphere(Pole) as oneof {
            LatHemisphereSign(Pole) = 'p' => parse_lat_hemisphere_sign; |_| true,
            LatHemisphere(Pole) = 'P' => parse_lat_hemisphere; |_| true,
        }

        // Longitude-specific
        LongDegrees(f64) as oneof {
            LongDegreesU(u8) = 'd' => parse_u8; |x: &u8| *x <= 180,
            LongDegreesI(i16) = 'f' => parse_i16; |x: &i16| (-180..=180).contains(x),
            LongDegreesDecimalU(f64) = 'D' => parse_f64_non_neg; |x: &f64| *x <= 180.0,
            LongDegreesDecimalI(f64) = 'F' => parse_f64; |x: &f64| *x >= -180.0 && *x <= 180.0,
        }

        LongMinutes(f64) as oneof {
            LongMinutes(u8) = 'n' => parse_u8; |x: &u8| *x < 60,
            LongMinutesFractional(f64) = 'N' => parse_f64_non_neg; |x: &f64| *x <= 60.0,
        }

        LongSeconds(f64) as oneof {
            LongSeconds(u8) = 't' => parse_u8; |x: &u8| *x < 60,
            LongSecondsFractional(f64) = 'T' => parse_f64_non_neg; |x: &f64| *x <= 60.0,
        }

        LongHemisphere(RotationalDirection) as oneof {
            LongHemisphereSign(RotationalDirection) = 'r' => parse_long_hemisphere_sign; |_| true,
            LongHemisphere(RotationalDirection) = 'R' => parse_long_hemisphere; |_| true,
        }

        Height(f64) as oneof {  // (z-coordinate)
            HeightU(u16) = 'h' => parse_u16; |_| true,
            HeightI(i16) = 'z' => parse_i16; |_| true,
            HeightDecimalU(f64) = 'H' => parse_f64_non_neg; |_| true,
            HeightDecimalI(f64) = 'Z' => parse_f64; |_| true,
        }

        ReferenceSystem(CoordinateReferenceSystem) as oneof {
            ReferenceSystem(CoordinateReferenceSystem) = 'c' => parse_crs; |_| true,
        }
    }
}

fn parse_u8(input: &str) -> IResult<&str, u8> {
    map_res(digit1, str::parse)(input)
}

fn parse_u16(input: &str) -> IResult<&str, u16> {
    map_res(digit1, str::parse)(input)
}

fn parse_i8(input: &str) -> IResult<&str, i8> {
    map_res(digit1, str::parse)(input)
}

fn parse_i16(input: &str) -> IResult<&str, i16> {
    map_res(digit1, str::parse)(input)
}

fn parse_f64(input: &str) -> IResult<&str, f64> {
    double(input)
}

fn parse_f64_non_neg(input: &str) -> IResult<&str, f64> {
    let (rest, x) = double(input)?;
    if x.is_sign_negative() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Float,
        )));
    }
    Ok((rest, x))
}

fn parse_lat_hemisphere_sign(input: &str) -> IResult<&str, Pole> {
    let (input, hemisphere) = alphanumeric1(input)?;
    match hemisphere {
        "+" => Ok((input, Pole::North)),
        "-" => Ok((input, Pole::South)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn parse_lat_hemisphere(input: &str) -> IResult<&str, Pole> {
    let (input, hemisphere) = alphanumeric1(input)?;
    match hemisphere {
        "N" => Ok((input, Pole::North)),
        "S" => Ok((input, Pole::South)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn parse_long_hemisphere_sign(input: &str) -> IResult<&str, RotationalDirection> {
    let (input, hemisphere) = alphanumeric1(input)?;
    match hemisphere {
        "+" => Ok((input, RotationalDirection::East)),
        "-" => Ok((input, RotationalDirection::West)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn parse_long_hemisphere(input: &str) -> IResult<&str, RotationalDirection> {
    let (input, hemisphere) = alphanumeric1(input)?;
    match hemisphere {
        "E" => Ok((input, RotationalDirection::East)),
        "W" => Ok((input, RotationalDirection::West)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn parse_crs(input: &str) -> IResult<&str, CoordinateReferenceSystem> {
    match input {
        "CRSWGS_84" => Ok((input, CoordinateReferenceSystem::CRSWGS_84)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

#[cfg(test)]
mod tests {
    use partse::PartParser as _;

    use super::*;

    #[test]
    fn valid_latitude() {
        let format = "%u deg %m min %S sec (%P)";
        let input = "72 deg 37 min 19.11792 sec (N)";

        let (_, values) = CoordinateAtom::parse(input, format).expect("Parsing failed");
        let lat = [
            CoordinateAtom::LatDegreesU(72),
            CoordinateAtom::LatMinutes(37),
            CoordinateAtom::LatSecondsFractional(19.11792),
            CoordinateAtom::LatHemisphere(Pole::North),
        ];
        assert_eq!(values, lat);

        let (_, values) = CoordinatePart::parse(input, format).expect("Parsing failed");
        let lat = [
            CoordinatePart::LatDegrees(72.0),
            CoordinatePart::LatMinutes(37.0),
            CoordinatePart::LatSeconds(19.11792),
            CoordinatePart::LatHemisphere(Pole::North),
        ];
        assert_eq!(values, lat);
    }

    #[test]
    fn valid_decimal_latitude() {
        let format = "%U (%P)";
        let input = "72.61976 (N)";

        let (_, values) = CoordinateAtom::parse(input, format).expect("Parsing failed");
        let lat = [
            CoordinateAtom::LatDegreesDecimalU(72.61976),
            CoordinateAtom::LatHemisphere(Pole::North),
        ];
        assert_eq!(values, lat);

        let (_, values) = CoordinatePart::parse(input, format).expect("Parsing failed");
        let lat = [
            CoordinatePart::LatDegrees(72.61976),
            CoordinatePart::LatHemisphere(Pole::North),
        ];
        assert_eq!(values, lat);
    }

    #[test]
    #[should_panic = "DegreesU(91) does not match requirements"]
    fn invalid_latitude() {
        let format = "%u deg %m min %S sec (%Z)";
        let input = "91 deg 37 min 19.11792 sec (N)";

        let _ = CoordinateAtom::parse(input, format).unwrap();
    }

    #[test]
    fn invalid_format_specifier() {
        let format = "%u deg %m min %x sec (%Z)";
        let input = "72 deg 37 min 19.11792 sec (N)";

        let err = CoordinateAtom::parse(input, format).expect_err("Parsing succeeded");
        assert_eq!(err, Error::InvalidFormatSpecifier('x'));
    }

    #[test]
    fn duplicate_degrees_in_latitude() {
        let format = "%u deg %U min %S sec (%P)";
        let input = "72 deg 37 min 19.11792 sec (N)";

        let (_, values) = CoordinateAtom::parse(input, format).expect("Parsing failed");
        let lat = [
            CoordinateAtom::LatDegreesU(72),
            CoordinateAtom::LatDegreesDecimalU(37.0),
            CoordinateAtom::LatSecondsFractional(19.11792),
            CoordinateAtom::LatHemisphere(Pole::North),
        ];
        assert_eq!(values, lat);

        let err = CoordinatePart::parse(input, format).unwrap_err();
        assert_eq!(
            err,
            Error::DuplicateAtoms {
                previous_part: "LatDegrees(72.0)".into(),
                new_part: "LatDegrees(37.0)".into(),
            }
        );
    }

    #[test]
    fn test_unexpected_end_of_input() {
        let format = "%u deg %m min %S sec (%Z)";
        let input = "72 deg 37 min 19.11792 sec";

        let err = CoordinateAtom::parse(input, format).expect_err("Parsing succeeded");
        assert_eq!(err, Error::UnexpectedEol);
    }

    #[test]
    fn literal_parsing() {
        let format = "Latitude: %u degrees";
        let input = "Latitude: 72 degrees";

        let (_, values) = CoordinateAtom::parse(input, format).expect("Parsing failed");
        let lat = [CoordinateAtom::LatDegreesU(72)];
        assert_eq!(values, lat);

        let (_, values) = CoordinatePart::parse(input, format).expect("Parsing failed");
        let lat = [CoordinatePart::LatDegrees(72.0)];
        assert_eq!(values, lat);
    }
}

#[cfg(test)]
mod helper_tests {
    use super::*;

    #[test]
    fn test_parse_u8() {
        let input = "123";
        let res = parse_u8(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, 123);
    }

    #[test]
    fn test_parse_f64() {
        let input = "123.456";
        let res = parse_f64(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, 123.456);
    }

    #[test]
    fn test_parse_hemisphere_north() {
        let input = "N";
        let res = parse_lat_hemisphere(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, Pole::North);
    }

    #[test]
    fn test_parse_hemisphere_south() {
        let input = "S";
        let res = parse_lat_hemisphere(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, Pole::South);
    }

    #[test]
    fn test_parse_hemisphere_invalid() {
        let input = "X";
        let res = parse_lat_hemisphere(input);
        assert!(res.is_err(), "Parsing succeeded with invalid hemisphere");
    }
}
