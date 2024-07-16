use std::{fmt, io};

#[derive(Clone, Copy, Debug)]
pub enum ErrorCode {
    MalformedExecutable = 1,
    UnsupportedExecutable = 2,
    Internal = 3,
    Usage = 4,
    Cancelled = 5,
    Format = 6,
    Dwarf = 7,
    ProcessState = 9,
    NotImplemented = 10,
    NoSection = 12,
    Loading = 13,
    OptimizedAway = 14,
    MissingSymbols = 15,
    Sanity = 16,
    OutOfHardwareBreakpoints = 17,
    NoCodeLocations = 18,
    ValueTreePlaceholder = 19,
    TooLong = 20,
    Syntax = 21,
    NoVariable = 22,
    TypeMismatch = 23,
    Runtime = 24,
    NoFunction = 25,
    Environment = 26,
    NotCalculated = 27,
    NotContainer = 28,
}

#[derive(Debug)]
pub enum ErrorEnum {
    IO(io::Error),
    Code(ErrorCode),
}

#[derive(Clone)]
pub struct Error {
    pub error: ErrorEnum,
    pub message: String,
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait ResultWithClonableError<T> { fn as_ref_clone_error(&self) -> Result<&T>; }
impl<T> ResultWithClonableError<T> for Result<T> { fn as_ref_clone_error(&self) -> Result<&T> { match self { Ok(t) => Ok(t), Err(e) => Err(e.clone()) } } }

impl Error {
    pub fn new(code: ErrorCode, message: String) -> Error {
        Error {error: ErrorEnum::Code(code), message}
    }

    pub fn from_io_error(e: io::Error, message: String) -> Error {
        Error {error: ErrorEnum::IO(e), message}
    }

    pub fn is_no_section(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::NoSection) => true, _ => false, } }
    pub fn is_loading(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::Loading) => true, _ => false, } }
    pub fn is_usage(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::Usage) => true, _ => false, } }
    pub fn is_missing_symbols(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::MissingSymbols) => true, _ => false, } }
    pub fn is_out_of_hardware_breakpoints(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::OutOfHardwareBreakpoints) => true, _ => false, } }
    pub fn is_value_tree_placeholder(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::ValueTreePlaceholder) => true, _ => false, } }
    pub fn is_too_long(&self) -> bool { match self.error { ErrorEnum::Code(ErrorCode::TooLong) => true, _ => false, } }
    pub fn is_io_not_found(&self) -> bool { match &self.error { ErrorEnum::IO(e) if e.kind() == io::ErrorKind::NotFound => true, _ => false, } }
    pub fn is_io_permission_denied(&self) -> bool { match &self.error { ErrorEnum::IO(e) if e.kind() == io::ErrorKind::PermissionDenied => true, _ => false, } }
    pub fn is_not_calculated(&self) -> bool { match &self.error { ErrorEnum::Code(ErrorCode::NotCalculated) => true, _ => false, } }
    pub fn is_not_container(&self) -> bool { match &self.error { ErrorEnum::Code(ErrorCode::NotContainer) => true, _ => false, } }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error {error: ErrorEnum::IO(error), message: String::new()}
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(error: std::num::ParseIntError) -> Self {
        Error {error: ErrorEnum::Code(ErrorCode::Format), message: format!("{}", error)}
    }
}

impl From<gimli::Error> for Error {
    fn from(error: gimli::Error) -> Self {
        Error {error: ErrorEnum::Code(ErrorCode::Dwarf), message: format!("{}", error)}
    }
}

impl From<std::str::Utf8Error> for Error {
    fn from(error: std::str::Utf8Error) -> Self {
        Error {error: ErrorEnum::Code(ErrorCode::Format), message: format!("{}", error)}
    }
}

impl From<std::string::FromUtf8Error> for Error {
    fn from(error: std::string::FromUtf8Error) -> Self {
        Error {error: ErrorEnum::Code(ErrorCode::Format), message: format!("{}", error)}
    }
}

impl From<std::fmt::Error> for Error {
    fn from(error: std::fmt::Error) -> Self {
        Error {error: ErrorEnum::Code(ErrorCode::Format), message: format!("{}", error)}
    }
}

impl From<std::env::VarError> for Error {
    fn from(error: std::env::VarError) -> Self {
        Error {error: ErrorEnum::Code(ErrorCode::Environment), message: format!("{}", error)}
    }
}

// For printing to log.
impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.error {
            &ErrorEnum::Code(code) => write!(f, "{}: {}", code as i64, self.message),
            ErrorEnum::IO(error) => write!(f, "{}: {}", self.message, error),
        }
    }
}

// For showing to the user.
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.error {
            &ErrorEnum::Code(_) => write!(f, "{}", self.message),
            ErrorEnum::IO(error) if self.message.is_empty() => write!(f, "{}", error),
            ErrorEnum::IO(error) => write!(f, "{}: {}", self.message, error),
        }
    }
}

impl Clone for ErrorEnum {
    fn clone(&self) -> Self {
        match self {
            Self::Code(c) => Self::Code(c.clone()),
            Self::IO(e) => Self::IO(match e.raw_os_error() {
                Some(os) => io::Error::from_raw_os_error(os),
                None => e.kind().into(),
            }),
        }
    }
}

#[macro_export]
macro_rules! error {
    ($code:ident, $($arg:tt)*) => (
        Error {error: ErrorEnum::Code(ErrorCode::$code), message: format!($($arg)*)}
    );
}

#[macro_export]
macro_rules! err {
    ($code:ident, $($arg:tt)*) => (
        Err(error!($code, $($arg)*))
    );
}

#[macro_export]
macro_rules! errno_err {
    ($($arg:tt)*) => (
        //panic!("{:?}", Error {error: ErrorEnum::IO(io::Error::last_os_error()), message: format!($($arg)*)})
        Err(Error {error: ErrorEnum::IO(::std::io::Error::last_os_error()), message: format!($($arg)*)})
    );
}
