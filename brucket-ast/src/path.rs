use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    Simple(String),
    Complex(Vec<String>),
}

impl Display for Path {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Path::Simple(ref name) => write!(f, "{}", name),
            Path::Complex(ref path) => write!(f, "{}", path.join("::")),
        }
    }
}
