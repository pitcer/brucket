use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Path {
    Simple(String),
    Complex(Vec<String>),
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Path::Simple(name) => write!(f, "{}", name),
            Path::Complex(path) => write!(f, "{}", path.join("::")),
        }
    }
}
