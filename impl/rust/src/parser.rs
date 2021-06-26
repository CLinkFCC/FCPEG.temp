pub struct Parser {
    src: String,
    index: usize,
}

impl Parser {
    pub fn new(src: String) -> Self {
        return Parser {
            src: src,
            index: 0,
        }
    }
}