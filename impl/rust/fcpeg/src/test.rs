#[test]
fn parse_fcpil() {
    use crate::*;
    use crate::il::FcpilParser;
    use cons_util::*;
    use cons_util::cons::*;

    let cons = &mut Console::new("ja".to_string(), ConsoleLogLimit::NoLimit);

    let src = MultilineSource::file(PathBuf::from("src/fcpeg/syntax.fcpil")).consume(cons).expect({
        cons.output(Vec::new());
        "Failed to read FCPIL file."
    });

    let rule_map = FcpilParser::parse(cons, src).expect({
        cons.output(Vec::new());
        "Failed to parse FCPIL source."
    });

    cons.output(Vec::new());

    dbg!(rule_map);
}
