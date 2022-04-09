#[test]
fn parse_fcpil() {
    use crate::*;
    use crate::il::FcpilParser;
    use cons_util::*;

    let cons = &mut Console::new("ja".to_string(), ConsoleLogLimit::NoLimit);

    let src = MultilineSource::file(FilePath::new("src/fcpeg/syntax.fcpil".to_string())).consume(cons).expect({
        cons.output(Vec::new());
        "Failed to read FCPIL file."
    });

    let rule_map = FcpilParser::parse(cons, src).expect({
        cons.output(Vec::new());
        "Failed to parse FCPIL source."
    });

    cons.output(Vec::new());

    for each_rule in rule_map.0.values() {
        println!("{}", each_rule.to_fcpil());
    }
}
