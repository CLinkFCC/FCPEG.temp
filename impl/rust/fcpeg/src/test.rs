#[test]
fn parse_fcpil() {
    use crate::il::FcpilParser;
    use cons_util::*;
    use cons_util::cons::{Console, ConsoleLogLimit};
    use cons_util::file::FileMan;

    let cons = &mut Console::new("ja".to_string(), ConsoleLogLimit::NoLimit);
    let lines = FileMan::read_lines("src/fcpeg/syntax.fcpil").consume(cons).expect("fcpil reading failure");
    let lines_ptr = lines.iter().map(|v| v.as_str()).collect::<Vec<&str>>();
    let rule_map = FcpilParser::parse(cons, lines_ptr).expect("fcpil initialization failure");
    dbg!(rule_map);
}
