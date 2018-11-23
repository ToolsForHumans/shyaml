#[macro_use]
extern crate serde_derive;

extern crate clap;
extern crate serde_json;
extern crate serde_yaml;
extern crate tempdir;

use std::io;
use std::io::Read;
use std::fs::File;
use std::string;
use std::process::exit;
use std::fmt;
use std::iter::FromIterator;
use std::process::Command;
use std::str;

use clap::{Arg, App, SubCommand};
use serde_yaml::Value as YValue;
use tempdir::TempDir;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

#[derive(Debug)]
enum Error {
    Io(io::Error),
    Yaml(serde_yaml::Error),
    Json(serde_json::Error),
    Utf8(string::FromUtf8Error),
    Other(&'static str),
}

impl From<serde_yaml::Error> for Error {
    fn from(error: serde_yaml::Error) -> Self {
        Error::Yaml(error)
    }
}

impl From<serde_json::Error> for Error {
    fn from(error: serde_json::Error) -> Self {
        Error::Json(error)
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::Io(error)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(error: string::FromUtf8Error) -> Self {
        Error::Utf8(error)
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Io(ref e) => write!(f, "{}", e),
            Error::Yaml(ref e) => write!(f, "{}", e),
            Error::Json(ref e) => write!(f, "{}", e),
            Error::Utf8(ref e) => write!(f, "{}", e),
            Error::Other(ref e) => write!(f, "{}", e),
        }
    }
}


fn main() {
    let app = App::new("shyaml")
        .version(VERSION)
        .author(AUTHOR)
        .about("YAML manipulation utility")
        .subcommand(
            SubCommand::with_name("json")
                .about("converts yaml to json")
                .arg(
                    Arg::with_name("input")
                        .help("Input file, - for stdin")
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("diff")
                .about("diff two yaml docs")
                .arg(Arg::with_name("input1").help("Input file #1").index(1))
                .arg(Arg::with_name("input2").help("Input file #2").index(2)),
        )
        .subcommand(
            SubCommand::with_name("keysort")
                .about("print yaml with keys sorted")
                .arg(Arg::with_name("input").help("Input file").index(1)),
        )
        .subcommand(
            SubCommand::with_name("kubediff")
                .about("print diff via Kube api")
                .arg(Arg::with_name("input").help("Input file").index(1)),
        )
        .get_matches();

    let result = match app.subcommand_name() {
        Some("json") => {
            let in_path = app.subcommand_matches("json")
                .unwrap()
                .value_of("input")
                .unwrap_or("-");
            to_json(&in_path)
        }
        Some("diff") => {
            let input1 = app.subcommand_matches("diff")
                .unwrap()
                .value_of("input1")
                .unwrap();
            let input2 = app.subcommand_matches("diff")
                .unwrap()
                .value_of("input2")
                .unwrap();
            diff(&input1, &input2)
        }
        Some("keysort") => {
            let in_path = app.subcommand_matches("keysort")
                .unwrap()
                .value_of("input")
                .unwrap_or("-");
            keysort(&in_path)
        }
        Some("kubediff") => {
            let in_path = app.subcommand_matches("kubediff")
                .unwrap()
                .value_of("input")
                .unwrap_or("-");
            kubediff(&in_path)
        }
        _ => Err(Error::Other("No subcommand passed.")),
    };
    exit(match result {
        Ok(_) => 0,
        Err(e) => {
            eprintln!("ERROR: {}", e);
            1
        }
    });
}

fn parse_yaml_file(in_path: &str) -> Result<YValue, Error> {
    if in_path == "-" {
        Ok(serde_yaml::from_reader(std::io::stdin())?)
    } else {
        Ok(serde_yaml::from_reader(File::open(in_path)?)?)
    }
}

fn to_json(in_path: &str) -> Result<(), Error> {
    let doc = parse_yaml_file(in_path)?;
    print!("{}", serde_json::to_string_pretty(&doc)?);
    Ok(())
}

fn diff(input1: &str, input2: &str) -> Result<(), Error> {
    let in1_parsed: YValue = serde_yaml::from_reader(File::open(input1)?)?;
    let in2_parsed: YValue = serde_yaml::from_reader(File::open(input2)?)?;
    find_diff(&in1_parsed, &in1_parsed, &in2_parsed, &in2_parsed)
}

fn prefix_lines(s: &String, prefix: &'static str) -> String {
    let mut prefixed = String::new();
    for line in s.lines() {
        prefixed.push_str(&format!("{}{}\n", prefix, line));
    }
    prefixed
}

fn diff_print(value1: &YValue, value2: &YValue) -> Result<(), Error> {
    print!(
        "{}",
        prefix_lines(&serde_json::to_string_pretty(value1)?, "- ")
    );
    print!(
        "{}",
        prefix_lines(&serde_json::to_string_pretty(value2)?, "+ ")
    );
    Ok(())
}

fn find_diff(
    parent1: &YValue,
    value1: &YValue,
    parent2: &YValue,
    value2: &YValue,
) -> Result<(), Error> {
    // Recursive function that will keep walkign down containers until differences are found
    eprintln!("Checking {:?} versus {:?}", value1, value2);
    match value1 {
        &YValue::Sequence(ref s1) => {
            match value2.as_sequence() {
                None => (),
                Some(s2) => {
                    let mut s2 = s2.iter();
                    for seqval1 in s1.iter() {
                        match s2.next() {
                            None => diff_print(value1, &YValue::Null)?,
                            Some(seqval2) => find_diff(value1, seqval1, value2, seqval2)?,
                        }
                    }
                    return Ok(());
                }
            }
        }
        &YValue::Mapping(ref m1) => {
            match value2.as_mapping() {
                None => (),
                Some(m2) => {
                    for kv1 in m1.iter() {
                        match m2.get(kv1.0) {
                            None => diff_print(kv1.0, &YValue::Null)?,
                            Some(mapval2) => {
                                find_diff(value1, kv1.1, value2, mapval2)?;
                            }
                        }
                    }
                    for kv2 in m2.iter() {
                        match m1.get(kv2.0) {
                            None => diff_print(&YValue::Null, kv2.1)?,
                            Some(_) => (), // If they are different, we already printed it
                        }
                    }
                    return Ok(());
                }
            }
        }
        &YValue::Number(ref n1) => {
            if value2.is_number() && n1.is_f64() && value2.is_f64() &&
                value1.as_f64() == value2.as_f64()
            {
                return Ok(());
            } else if value1.is_u64() && value2.is_u64() && value1.as_u64() == value2.as_u64() {
                return Ok(());
            } else if value1.is_i64() && value2.is_i64() && value1.as_i64() == value2.as_i64() {
                return Ok(());
            }
        }
        &YValue::String(ref s1) => {
            match value2.as_str() {
                None => (),
                Some(s2) => {
                    if s2 == s1 {
                        return Ok(());
                    }
                }
            }
        }
        &YValue::Bool(ref b1) => {
            match value2.as_bool() {
                None => (),
                Some(ref b2) => {
                    if b2 == b1 {
                        return Ok(());
                    }
                }
            }
        }
        &YValue::Null => {
            if value2.is_null() {
                return Ok(());
            }
        }
    };
    diff_print(parent1, parent2)
}

/// Walk the doc, sorting keys if needed, returning a new doc that has keys sorted
fn sort_mapping_keys(doc: &YValue) -> YValue {
    match doc {
        &YValue::Mapping(ref m) => {
            let mut keys: Vec<(YValue, YValue)> = m.clone().into_iter().collect();
            keys.sort_unstable_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
            YValue::Mapping(serde_yaml::Mapping::from_iter(keys.iter().map(
                |kv| (kv.0.clone(), sort_mapping_keys(&kv.1)),
            )))
        }
        &YValue::Sequence(ref s) => {
            let values: Vec<YValue> = s.clone();
            YValue::Sequence(
                values
                    .iter()
                    .map(|value| sort_mapping_keys(&value))
                    .collect(),
            )
        }
        &YValue::Null => YValue::Null,
        &YValue::Bool(ref b) => YValue::Bool(b.clone()),
        &YValue::Number(ref n) => YValue::Number(n.clone()),
        &YValue::String(ref s) => YValue::String(s.clone()),
    }
}

fn write_indent<W>(writer: &mut W, indent: usize) -> Result<(), Error>
where
    W: io::Write,
{
    for _indents in 0..indent {
        writer.write_all(b"  ")?;
    }
    Ok(())
}
fn to_writer_pretty<W>(writer: &mut W, doc: &YValue) -> Result<(), Error>
where
    W: io::Write,
{
    node_to_writer_pretty(writer, doc, 0, &YValue::Null)
}

fn node_to_writer_pretty<W>(
    writer: &mut W,
    doc: &YValue,
    indent: usize,
    parent: &YValue,
) -> Result<(), Error>
where
    W: io::Write,
{
    if let &YValue::Mapping(ref m) = doc {
        if let &YValue::Mapping(_) = parent {
            writer.write_all(b":\n")?;
        }
        for (key, value) in m.iter() {
            write_indent(writer, indent)?;
            let key_str = serde_yaml::to_string(key)?;
            // Chopping documentstart off
            writer.write_all(&key_str[4..].as_bytes())?;
            node_to_writer_pretty(writer, value, indent + 1, doc)?;
        }
    } else if let &YValue::Sequence(ref s) = doc {
        if let &YValue::Mapping(_) = parent {
            writer.write_all(b":\n")?;
        }
        for value in s.iter() {
            node_to_writer_pretty(writer, value, indent, doc)?;
        }
    } else if let &YValue::String(ref s) = doc {
        // Search for newlines
        if s.len() > 0 && s.bytes().position(|x| x == b'\n').is_some() &&
            s.bytes().last().unwrap() == b'\n'
        {
            if let &YValue::Mapping(_) = parent {
                writer.write_all(b": |\n")?;
            }
            for line in s.lines() {
                write_indent(writer, indent)?;
                writer.write_all(line.as_bytes())?;
                writer.write_all(&[b'\n'])?;
            }
        } else {
            if let &YValue::Mapping(_) = parent {
                writer.write_all(b": ")?;
            } else if let &YValue::Sequence(_) = parent {
                write_indent(writer, indent)?;
                writer.write_all(b"- ")?;
            }
            let raw_yaml = serde_yaml::to_string(&doc)?;
            writer.write_all(&raw_yaml[4..].as_bytes())?;
            writer.write_all(b"\n")?;
        }
    } else {
        if let &YValue::Mapping(_) = parent {
            writer.write_all(b": ")?;
        } else if let &YValue::Sequence(_) = parent {
            write_indent(writer, indent)?;
            writer.write_all(b"- ")?;
        }
        // XXX It thinks everything is a document  :|
        let raw_yaml = serde_yaml::to_string(&doc)?;
        writer.write_all(&raw_yaml[4..].as_bytes())?;
        match *parent {
            YValue::Mapping(_) |
            YValue::Sequence(_) => {
                writer.write_all(b"\n")?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn _test_yaml(yaml: &'static str) {
    let doc: Result<YValue, serde_yaml::Error> = serde_yaml::from_str(yaml);
    match doc {
        Err(e) => assert!(false, "YAML Error: {}", e),
        Ok(doc) => {
            let mut pretty = Vec::with_capacity(128);
            match to_writer_pretty(&mut pretty, &doc) {
                Err(e) => assert!(false, "Writer fail: {}", e),
                Ok(_) => {
                    assert_eq!(str::from_utf8(&pretty).unwrap(), yaml);
                    // And now can we parse it again?
                    let parsed_p: Result<YValue, serde_yaml::Error> =
                        serde_yaml::from_slice(&pretty);
                    match parsed_p {
                        Err(e) => {
                            assert!(
                                false,
                                "Second parse failed: {} [{}]",
                                e,
                                str::from_utf8(&pretty).unwrap()
                            )
                        }
                        Ok(parsed_p) => assert_eq!(parsed_p, doc),
                    }
                }
            }
        }
    }
}

#[test]
fn t_node_to_writer_pretty() {
    _test_yaml(
        "a: 1\nb: |\n  line1\n  line2\nc:\n  z: \"linez0\\nlinez1\"\n",
    );
    _test_yaml(
        "apiVersion: foo/bar\nkind: Deployment\nmetadata:\n  labels:\n    k8s-app: foo\n",
    );
    _test_yaml(
        //"alist:\n  - 99\n  - \"dashed-string\"\n  - amap:\n      akey: zoop\n",
        "alist:\n  - 99\n  - dashed-string\n",
    );
}

fn keysort(in_path: &str) -> Result<(), Error> {
    let doc = parse_yaml_file(in_path)?;
    to_writer_pretty(&mut io::stdout(), &sort_mapping_keys(&doc))?;
    Ok(())
}

#[derive(Deserialize)]
struct KubeMetadata {
    name: String,
    namespace: Option<String>,
}

#[derive(Deserialize)]
struct KubeDoc {
    kind: String,
    metadata: KubeMetadata,
}

fn kubediff(in_path: &str) -> Result<(), Error> {
    let mut yaml_source = Vec::with_capacity(1024 * 1024);
    if in_path == "-" {
        std::io::stdin().read_to_end(&mut yaml_source)?
    } else {
        File::open(in_path)?.read_to_end(&mut yaml_source)?
    };
    let doc: KubeDoc = serde_yaml::from_slice(&yaml_source)?;
    let kind = doc.kind.to_lowercase();
    let ns_args = match doc.metadata.namespace.clone() {
        Some(namespace) => vec![String::from("--namespace"), namespace],
        None => vec![],
    };
    let mut args = vec!["get", kind.as_str(), doc.metadata.name.as_str(), "-o", "yaml"];
    args.extend(ns_args.iter().map(|ref v| v.as_str()));
    let server_source = {
        let mut in_server = Command::new("kubectl");
        in_server.args(&args);
        eprintln!("Running: {:?}", in_server);
        in_server.output()?.stdout
    };
    // Get the raw Yaml now from local doc
    let local_doc: YValue = serde_yaml::from_slice(&yaml_source)?;
    let mut server_doc: YValue = serde_yaml::from_slice(&server_source)?;
    // If we have a last-applied, use that instead
    let new_server_doc = if let Some(md) = server_doc.get("metadata") {
        if let Some(a) = md.get("annotations") {
            if let Some(last) = a.get("kubectl.kubernetes.io/last-applied-configuration") {
                Some(last.clone())
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };
    if let Some(nsd) = new_server_doc {
        if nsd.is_string() {
            // It's json, convert to yaml values -- Thanks YAML for parsing json cleanly!
            let doc: serde_json::Value = serde_json::from_str(nsd.as_str().unwrap())?;
            server_doc = serde_yaml::from_str(&serde_json::to_string(&doc)?)?;
        } else {
            return Err(Error::Other(
                "Weird non-string for last-applied-configuration",
            ));
        }
    }
    // Filter out stuff that doesn't matter from server_doc
    if let Some(m) = server_doc.as_mapping_mut() {
        if let Some(md) = m.get_mut(&serde_yaml::from_str("metadata")?) {
            if let Some(md) = md.as_mapping_mut() {
                md.remove(&serde_yaml::from_str("creationTimestamp")?);
                md.remove(&serde_yaml::from_str("resourceVersion")?);
                md.remove(&serde_yaml::from_str("status")?);
                md.remove(&serde_yaml::from_str("selfLink")?);
                if doc.metadata.namespace.is_none() {
                    md.remove(&serde_yaml::from_str("namespace")?);
                }
            } else {
                return Err(Error::Other("metadata was not a mapping"));
            }
        }
        m.remove(&serde_yaml::from_str("status")?);
    } else {
        return Err(Error::Other("Non-mapping received from server!"));
    };
    let server_doc = server_doc;
    // Sort mapping keys on both
    let local_sorted = sort_mapping_keys(&local_doc);
    let server_sorted = sort_mapping_keys(&server_doc);
    let diff_temp = TempDir::new("kubediff")?;
    let local_path = diff_temp.path().join("local");
    let server_path = diff_temp.path().join("server");
    let mut local_file = File::create(&local_path)?;
    let mut server_file = File::create(&server_path)?;
    to_writer_pretty(&mut local_file, &local_sorted)?;
    to_writer_pretty(&mut server_file, &server_sorted)?;
    Command::new("diff")
        .arg("-u")
        .arg(&server_path)
        .arg(&local_path)
        .status()?;
    Ok(())
}
