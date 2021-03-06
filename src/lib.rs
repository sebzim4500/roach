use crate::spec::Spec;
use openapiv3::OpenAPI;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path::Path;
use std::process::{Command, Stdio};

mod codegen;
mod spec;

pub fn generate_code_from_file(
    input: impl AsRef<Path>,
    output: impl AsRef<Path>,
) -> io::Result<()> {
    let path_ref = input.as_ref();
    let mut schema_text = String::new();
    File::open(input)?.read_to_string(&mut schema_text)?;
    let output_text = format(&generate(&schema_text));
    let mut output_file = File::create(output)?;
    output_file.write_all(&output_text)?;
    Ok(())
}

fn generate(code: &str) -> String {
    let open_api: OpenAPI = serde_yaml::from_str(code).unwrap();
    let spec = Spec::from_open_api(&open_api);
    spec.to_token_stream().to_string()
}

fn format(code: &str) -> Vec<u8> {
    let mut cmd = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .arg("--edition")
        .arg("2018")
        .arg("--config")
        .arg("normalize_doc_attributes=true")
        .spawn()
        .unwrap();
    cmd.stdin
        .as_mut()
        .unwrap()
        .write_all(code.as_bytes())
        .unwrap();
    cmd.wait_with_output().unwrap().stdout
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
