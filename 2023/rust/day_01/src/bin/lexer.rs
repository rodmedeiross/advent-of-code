use std::error::Error;

pub fn read_lines(file: &str) -> Result<Vec<&str>, Box<dyn Error>> {
    let lines: Vec<&str> = file.lines().map(|line| line.trim()).collect();

    match lines.is_empty() {
        true => Err("Input is empty".into()),
        false => Ok(lines),
    }
}
