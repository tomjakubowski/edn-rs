extern crate edn;

pub fn main() {
    let mut stdin = std::io::stdio::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        spawn(proc() {
            println!("{}", edn::parse_str(line.as_slice()))
        })
    }
}
