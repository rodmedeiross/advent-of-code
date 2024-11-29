use md5;

const SECRET: &str = "bgvyzdsv";

fn bruteforce_md5(n_rand: usize) -> (String, u64) {
    let mut counter = 1_u64;
    let tg = "0".repeat(n_rand);

    loop {
        let secret_input = format!("{}{}", SECRET, counter);
        let hash = calculate_md5(&secret_input);

        if hash.starts_with(&tg) {
            return (hash, counter);
        }
        counter += 1;
    }
}

fn calculate_md5(input: &str) -> String {
    let result = md5::compute(input);
    format!("{:x}", result)
}

fn main() {
    let (hash, counter) = bruteforce_md5(6);
    println!("The MD5: {}, and the magic number {}", hash, counter);
}
