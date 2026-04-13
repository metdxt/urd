use urd::ExternObject;

#[derive(ExternObject)]
enum BadEnum {
    A,
    B(i32),
}

fn main() {}
