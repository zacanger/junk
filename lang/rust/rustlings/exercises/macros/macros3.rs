// macros3.rs
// Make me compile, without taking the macro out of the module! Scroll down for hints :)

mod macros {
    #[macro_export]
    macro_rules! my_macro {
        () => {
            println!("Check out my macro!");
        };
    }
}

fn main() {
    my_macro!();
}
