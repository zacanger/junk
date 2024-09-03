// primitive_types4.rs
// Get a slice out of Array a where the ??? is so that the `if` statement
// returns true. Scroll down for hints!!

fn main() {
    let a = [1, 2, 3, 4, 5];

    let nice_slice = &a[1..3];

    if nice_slice == [2, 3, 4] {
        println!("Nice slice!");
    } else {
        println!("Not quite what I was expecting... I see: {:?}", nice_slice);
    }
}
