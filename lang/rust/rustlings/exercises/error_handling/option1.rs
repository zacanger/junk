// option1.rs
// This example panics because the second time it calls `pop`, the `vec`
// is empty, so `pop` returns `None`, and `unwrap` panics if it's called
// on `None`. Handle this in a more graceful way than calling `unwrap`!
// Scroll down for hints :)

fn main() {
    let mut list = vec![3];

    if let Some(last) = list.pop() {
      println!("The last item in the list is {:?}", last);
    }

    if let Some(second_to_last) = list.pop() {
      println!("The second-to-last item in the list is {:?}", second_to_last);
    }
}
