// Regular comment
/// Documentation comment, with **markdown** support!
/*
 * block comment
 */

// implicit return when the thing doesn't have a semicolon
fn foo(a: i32) -> i32 {
  a
}

let a = 1;
let mut b = 2;
b += 1;

let s: &str = "string literal";
println!("{}", s); // printf-like, it's a macro, hence the bang
let t: String = "hello world".to_string(); // heap allocated string
let t_slice: &str = &s; // a slice is just pointers

let foo: [i32, 1] = [1]; // fixed size
let foo = [1; 4]; // [1,1,1,1]
let vector: Vec<i32> = vec![1,1,1];
// slices work on vectors too

let (a, b) = (1, "foo"); // tuples with destructuring
let bar = (1, 2, "hello");
bar.1 == 2;

struct Point {
  x: i32,
  y: i32,
}
let thing: Point = Point { x: 0, y: 0 };
// 'tuple struct', unnamed fields
struct Point2(i32, i32);
let stuff = Point2(0, 0);

enum Foo {
  A,
  B,
}

// generics
struct A<T> { t: T }
impl<T> A<T> {
  fn get_t(self) -> T {
    self.t
  }
}

// traits are typeclasses
trait Thing<t> {
  fn thing(self) => Option<T>;
}
impl<T> Thing<T> for A<T> {
  fn thing(self) -> Option<T> {
    Some(self.t)
  }
}

match foo(whatever) {
  Ok(a) => a,
  Err(e) => {
    return;
  }
}

let xs = [1,2,3,4];
for x in xs.iter() {
  // do a thing
}
for x in 0u32..4 {
  // do a thing
}

if foo == bar {
  //
} else {
  //
}

let thing = if something() {
  "a"
} else {
  "b"
}

while a == b {
  // thing
  break
}
loop {
  // foo
  break
}
let mut foo = 1;
*foo = 2; // dereference -- foo has now been 'moved'

// references are immutable pointers
// when a reference is taken, they call that beeing 'borrowed'
// the borrowing ends when the scope of the borrowing ends
// mutable references are also a thing:
let mut foo = 1;
let foo_ref = &mut i32 = &mut foo;
*foo_ref = 2;
