fn calculateprice (n: i32) -> i32 {
  if n % 2 == 0 {
    n * 2
  } else {
    n
  }
}
// Don't modify this function!
#[test]
fn verify_test() {
    let price1 = calculateprice(55);
    let price2 = calculateprice(40);

    assert_eq!(price1, 55);
    assert_eq!(price2, 80);
}
