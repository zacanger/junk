* page 151
* car and cdr only work on non-empty lists
* cdr list will always return list
* cons s-expr list
* null? list (not atom -- will be #f for all non-empty list)
* atom? s-expr
* eq? takes 2 non num atoms (can actually do lists, but i guess shouldn't?)
* (can also do nums, a/l in rkt, maybe not in general schemes?)
* lat? : is list of atoms? (no lists in list)
* positive integers are atoms
  * there are no negative numbers (?)
  * ...or floats (?)
* you can do `zero?` on numbers like `null?` on lists and that's okay
* tuples are lists of numbers
  * this includes an empty list
* `addtup` is like `map + list`
