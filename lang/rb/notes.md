strings with variables NEED double quotes. single quotes will print the string literally. same as backticks in es6

%q(string)
or %Q({string}) for interpolation
(doesn't HAVE to be parens, apparently.)
i
<<-WHATEVER
Here's a doc.
It's a heredoc. Lol.
Multiline works just like other heredocs.
Variables work too.
WHATEVER

.upcase
.downcase

why not .toupper or .tolower like a normal language?

.reverse

variable-whatever = variable.reverse

duh

method that ends with ! means variable is changed. so, name.upcase! means name = name.upcase

in (returned) strings, one can perform operations inside the braces,

.to_i = to integer

.chomp (as in gets.chomp) removes the automatic newline that gets inserts.

load "./file.rb" (in irb, but in scripts also?) to source other files.

`*=, +=` will work on strings, too, just concat sorta?

