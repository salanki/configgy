
CONFIGGY
========

Configgy is a library for handling config files and logging for a scala
daemon. The idea is that it should be simple and straightforward, allowing
you to plug it in and get started quickly.



Salanki changes are in the salanki branch, the improvments are:

Added multiple inheritance:

sub (inherit="a,b") {
…
}

Made including config files work as if the included file was actually just cut-and-pasted in the file it was included from
Made searches for inherits recursive for multiple nesting level. Starts search from current level and then goes down each level to the root
Made searches for variable substitutions recursive the same way as inherits.