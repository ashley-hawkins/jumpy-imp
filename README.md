# JumpyImp

Arrays are implemented now but they haven't been tested very much.

Web demo is available at https://users.sussex.ac.uk/~ah2084/ji

Operator precedence hasn't been tested that much yet. If in doubt, use parentheses to disambiguate.

Unlike in slides, commented and empty lines do count towards the line numbers used for `goto line` statements.

The line number used in the `goto line` statement will just be the line number shown by any text editor that supports showing line numbers.

Also if you actually read the code then I apologise because this was my first actual program written in Haskell and I know it's really ugly compared to the stuff people can write when they're super familiar with all the syntax available to make programs more concise.

I tried to write comments at the top of each file giving an outline of the file but it's still a really terribly organised project.

Example of how to use it:

`cabal run JumpyImp -- even.ji n:10` (Should return true)

`cabal run JumpyImp -- even.ji n:11` (Should return false)

In general, the first argument is the file name of the program you want to run, and the other arguments are setting initial values for input variables, so `n:10` means give `n` a value of `10`.
