# JumpyImp

Unimplemented features: Arrays

Known parser issues: Empty lines cause a parsing error

I'm gonna reimplement the parser and implement arrays in the new version I think.

Also if you actually read the code then I apologise because this was my first actual program written in Haskell and I know it's really ugly compared to the stuff people can write when they're super familiar with all the syntax available to make programs more concise.

I tried to write comments at the top of each file giving an outline of the file but it's still a really terribly organised project.

Example of how to use it:

`cabal run JumpyImp -- even.ji n:10` (Should return true)
`cabal run JumpyImp -- even.ji n:11` (Should return false)