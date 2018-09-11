# battlefront

A small 2D game project to try and learn common lisp!

This isn't really in a state to be played, but you might be able to use the below instructions to fudge your way through getting it running.

## dev setup

1. install SBCL
2. install quicklisp
3. ln -s ~/dev/battlefront ~/quicklisp/local-projects/battlefront
4. "M-x slime" in the project dir
5. (ql:quickload :battlefront)
6. (in-package :battlefront)
7. (main)

