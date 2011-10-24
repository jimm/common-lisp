#!/bin/bash

cd `dirname $0`
rm -rf arc
cp -r /usr/local/src/Lisp/arc .
cp -r *.arc arc
echo Type '"(fake)"' to start fake
cd arc && rlwrap mzscheme -m -f as.scm
