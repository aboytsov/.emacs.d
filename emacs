#!/bin/bash
open -a emacs --args -q --load ~/.emacs.d/init-kinesis.el ~/stuph/src/clj/stuph/repl.clj $@
