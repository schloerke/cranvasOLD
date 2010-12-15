#!/usr/bin/env Rscript

## deal with LyX filename mangling
x = readLines('cranvas-development.tex')
idx = grep('\\\\documentclass', x)
if (idx > 1) x = x[-(1:(idx-1))]
idx = grep('\\\\bibliography\\{', x)
x[idx] = sub('\\{.*cranvas-development._inst_doc_', '{', x[idx])
writeLines(x, 'cranvas-development.tex')
file.rename('cranvas-development.tex', 'cranvas-development.Rnw')
## now we can cheat Sweave :-)
