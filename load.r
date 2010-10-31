require(productplots)
require(qtpaint)
require(reshape)
require(plyr)
require(plumbr)

FILE <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
PATH <- dirname(FILE)
browser()
lapply(dir(file.path(PATH, "R"), pattern = "\\.(R|r)$", full.name=T), source)
