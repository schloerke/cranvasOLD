library(qtpaint)
library(IRanges)
options(warn=0)
## options(error=recover)

sourceDir <- function(path, trace = TRUE, ...) {
         for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
            if(trace) cat(nm,":")           
            source(file.path(path, nm), ...)
            if(trace) cat("\n")
         }
      }

sourceDir('~/Prolang/svn/repos/trunk/eos/pkg/eos/R')
load('~/Prolang/svn/repos/trunk/eos/pkg/eos/data/cytobands.rda')
obj <- cytobands[[1]]
lst <- by(obj,obj$chr,function(x) data.frame(chr=unique(x$chr),start=min(x$start),end=max(x$end)))

mydf <- do.call('rbind',lst)

## ord <- apply(mydf,1,function(x){
##   temp <- substr(x[['chr']],4,nchar(x[['chr']]))
##   if(temp !%in% c('X','Y')){
##     temp <- as.numeric(temp)
##   }
## })


ir <- IRanges(start=mydf$start,end=mydf$end)
ird <- RangedData(ir,space=mydf$chr)
idx <- order(width(ird))
ird <- ird[rev(idx)]

.TYPES <- c('sector','segment','text')
width(ird)

e1 <- EOSTrack(ird,type='sector')
e2 <- EOSTrack(ird,type='segment')
e3 <- EOSTrack(ird,type='text')
eosview <- EOSView(list(e2,e1,e3),scale=max(end(ird)))


plot(eosview)



