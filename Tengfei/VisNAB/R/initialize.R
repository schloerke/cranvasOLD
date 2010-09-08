## initialize all the variables and data we are going to use globaly.
gInitialize <- function(){
  zoomStarts<<-NULL
  zoomEnds<<-NULL
  l.wide.state <<-FALSE
  id.last<<--1
  isinside<<-FALSE
  pos.hover<<-NULL
  chromIn<<-NULL
  isshift<<-FALSE
  hotden<<-NULL
  isPressed<<-FALSE
  isHotPressed<<-FALSE
hotRegionStarts<<-NULL
scatterObj<<-diagScatter(mysum9,xy=c('maxs1','maxs2'))
zoomObj <<- genZoomBrowserDb('Mus musculus','chr1')
birdObj <<- genBirdEye('Mus musculus',cytobands=FALSE)

}

## scatterObj<<-diagScatter(mysum9,xy=c('maxs1','maxs2'))
## zoomObj <<- genZoomBrowserDb('Mus musculus','chr1')
## birdObj <<- genBirdEye('Mus musculus',cytobands=FALSE)
## dfsub.df <- as.data.frame(scatterObj@mutaframe)
## dfsub.df$chromosome <- as.character(dfsub.df$chromosome)
## hotRegion <<- apply(dfsub.df,1,chr2loc.cb)
## r11 <<- lapply(r1,function(cov1){
##         cov_r1 <- successiveIRanges(runLength(cov1))
##         cov_y1 <- as.integer(round(runValue(cov1)))
##         peaks1 <- slice(cov1,8)
##         summit_x1 <- viewWhichMaxs(peaks1)
##         summit_y1 <- log(viewMaxs(peaks1)+1)
##         my1 <- max(summit_y1)
##         data.frame(summit_x1=summit_x1,
##                    summit_y1=summit_y1,
##                    my1=my1)
##       })
## r22 <<- lapply(r2,function(cov2){
##         cov_r2 <- successiveIRanges(runLength(cov2))
##         cov_y2 <- as.integer(round(runValue(cov2)))
##         peaks2 <- slice(cov2,8)
##         summit_x2 <- viewWhichMaxs(peaks2)
##         summit_y2 <- log(viewMaxs(peaks2)+1)
##         my2 <- max(summit_y2)
##        data.frame(summit_x2=summit_x2,
##                    summit_y2=summit_y2,
##                    my1=my2)
## })

  ## sorted for the following use
  ## maxpos <- apply(chipseq,1,function(x){
##     if(x['isisland1']=='1'){
##       maxpos <- as.numeric(x['maxspos1'])
##     }else{
##       maxpos <- as.numeric(x['maxspos2'])
##     }
##     return(maxpos)
##   })
##   chipseq$maxpos <- maxpos
##   chipseq <- sparseby(chipseq,list(x=chipseq$chromosome),function(x){
##         x <- x[order(x$maxpos,decreasing=FALSE),]
##    })
## }
  ## browser()
  ## test <- rbind(by(chipseq,chipseq$chromosome,function(x){
  ##   x <- as.data.frame(x[order(x$maxpos,decreasing=FALSE),])
  ## }))
 ##  pframe<<-as.mutaframe(chipseq)
## ##  rownames(pframe)<<-pframe$id
##   gpars<<-list()
##   gpars$scale<<-400
##   gpars$width<<-12
##   gpars$skip.factor<<-25
##   gpars$mar<<-c(50,50,40,20)
##   gpars$chrText.width<<-40
##   gpars$lower<<-c(40,40)
##   gpars$mar.scatter<<-c(50,50,40,20)
##   gpars$xy<<-c('maxs1','maxs2')
##   mx <- max(unlist(lapply(cytoband,function(x){max(x$end)})))
##   gpars$mx <<- mx
##   txobj <- loadFeatures('~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/data/mmus.sqlite')
##   chrnames <- names(spe.lst)
##   temp <- as.list(chrnames)
##   names(temp) <- chrnames
##   objExonLst <<- lapply(temp,function(chr){
##   chr <- substr(chr,4,nchar(chr))
##   objExons <-  exons(txobj,vals=list(exon_chrom=chr))
##   })
##   objTxLst <<- lapply(temp,function(chr){
##   chr <- substr(chr,4,nchar(chr))
##   objTx <- transcripts(txobj,vals=list(tx_chrom=chr))
##   objTx
##   })
##   load('~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/data/cytobands.rda')
## cytobands<<-cytobands
## birdObj <<- genBirdEye('Mus musculus',cytobands=FALSE)
## chromRangeDf <<- chromRange(cytoband)  
## zoomObj <<- genZoomBrowserDb('Mus musculus','chr1')
##  denlst <<- getDenLst(mysum8)
  ## scatterObj <<- diagScatter(pframe,c('maxs1','maxs2'))
## need to make a chromRange first
## input object is overview returned object and cytoband list
## need to return a data frame

  
chromRange <- function(cytoband){
  ## get parameters to translate location
  cytoband <- orderChr(cytoband)
  chrTotalName <- names(cytoband)
  chrText.width <- getPar(birdObj,'chrText.width')
  scale <- getPar(birdObj,'scale')
  mx <- getPar(birdObj,'mx')
  mar <- getPar(birdObj,'mar')
  width <- getPar(birdObj,'width')
  skip.factor <- getPar(birdObj,'skip.factor')
  total <- pframe
  chrnames <- names(total)
  lst <- lapply(cytoband,function(chr){
    lst <- by(chr,chr$arm,function(x){
      start <- min(x$start)
      end <- max(x$end)
      arm <- unique(x$arm)
      chr <- unique(x$chr)
      ## cat(length(start),length(end),length(arm),length(chr))
      df <- data.frame(chrom=chr,start=start,end=end,arm=arm,stringsAsFactors=FALSE)
    })
    lst <- do.call('rbind',lst)
  })
  df <- do.call('rbind',lst)
  df$arm <- as.character(df$arm)
  df$topLeftX <- apply(df,1,function(x){
    x <- as.data.frame(t(x))
    if(x['arm']=='p'){
      start.loc <- as.numeric(as.character(x$start))/mx*scale+mar[2]+chrText.width
    }else{
      start.loc <- as.numeric(as.character(x$start))/mx*scale+mar[2]+chrText.width+2*width
    }
    start.loc
  })
  df$bottomRightX <- apply(df,1,function(x){
    x <- as.data.frame(t(x))
    if(x['arm']=='p'){
      end.loc <- as.numeric(as.character(x$end))/mx*scale+mar[2]+chrText.width
    }else{
      end.loc <- as.numeric(as.character(x$end))/mx*scale+mar[2]+chrText.width+2*width
    }
    end.loc
  })
  df$topLeftY <-apply(df,1,function(x){
    x <- as.data.frame(t(x))
    idx <- which(chrTotalName==x$chr)
    idx*skip.factor+mar[3]
  })
  df$bottomRightY <- df$topLeftY+width
  df
}



