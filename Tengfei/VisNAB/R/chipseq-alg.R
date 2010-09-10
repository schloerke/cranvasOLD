gChipseqSummary <- function(range1,range2,chromlen,lower=c(20,20),island=TRUE,merge=0,smooth=FALSE,...){
  cov1 <- lapply(range1,findPeaks,island,lower[1],merge)
  cov2 <- lapply(range2,findPeaks,island,lower[2],merge)
  chrnames <- unique(c(names(cov1),names(cov2)))
  ## combine these peak range based on chromosome and make summary
  res <- lapply(chrnames,function(chr){
    message('Processing',' ',chr,'\n')
    if(chr%in%names(cov1)&chr%in%names(cov2)){
      start<- c(start(cov1[[chr]]),start(cov2[[chr]]))
      width <- c(width(cov1[[chr]]),width(cov2[[chr]]))
      cov1[[chr]]@start <- (cov2[[chr]]@start <- start)
      cov1[[chr]]@width <- (cov2[[chr]]@width <- width)
    }else{
      if(chr%in%names(cov1)){
        start <- as.numeric(cov1[[chr]]@start)
        width <- as.numeric(cov1[[chr]]@width)
      }else{
        start <- as.numeric(cov2[[chr]]@start)
        width <- as.numeric(cov2[[chr]]@width)
      }
    }
    n <- length(start)
    ## do not change characters to factors
    res <- data.frame(chromosome=rep(chr,n), start=start,end=start+width,stringsAsFactors=FALSE)
    if(chr%in%names(cov1)){
      res$sums1 <- viewSums(cov1[[chr]])
      res$maxs1 <- viewMaxs(cov1[[chr]])
      res$maxspos1 <- viewWhichMaxs(cov1[[chr]])
      res$isisland1 <- as.numeric(res$maxs1>lower[1])
      if(smooth){
        res$smoothmaxs1 <- sapply(cov1[[chr]],function(x){
          raw <- as.numeric(x)
          raw <- viewSmooth(raw,'max')
        })
        res$smoothsums1 <- sapply(cov1[[chr]],function(x){
          raw <- as.numeric(x)
          raw <- viewSmooth(raw,'sum')
        })
      }
    }
    if(chr%in%names(cov2)){
      res$sums2 <- viewSums(cov2[[chr]])
      res$maxs2 <- viewMaxs(cov2[[chr]])
      res$maxspos2 <- viewWhichMaxs(cov1[[chr]])
      res$isisland2 <- as.numeric(res$maxs2>lower[2])
      if(smooth){
        res$smoothmaxs2 <- sapply(cov2[[chr]],function(x){
          raw <- as.numeric(x)
          raw <- viewSmooth(raw,'max')
        })
        res$smoothsums2 <- sapply(cov2[[chr]],function(x){
          raw <- as.numeric(x)
          raw <- viewSmooth(raw,'sum')
        })
      }
      ## res$densitymaxs2 <- viewDensityMax(cov2[[chr]])
    }
    chromLen <- max(chromRangeDf[chromRangeDf$chrom==chr,'end'])
    res$asums1 <- asinh(res$sums1)
    res$asums2 <- asinh(res$sums2)
    res$only <- as.logical(abs(res$isisland1-res$isisland2)==1)
    res$onlynum <- as.numeric(res$only)
    gp <- c(res$maxspos1[res$only],res$maxspos2[res$only])
    den <- ash1(bin1(gp,c(1,chromLen),chromLen/100),m=5e3)
    df <- data.frame(x=den$x,y=den$y)
    big <- as.big.matrix(df)
    denvalue <- sparseby(res,1:nrow(res),function(x){
      if(x$only){
        if(x$isisland1==1){
          maxpos <- x$maxspos1
        }else{
          maxpos <- x$maxspos2
        }
    # idx <- order(abs(den$x-maxpos),decreasing=FALSE)[1:2]
        idx <- mwhich(big,cols=1,val=maxpos,comps='gt')[1]
        return(mean(den$y[c(idx,idx-1)]))
      }else{
        return(0)
      }
    })
    res$den <- denvalue[,2]
    res
  })
  res <- do.call('rbind',res)
  res
}          


######################################################################
## smoother alg
######################################################################
difsmw <- function(y, lambda=1e6, w=NULL, d=2){
  ## Weighted smoothing with a finite difference penalty
  ## y:      signal to be smoothed
  ## lambda: smoothing parameter
  ## w:      weights (use0 zeros for missing values)
  ## d:      order of differences in penalty (generalaly 2)
  ## Paul Eilers, 2002, ported by Nicholas Lewin-Koh 2004, 2008
  require(Matrix)
  m <- length(y)
  if(is.null(w)){
    W <- Diagonal(m)
    w <- 1
  }
  else W <- Diagonal(x=w)
  D <- crossprod(diff(Diagonal(m),differences=d))
  B <- W + (lambda * D)
  z <- solve(B,w*y)
  as.vector(z)
}

######################################################################
## find peaks algorithm
######################################################################
## finding peaks for chipseq
findPeaks <- function(x,island,lower,merge) {
  if (island) {
    s <- slice(x, lower = 1)
    peaks <- s[viewMaxs(s) >= lower]
  }
  else peaks <- slice(x, lower = lower)
  if (merge > 0) {
    end(peaks) <- end(peaks) + merge
    peaks <- reduce(peaks)
    end(peaks) <- end(peaks) - merge
  }
  peaks
}

viewSmooth <- function(iranges,stat=c('max','sum'),method=c('difsmw')){
  ir <- as.vector(iranges)
  res <- switch(method,
                difsmw <- difsmw.fun(stat,ir)
                )
  res
}

difsmw.fun <- function(stat,ir){
  if(length(ir)>2){
    ir.new <- difsmw(ir)
    statres <- switch(stat,
                      max <- max(ir.new),
                      sum <- sum(ir.new)
                      )
  }else{
    ir.new <- ir
    statres <- switch(stat,
                      max <- max(ir.new),
                      sum <- sum(ir.new)
                      )
  }
  statres
}


viewDensityMax <- function(iranges,chrom){
  res <- NULL
  for(i in 1:length(iranges)){
    ir <- as.vector(iranges[[i]])
    start <- start(iranges)[i]
    end <- end(iranges)[i]
    dd <- rep(start:end,each=ir)
    if(length(dd)>2){
      den <- density(dd,cut=0,)
      x <- den$x
      y <- den$y
      fit <- lm(y~x)
      pred <- predict(fit,data.frame(x=start:end))
      res <- c(max(pred),res)
    }else{res<-c(0,res)}
  }
  res
}


getDenLst <- function(summary){
  lst <- split(summary,summary$chromosome)
  res <- lapply(lst,function(res){
    chr <- unique(res$chromosome)
    chromLen <- max(chromRangeDf[chromRangeDf$chrom==chr,'end'])
    gp <- c(res$maxspos1[res$only],res$maxspos2[res$only])
    den <- ash1(bin1(gp,c(1,chromLen),chromLen/100),m=1e5)
    ##    plot(den,type='l')
    ##    browser()
    den
  })
}


