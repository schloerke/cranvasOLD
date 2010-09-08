##=================================================================##
##                Define all the methods for 'BirdEyeObj'
##=================================================================##

##-----------------------------------------------------------------##
##                  FOR class 'BirdEyeObj'
##-----------------------------------------------------------------##
genBirdEye <- function(species,...){
  ## load('~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/data/cytobands.rda')
  spe <- cytobands[[species]]
  mydf <- spe
  mx <- max(spe$end)
  obj <- new('BirdEyeObj',species=as.character(species),chr=as.character(mydf$chr),
      start=as.numeric(as.character(mydf$start)),
      end=as.numeric(as.character(mydf$end)),
      stain=as.character(mydf$stain),arm=as.character(mydf$arm),pars=new.env(),...)
  obj@pars$mx <- mx
  obj@pars$width <- 12
  obj@pars$scale <- 400
  obj@pars$mar <- c(50,50,40,20)
  obj@pars$skip.factor <- 25
  obj@pars$bg.col <- 'white'
  obj@pars$chrText.width <- 40
  obj@pars$bg.alpha <- 1
  obj
}


setGeneric('gplot',function(obj,...) standardGeneric('gplot'))
setMethod('gplot','BirdEyeObj',function(obj,interaction=FALSE,...){
  df <- data.frame(chr=obj@chr,start=obj@start,end=obj@end,stain=obj@stain,arm=obj@arm)
  object <- split(df,df$chr)
  object <- orderChr(object)
  gdrawChromNoCytoband <- function(layer,painter){
    lst <- split(chromRangeDf,chromRangeDf$chrom)
    lst <- orderChr(lst)
    chrnames <- names(lst)
    for(i in chrnames){
      df <- lst[[i]]
      topleftx <- df$topLeftX
      bottomrightx <- df$bottomRightX
      toplefty <- df$topLeftY
      bottomrighty <- df$bottomRightY
      wid <- bottomrighty[1]-toplefty[1]
      qdrawText(painter,i,mar[2]+chrText.width,toplefty[1]-wid,halign='right',valign='center')
      qdrawRect(painter,topleftx,toplefty,bottomrightx,bottomrighty,fill=NA,stroke='black')
      ## draw conjuntion
      ## draw two triangle and filled with dark red
      x1 <- topleftx[1]
      x0 <- x1-2*width
      x <- x1-width
      ## x0 <- mar[2]+chrText.width
      ## x <- x0+width
      ## x1 <- x0+2*width
      y0 <- toplefty[1]
      y <- y0+width/2
      y1 <- y0+width
      ## if('p' %in% unique(lst[[i]]$arm)){
      ##   qdrawPolygon(painter,c(x0,x,x0),c(y0,y,y1),fill='darkred')
      ## }
      qdrawPolygon(painter,c(x,x1,x1),c(y,y0,y1),fill='darkred')
    }
  }
  gdrawChrom <- function(layer,painter){
    stain <- unique(object[[1]]$stain)
    color <- paste('gray',round((1:length(unique(stain)))/length(unique(stain))*100),sep='')
    names(color) <- unique(stain)
    for(i in 1:length(object)){
      ## plus 20 cause leave black for text
      object[[i]]$pos.start <- object[[i]]$start/mx*scale+mar[2]+chrText.width
      object[[i]]$pos.end <- object[[i]]$end/mx*scale+mar[2]+chrText.width
      object[[i]][object[[i]]$arm=='q',]$pos.start <- object[[i]][object[[i]]$arm=='q',]$pos.start+width*2
      object[[i]][object[[i]]$arm=='q',]$pos.end <- object[[i]][object[[i]]$arm=='q',]$pos.end+width*2
      ## think a way to replace this stupid method
      ## assign('l',object,env)
      yaxis <- i*skip.factor+mar[3]
      if(!('p' %in% unique(object[[i]]$arm))){
        head(object[[i]])
        object[[i]]$arm
        x0 <- min(object[[i]][object[[i]]$arm=='q',]$pos.start)-width*2
      }else{
        x0 <- tail(object[[i]][object[[i]]$arm=='p',]$pos.end,1)
      }
      y0 <- yaxis
      x1 <- x0+width*2
      y1 <- yaxis+width
      qdrawText(painter,names(object)[i],mar[2],y0-width,'left','center')
      apply(object[[i]],1,function(chrom){
        x1 <- as.numeric(chrom['pos.start'])
        x2 <- as.numeric(chrom['pos.end'])
        band <- chrom['band']
        stain <- chrom['stain']
        qdrawRect(painter,x1,yaxis,x2,yaxis+width,fill=color[stain],stroke='black')
        ## chromosome name
        if(FALSE){
          qdrawText(painter,band,x1+(x2-x1)/2,yaxis,halign='center',valign='top')}
      })
      ## two qdrawPolygon to plot the junction plot
      x <- (x0+x1)/2
      y <- (y0+y1)/2
      ## draw two triangle and filled with dark red
      if('p' %in% unique(object[[i]]$arm)){
        qdrawPolygon(painter,c(x0,x,x0),c(y0,y,y1),fill='darkred')
      }
      qdrawPolygon(painter,c(x,x1,x1),c(y,y0,y1),fill='darkred')
    }
    ## add title here
    ## if(!is.null(title)){
    ##   qdrawText(painter,title,(scale+chrText.width/2+mar[2]*2)/2,mar[3]/2,'center','center')
    ## }
    ## draw legend here
    if(FALSE){
      ## compute legend position
      legend.x <- scale+chrText.width+mar[2]+width*2+50
      legend.y <- mar[3]*3
      k <- 0
      for(i in names(color)){
        k <- k+1
        qdrawRect(painter,legend.x,legend.y+k*20,legend.x+width,legend.y+k*20+width,fill=color[i])
        qdrawText(painter,i,legend.x+width+10,legend.y+k*20,'left','top')
      }
    }
  }
  ## initialize parameter
  mx <- max(obj@end)
  scale <- getPar(obj,'scale')
  mar <- getPar(obj,'mar')
  chrText.width <- getPar(obj,'chrText.width')
  skip.factor <- getPar(obj,'skip.factor')
  width <- getPar(obj,'width')
  bg.col <- getPar(obj,'bg.col')
  bg.alpha <- getPar(obj,'bg.alpha')
  bg <- col2rgb(bg.col)
  bgc <- qbrush(qcolor(bg[1,],bg[2,],bg[3,],bg.alpha))
  s.bird <<- qscene()
  bird.root<<-qlayer(s.bird,geometry=qrect(0,0,600,700))
  if(obj@cytobands){
    lb<<-qlayer(bird.root,paintFun=gdrawChrom,cache=TRUE)
  }else{
    lb<<-qlayer(bird.root,paintFun=gdrawChromNoCytoband,cache=TRUE)
  }
    l.line<<-qlayer(bird.root,paintFun=hotLine,hoverMoveFun=mouseHover)
    l.hot <<- qlayer(bird.root,paintFun=hotSpot2,
                     mouseDoubleClickFun=hotDBclick,
                     mousePressFun=mouseRightClick,
                      mouseMoveFun=hotRegionMove,
                      mouseReleaseFun=hotRegionRelease,
                     cache=TRUE)
    l.region<<-qlayer(bird.root,paintFun=hotRegionPaint)
  if(FALSE){
    l.den<<-qlayer(bird.root,paintFun=widerange,cache=FALSE)
  }
  s.bird$setBackgroundBrush(bgc)
  view.bird <<- qplotView(s.bird,rescale='transform')
})

hotRegionPaint <- function(layer,painter){
  if(isHotPressed){
  topY <- chromRangeDf[chromRangeDf$chrom==chromIn,'topLeftY'][1]
  bottomY <- chromRangeDf[chromRangeDf$chrom==chromIn,'bottomRightY'][1]
  chromHotIn<<-chromRangeDf[chromRangeDf$chrom==chromIn,'chrom'][1]
  if(!is.null(hotRegionStarts)){
  qdrawRect(painter,hotRegionStarts,topY,hotRegionEnds,bottomY,fill=rgb(0,0,1,0.5))
}
}
}

hotRegionMove <- function(layer,event){
  if(!isHotPressed){
      isHotPressed<<-TRUE
  }
  hotRegionStarts<<-as.numeric(event$buttonDownPos(Qt$Qt$LeftButton))[1]
  hotRegionEnds<<-as.numeric(event$pos())[1]
  qupdate(l.region)
}

hotRegionRelease <- function(layer,event){
  if(isHotPressed){
  hotRegionLastEnds<<-as.numeric(event$pos())[1]
  mx <- birdObj@pars$mx
  scale <- birdObj@pars$scale
  chrText.width <- birdObj@pars$chrText.width
  mar <- birdObj@pars$mar
  width <- birdObj@pars$width
  rss <- (hotRegionStarts-chrText.width-mar[2]-2*width)/scale*mx
  ree <- (hotRegionLastEnds-chrText.width-mar[2]-2*width)/scale*mx
  if(length(chromIn)>0){
  zoomObj <<- genZoomBrowserDb('Mus musculus',chromIn,
                                region_start=rss,
                                region_end=ree)
   viewZoomTrack$resetTransform()
   qupdate(viewZoomTrack)
   qupdate(viewZoomChrom)
}
}
   isHotPressed<<-FALSE
}

## order chr names
orderChr <- function(object){
  chrnames <- names(object)
  chrnames <- sort(chrnames)
  l <- lapply(chrnames,function(x) {
    s <- substr(x,start=4,stop=nchar(x))
    if(!s%in%c('x','y','X','Y')){
      s.temp <- as.numeric(as.character(s))
    }else{
      s.temp <- s
    }
    return(s.temp)
  })
  l.logic <- unlist(lapply(l,is.character))
  l.number <- l[!l.logic]
  id.char <- seq_along(chrnames)[l.logic]
  id.num <- order(unlist(l.number))
  idx <- c(id.num,id.char)
  new.lst <- object[chrnames[idx]]
  new.lst
}

##----------------------------------------------------------------##
##              Painters
##----------------------------------------------------------------##

hotLine <- function(layer,painter){
  if(!is.null(pos.hover)){
    isinside <- apply(chromRangeDf,1,function(x){
      x <- as.data.frame(t(x),stringsAsFactors=FALSE)
      isinside <- isInside(pos.hover,as.numeric(c(x$topLeftX,x$topLeftY)),as.numeric(c(x$bottomRightX,x$bottomRightY)))
      isinside
    })
    chromIn<<-as.character(chromRangeDf$chrom)[isinside]
    isinside <- any(isinside)
  }
  if(isinside){
    chrText.width <- birdObj@pars$chrText.width
     mar <- birdObj@pars$mar
    scale <- birdObj@pars$scale
    mx <- birdObj@pars$mx
    width <- birdObj@pars$width
    topY <- chromRangeDf[chromRangeDf$chrom==chromIn,'topLeftY'][1]
    bottomY <- chromRangeDf[chromRangeDf$chrom==chromIn,'bottomRightY'][1]
    arm <- chromRangeDf[chromRangeDf$chrom==chromIn,'arm'][1]
    qdrawSegment(painter,pos.hover[1],topY,pos.hover[1],bottomY,stroke='red')
   ## show chromosome coordinates
    if(arm=='p'){
      coord <- (pos.hover[1]-chrText.width-mar[2])/scale*mx
    }else{
      coord <- (pos.hover[1]-chrText.width-mar[2]-width*2)/scale*mx
    }
    coord <- round(coord)
       qdrawText(painter,coord,pos.hover[1],topY+8,'center','bottom')
   }
}

isInside <- function(pos,rec1,rec2,...){
  x.max <- max(rec1[1],rec2[1])
  x.min <- min(rec1[1],rec2[1])
  y.max <- max(rec1[2],rec2[2])
  y.min <- min(rec1[2],rec2[2])
  isInside <- pos[1]<x.max&pos[1]>x.min&pos[2]<y.max&pos[2]>y.min
  isInside
}


hotSpot2 <- function(layer,painter){
  pframe <- scatterObj@mutaframe
  ## dfsub <- pframe[pframe$isSelected,]
  ## dfsub.df <- as.data.frame(dfsub)
  ## if(nrow(dfsub.df)>0){
  if(any(pframe$isSelected)){
    idx <- pframe$isSelected
    hotregion<<-hotRegion[,idx,drop=FALSE]
    mapTopLeftX <- hotregion[1,,drop=FALSE]
    mapBottomRightX <- hotregion[3,,drop=FALSE]
    mapTopLeftY <- hotregion[2,,drop=FALSE]
    mapBottomRightY <- hotregion[4,,drop=FALSE]
    width <- getPar(birdObj,'width')
    if(isshift){
      qdrawRect(painter,mapTopLeftX,mapTopLeftY-width,mapBottomRightX,mapBottomRightY-width,stroke='red')
    }else{
      qdrawRect(painter,mapTopLeftX,mapTopLeftY,mapBottomRightX,mapBottomRightY,stroke='red')
    }
  }
}

chr2loc.cb <- function(x){
  chrom <- as.character(x['chromosome'])
  start <- as.numeric(x['start'])
  end <- as.numeric(x['end'])
  arm <- chromRangeDf[chromRangeDf$chrom==chrom,'arm'][1]
  chrText.width <- getPar(birdObj,'chrText.width')
  scale <- getPar(birdObj,'scale')
  mx <- getPar(birdObj,'mx')
  mar <- getPar(birdObj,'mar')
  width <- getPar(birdObj,'width')
  if(arm=='p'){
    start <- start*scale/mx+chrText.width+mar[2]
    end <- end*scale/mx+chrText.width+mar[2]
  }else{
    start <- start*scale/mx+chrText.width+mar[2]+2*width  
    end <- end*scale/mx+chrText.width+mar[2]+2*width  
  }
  ystart <- chromRangeDf[chromRangeDf$chrom==chrom,'topLeftY'][1]
  yend <- chromRangeDf[chromRangeDf$chrom==chrom,'bottomRightY'][1]
  c(start,ystart,end,yend)
}


## for wideRange layer
wideRange <- function(layer,painter){
  df <- as.data.frame(pframe)
  ## need to sort them first
  den.cutoff <- min(as.numeric(as.character(pframe$den[pframe$isSelected])))
  dfsub <- subset(df,df$only)       #leave only interesting region
  dfsub$logics <- as.integer(dfsub$den>den.cutoff)
  widerange <- sparseby(dfsub,dfsub$chromosome,function(x){
    diffs <- diff(c(0,x$logics))
    x$diff <- diffs
    start <- x$start[x$diff==1]
    end <- x$end[which(x$diff==-1)-1]
    if(length(start)==length(end)+1){
      end <- c(max(x$end),end)
    }
    ir <- IRanges(start=start,end=end)
    irr <- IRanges::reduce(ir)
    start <- start(irr)
    end <- end(irr)
    nstart <- length(start)
    data.frame(chromosome=rep(unique(x$chromosome)[1],nstart),start=start,end=end)

   })
  widerange <- widerange[,-1]
  hotrange <- apply(widerange,1,chr2loc.cb)
  mapTopLeftX <- hotrange[1,]
  mapBottomRightX <- hotrange[3,]
  mapTopLeftY <- hotrange[2,]
  mapBottomRightY <- hotrange[4,]
  if(filter.num>1){
      hotden<<-NULL
  }else{
  hotden<<-data.frame(topLeftX=mapTopLeftX,
                      topLeftY=mapTopLeftY,
                      bottomRightX=mapBottomRightX,
                      bottomRightY=mapBottomRightY,
                      chromosome=widerange$chromosome,
                      start=widerange$start,
                      end=widerange$end)
}
}

##----------------------------------------------------------------##
##         Events
##----------------------------------------------------------------##


hotDBclick <- function(layer,event){
  pframe<-scatterObj@mutaframe
  pos <- as.numeric(event$pos())
  if(is.null(hotden)){
    if(any(pframe$isSelected)){
      iscloseto <- isCloseToHot(pos,hotRegion,isshift=isshift)
      if(any(iscloseto)){
        hits <- which(iscloseto)[1]
        chr <- pframe$chromosome[hits]
        zoomObj <<- genZoomBrowserDb('Mus musculus',chr,
                                     region_start=pframe$start[hits],
                                     region_end=pframe$end[hits])
        scatterObj@mutaframe$isHighlighted<<-FALSE
        scatterObj@mutaframe$isHighlighted[hits]<<-TRUE
        scatterObj@mutaframe$isHoverSelected<<-FALSE
        scatterObj@mutaframe$isHoverSelected[hits]<<-TRUE
        viewZoomTrack$resetTransform()
        qupdate(viewZoomTrack)
        qupdate(viewZoomChrom)
        qupdate(layerIsland)
       qupdate(l.red)
      }}
  }else{
    iscloseto.den <- isCloseToHot.den(pos,hotden,isshift=FALSE)
    if(any(iscloseto.den)){
      hits <- which(iscloseto.den)[1]
      chr <- hotden$chromosome[hits]
      zoomObj <<- genZoomBrowserDb('Mus musculus',chr,
                                   region_start=hotden$start[hits],
                                   region_end=hotden$end[hits])
      qupdate(viewZoomTrack)
      qupdate(viewZoomChrom)
      ##    qupdate(layerIsland)
      ##chipSeqPlot.den(hotden,r1,r2,id,lower=gpars$lower)
    }
  }
}



isCloseToHot <- function(pos,hotrg,mar=c(0,5,0,5),isshift=isshift){
  hot <- hotrg
  width <- birdObj@pars$width
  if(!isshift){
    hot[1,] <- hot[1,]-mar[2]
    hot[2,] <- hot[2,]-mar[3]
    hot[3,] <- hot[3,]+mar[4]
    hot[4,] <- hot[4,]+mar[1]
  }else{
    hot[1,] <- hot[1,]-mar[2]
    hot[2,] <- hot[2,]-mar[3]-width
    hot[3,] <- hot[3,]+mar[4]
    hot[4,] <- hot[4,]+mar[1]-width
  }
  isclose <- apply(hot,2,function(x){
    isInside(pos,c(x[1],x[2]),c(x[3],x[4]))
  })
  id <- isclose&pframe$isSelected
  id
}


isCloseToHot.den <- function(pos,hotrg,mar=c(0,0,0,0),isshift=isshift){
  hot <- hotrg
  width <- birdObj@pars$width
  if(!isshift){
    hot[,1] <- hot[,1]-mar[2]
    hot[,2] <- hot[,2]-mar[3]
    hot[,3] <- hot[,3]+mar[4]
    hot[,4] <- hot[,4]+mar[1]
  }else{
    hot[,1] <- hot[,1]-mar[2]
    hot[,2] <- hot[,2]-mar[3]-width
    hot[,3] <- hot[,3]+mar[4]
    hot[,4] <- hot[,4]+mar[1]-width
  }
  isclose <- sparseby(hot,1:nrow(hot),function(x){
    isInside(pos,c(x$topLeftX,x$topLeftY),c(x$bottomRightX,x$bottomRightY))
  })
  id <- isclose[,2]
  id
}

mouseHover <- function(layer,event){
  pos.hover <<- as.numeric(event$pos())
  qupdate(l.line)
}

mouseRightClick <- function(layer,event){
  if(event$buttons()==2){
  chr <- chromIn
  if(length(chromIn)>0){
  zoomObj <<- genZoomBrowserDb('Mus musculus',chr,region_start=NULL,
                                   region_end=NULL)
  viewZoomTrack$resetTransform()
  zoomStarts<<-zoomEnds<<-NULL
  qupdate(sceneZoomTrack)
  qupdate(sceneZoomChrom)
}else{
  message('Outside the chromosome!')
}
}
}


