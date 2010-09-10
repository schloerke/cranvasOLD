##-----------------------------------------------------------------##
##             For class 'ZoomableViewDb' 
##-----------------------------------------------------------------##
genZoomBrowserDb <- function(species,chr,...){
  spe <- cytobands[[species]]
  spe.lst <- split(spe,spe$chr)
  mydf <- spe.lst[[chr]]
  obj <- new('ZoomableViewDb',species=as.character(species),chr=as.character(chr),
             start=as.numeric(as.character(mydf$start)),
             end=as.numeric(as.character(mydf$end)),
             stain=as.character(mydf$stain),arm=as.character(mydf$arm),pars=new.env(),...)
  obj@pars$width <- 12
  obj@pars$scale <- 400
  obj@pars$mar <- c(40,40,40,40)
  obj@pars$skip.factor <- 20
  obj@pars$bg.col <- 'white'
  obj@pars$chrText.width <- 0
  obj@pars$bg.alpha <- 1
  obj@pars$name.width <- 50
  obj@pars$col.exon <- rgb(1,0,0,1)
  obj@pars$col.transcript <- rgb(0,1,0,1)
  obj
}


setMethod('gplot','ZoomableViewDb',function(obj,...){
  ## wheel function
  wheelZoom <- function(layer, event) {
    zoom_factor <- 2
    if(!inherits(try(event$delta()<0,TRUE),'try-error')){
    if (event$delta() < 0)
      zoom_factor <- 0.5
     tform <- viewZoomTrack$transform()
     tform$scale(zoom_factor,1)
     viewZoomTrack$setTransform(tform)
   ##viewZoomTrack$scale(zoom_factor,1)
  }else{
    message('Unsolved Error here..')
  }
  }
  gDrawSingleChrom<- function(layer,painter){
    obj <- zoomObj
    chrom <- unique(obj@chr)
    mar <- obj@pars$mar
    chrText.width <- obj@pars$chrText.width
    name.width <- obj@pars$name.width
    width <- obj@pars$width
    scale <- obj@pars$scale
    stain <- unique(obj@stain)
    color <- paste('gray',round((1:length(unique(stain)))/length(unique(stain))*100),sep='')
    names(color) <- unique(stain)
    ## plus 20 cause leave black for text
    mx <- max(obj@end)
    pos.start <- obj@start/mx*scale+mar[2]+name.width+chrText.width
    pos.end <- obj@end/mx*scale+mar[2]+name.width+chrText.width
    pos.start[obj@arm=='q'] <- pos.start[obj@arm=='q']+width*2
    pos.end[obj@arm=='q'] <- pos.end[obj@arm=='q']+width*2
    yaxis <- width/2
    if(!('p' %in% unique(obj@arm))){
      x0 <- head(pos.start[obj@arm=='q'],1)-width*2
    }else{
      x0 <- tail(pos.end[obj@arm=='p'],1)
    }
    y0 <- yaxis
    x1 <- x0+width*2
    y1 <- yaxis+width
    ##  qdrawText(painter,chrom,mar[2],y0-width,'left','center')
    mydf <- data.frame(pos.start=pos.start,pos.end=pos.end,stain=obj@stain)
    apply(mydf,1,function(chrom){
      x1 <- as.numeric(chrom['pos.start'])
      x2 <- as.numeric(chrom['pos.end'])
      ## band <- chrom['band']
      stain <- as.character(chrom['stain'])
      qdrawRect(painter,x1,yaxis,x2,yaxis+width,fill=color[stain],stroke='black')
    })
    ## two qdrawPolygon to plot the junction plot
    x <- (x0+x1)/2
    y <- (y0+y1)/2
    ## draw two triangle and filled with dark red
    if('p' %in% unique(obj@arm)){
      qdrawPolygon(painter,c(x0,x,x0),c(y0,y,y1),fill='darkred')
    }
    qstrokeColor(painter) <- 'black'
    qdrawText(painter,obj@chr,name.width,yaxis+width/2,'right','center')
    qdrawPolygon(painter,c(x,x1,x1),c(y,y0,y1),fill='darkred')
    ## draw hot region
    starts <- zoomObj@region_start
    ends <- zoomObj@region_end
    poscenter <- min(pos.start)+(max(pos.end)-min(pos.start))/2
    if(!(is.null(starts))&(!is.null(ends))){
      if(is.null(zoomStarts)){
      hotstart <- starts/mx*scale+mar[2]+name.width+chrText.width+2*width
      hotend <- ends/mx*scale+mar[2]+name.width+chrText.width+2*width
      qdrawRect(painter,hotstart,yaxis-5,hotend,yaxis+width+5,stroke='red',
                fill=NA)
      selectwidth <- abs(starts-ends)
      qdrawSegment(painter,c(hotstart,hotend),yaxis-5,
                   c(0,550),-50,stroke='red')
      qdrawText(painter,paste('From:',round(starts,0),
                              'To:',round(ends,0),
                              'Width:',round(selectwidth,0),'bp'),
                poscenter,
                yaxis+20,'center','bottom')
    }else{
      zoomStarts2 <<- zoomStarts/mx*scale+mar[2]+name.width+chrText.width+2*width
          zoomEnds2 <<- zoomEnds/mx*scale+mar[2]+name.width+chrText.width+2*width
          selectwidth <- zoomEnds-zoomStarts
          qdrawRect(painter,zoomStarts2,yaxis-5,zoomEnds2,yaxis+width+5,stroke='red',fill=NA)
          qdrawSegment(painter,c(zoomStarts2,zoomEnds2),yaxis-5,
                       c(0,550),-50,stroke='red')
          qdrawText(painter,paste('From:',round(zoomStarts,0),
                                  'To:',round(zoomEnds,0),
                                  'Width:',round(selectwidth,0),'bp'),
                    poscenter,
                    yaxis+20,'center','bottom')
    }
    }else{
      if(is.null(zoomStarts)){
          starts <- min(pos.start)
          ends <- max(pos.end)
          qdrawRect(painter,starts,yaxis-5,ends,yaxis+width+5,stroke='red',fill=NA)
          qdrawSegment(painter,c(starts,ends),yaxis-5,
                       c(0,550),-50,stroke='red')
          qdrawText(painter,paste('From:',min(zoomObj@start),
                                  'To:',max(zoomObj@end),
                                  'Width:',max(zoomObj@end)-min(zoomObj@start),'bp'),
                    poscenter,
                    yaxis+20,'center','bottom')
 
         }else{
          zoomStarts2 <<- zoomStarts/mx*scale+mar[2]+name.width+chrText.width+2*width
          zoomEnds2 <<- zoomEnds/mx*scale+mar[2]+name.width+chrText.width+2*width
          selectwidth <- zoomEnds-zoomStarts
          qdrawRect(painter,zoomStarts2,yaxis-5,zoomEnds2,yaxis+width+5,stroke='red',fill=NA)
          qdrawSegment(painter,c(zoomStarts2,zoomEnds2),yaxis-5,
                       c(0,550),-50,stroke='red')
          qdrawText(painter,paste('From:',round(zoomStarts,0),
                                  'To:',round(zoomEnds,0),
                                  'Width:',round(selectwidth,0),'bp'),
                    poscenter,
                    yaxis+20,'center','bottom')
         }
    }
  }


  scaleTrack <- function(layer,painter){
  if(is.null(zoomObj@region_start)|is.null(zoomObj@region_end)){
    if(is.null(zoomStarts)){
      xlim <-c(min(zoomObj@start),max(zoomObj@end))
    }else{
      xlim <- c(zoomStarts,zoomEnds)
    }
    }else{
      xlim <- c(zoomObj@region_start,zoomObj@region_end)
    }
    mapscale <- round(1/3*diff(xlim),0)
    mapscale <- as.numeric(substr(mapscale,1,1))*10^(nchar(mapscale)-1)
    maplen <- mapscale/diff(xlim)*20
    mapstart <- 10-maplen/2
    mapend <- 10+maplen/2
    qdrawSegment(painter,mapstart,2.5,mapend,2.5)
    qdrawSegment(painter,c(mapstart,mapend),2,c(mapstart,mapend),3)
    qdrawText(painter,mapscale,mapstart-3,2.5,'right','center')
  }

  covPaintFun <- function(layer,painter,exposed){
##    qupdate(layerZoomScale)
    chr <- zoomObj@chr
    cov1 <- r1[[chr]]
    cov2 <- r2[[chr]]
    if(!is.null(cov1)&!is.null(cov2)){
      getIr <- function(covrange,chr,starts,ends){
        test <- covrange[[chr]]
        res <-  as.vector(seqselect(test,starts,ends))
      }
      if(is.null(zoomObj@region_start)|is.null(zoomObj@region_end)){
        xlim <- c(min(zoomObj@start),max(zoomObj@end))
        summit_x1 <- r11[[chr]]$summit_x1
        summit_x2 <- r22[[chr]]$summit_x2
        summit_y1 <- r11[[chr]]$summit_y1
        summit_y2 <- r22[[chr]]$summit_y2
        mxy <- max(summit_y1,summit_y2)
        col.ir1 <- rgb(1,1-summit_y1/mxy,0)
        col.ir2 <- rgb(1,1-summit_y2/mxy,0)
        xlimZoom <- c(zoomStarts,zoomEnds)
          if(diff(xlimZoom)>100000){
            layerZoomTrackCov$setLimits(qrect(xlim,c(-10,80)))
            qdrawSegment(painter,0,15,xlim[2],15,stroke='gray')
            qdrawSegment(painter,0,55,xlim[2],55,stroke='gray')
             qstrokeColor(painter) <- 'red'
                        qdrawText(painter,'Coverage Score for Pax5-NULL',
                      sum(xlimZoom)/2,30,'center','bottom')
            qdrawText(painter,'Coverage Score for Rag2-NULL',
                   sum(xlimZoom)/2,70,'center','bottom')
            qdrawSegment(painter,summit_x1,0,summit_x1,30,stroke=col.ir1)
            qdrawSegment(painter,summit_x2,40,summit_x2,70,stroke=col.ir2)

          }else{
            starts <- round(xlimZoom[1],0)
            ends <- round(xlimZoom[2],0)
            ir1 <- log(getIr(r1,chr,starts,ends)+1)
            ir2 <- log(getIr(r2,chr,starts,ends)+1)
            ## mxr <- max(ir1,ir2)
            ir1 <- ir1/mxy*35
            ir2 <- ir2/mxy*35
            mxr <- 35
            idx <- starts:ends
            col.ir1 <- rgb(1,1-ir1/mxr/35,0)
            col.ir2 <- rgb(1,1-ir2/mxr/35,0)
            qdrawSegment(painter,idx,0,idx,ir1)
            qdrawSegment(painter,idx,mxr*1.1,idx,mxr*1.1+ir2)
            qstrokeColor(painter) <- 'black'
            qdrawText(painter,'Coverage Score for Pax5-NULL',
                      sum(xlimZoom)/2,max(ir1)+2,'center','bottom')
            qdrawText(painter,'Coverage Score for Rag2-NULL',
                   sum(xlimZoom)/2,mxr*1.1+max(ir2)+2,'center','bottom')
            qdrawSegment(painter,starts,0,ends,0,stroke='gray')
            qdrawSegment(painter,starts,mxr*1.1,ends,mxr*1.1,stroke='gray')
##            layerZoomTrackCov$setLimits(qrect(xlim,c(-10,mxr*2.2)))
          }
        ## }else{
        ##   zoomStarts<<-NULL
        ##   zoomEnds<<-NULL
        ## }
      }else{
        ## if region_start and region_end are not NULL
        starts <- zoomObj@region_start
        ends <- zoomObj@region_end
        xlim <- c(starts,ends)
        if(diff(xlim)<5000){
        ir1 <- getIr(r1,chr,starts,ends)
        ir2 <- getIr(r2,chr,starts,ends)
        mxr <- max(ir1,ir2)
        idx <- starts:ends
        qdrawRect(painter,idx,0,idx+1,ir1,fill='black',stroke=NA)
        qdrawRect(painter,idx,mxr+15,idx+1,mxr+15+ir2,fill='black',stroke=NA)
        qstrokeColor(painter) <- 'black'
        qdrawText(painter,'Coverage Score for Pax5-NULL',
                      sum(xlim)/2,mxr+1,'center','bottom')
        qdrawText(painter,'Coverage Score for Rag2-NULL',
                   sum(xlim)/2,mxr*2+18,'center','bottom')
        qdrawSegment(painter,starts,0,ends,0,stroke='gray')
        qdrawSegment(painter,starts,mxr+15,ends,mxr+15,stroke='gray')
        layerZoomTrackCov$setLimits(qrect(xlim,c(-10,mxr*2+30)))
      }else{
        ## if width>100000
        summit_x1 <- r11[[chr]]$summit_x1
        summit_x2 <- r22[[chr]]$summit_x2
        summit_y1 <- r11[[chr]]$summit_y1
        summit_y2 <- r22[[chr]]$summit_y2
        idx1 <- summit_x1<ends&summit_x1>starts
        idx2 <- summit_x2<ends&summit_x2>starts
        summit_x1 <- summit_x1[idx1]
        summit_x2 <- summit_x2[idx2]
        summit_y1 <- summit_y1[idx1]
        summit_y2 <- summit_y2[idx2]
        mxy <- max(summit_y1,summit_y2)
        col.ir1 <- rgb(1,1-summit_y1/mxy,0)
        col.ir2 <- rgb(1,1-summit_y2/mxy,0)
        xlimZoom <- c(starts,ends)
            qdrawSegment(painter,0,15,xlim[2],15,stroke='gray')
            qdrawSegment(painter,0,55,xlim[2],55,stroke='gray')
            qstrokeColor(painter) <- 'red'
                        qdrawText(painter,'Coverage Score for Pax5-NULL',
                      sum(xlimZoom)/2,30,'center','bottom')
            qdrawText(painter,'Coverage Score for Rag2-NULL',
                   sum(xlimZoom)/2,70,'center','bottom')
            qdrawSegment(painter,summit_x1,0,summit_x1,30,stroke=col.ir1)
            qdrawSegment(painter,summit_x2,40,summit_x2,70,stroke=col.ir2)
       layerZoomTrackCov$setLimits(qrect(xlim,c(0,80)))
      }
      }
    }else{
      starts <-min(zoomObj@start)
      ends <- max(zoomObj@end)
      xlim <- c(starts,ends)
      qstrokeColor(painter) <- 'black'
      qdrawText(painter,'Coverage Score for Pax5-NULL',
                      sum(xlim)/2,5,'center','bottom')
      qdrawText(painter,'Coverage Score for Rag2-NULL',
                   sum(xlim)/2,35*1.1+2,'center','bottom')
      qdrawSegment(painter,starts,0,ends,0,stroke='gray')
      qdrawSegment(painter,starts,35*1.1,ends,35*1.1,stroke='gray')
    }
  }
  exonTrack <- function(layer,painter,exposed){
     xlimZoom <- as.matrix(exposed)[,1]
     zoomStarts<<-xlimZoom[1]
     zoomEnds<<-xlimZoom[2]
     selectwidth<<-round(diff(xlimZoom),0)
     qupdate(layerZoomChrom)
    if(is.null(zoomObj@region_start)|is.null(zoomObj@region_end)){
      xlim <-c(min(zoomObj@start),max(zoomObj@end))
      objExon <- objExonLst[[zoomObj@chr]]
      irexon <- IRanges(start(objExon),end(objExon))
      binsexon <- disjointBins(irexon)
      binmx <- max(binsexon*10+5)
      qdrawRect(painter,start(objExon),(binsexon*10)/binmx*5,end(objExon),
                (binsexon*10+5)/binmx*5,stroke='black',fill='black')
      qdrawText(painter,'Exons',sum(xlimZoom)/2,6,
                'center','bottom')
      layerZoomTrackExon$setLimits(qrect(xlim,c(0,5+5)))
    }else{
      xlim <- c(zoomObj@region_start,zoomObj@region_end)
      objExon <- objExonLst[[zoomObj@chr]]
      irexon <- IRanges(start(objExon),end(objExon))
      binsexon <- disjointBins(irexon)
      irhot <- IRanges(xlim[1],xlim[2])
      mat <- as.matrix(findOverlaps(irexon,irhot))
      query <- mat[,1]
      if(length(query>0)){
              irnewexon <- irexon[query]
      binsexon <- disjointBins(irnewexon)
      binmx <- max(binsexon*10+5)
        qdrawRect(painter,start(irnewexon),binsexon*10/binmx*5,end(irnewexon),(binsexon*10+5)/binmx*5,stroke='black',fill='black')
        qstrokeColor(painter) <- 'black'
        qdrawText(painter,'Exons',sum(xlimZoom)/2,6,
                'center','bottom')
       layerZoomTrackExon$setLimits(qrect(xlim,c(0,5+5)))
      }else{
        qdrawSegment(painter,xlim[1],1,xlim[2],1,stroke='gray')
        qstrokeColor(painter) <- 'black'
        qdrawText(painter,'Exons',sum(xlimZoom)/2,6,
                'center','bottom')
        layerZoomTrackExon$setLimits(qrect(xlim,c(0,5+5)))
      }   
    }
  }
  txTrack <- function(layer,painter,exposed){
     xlimZoom <- as.matrix(exposed)[,1]
    if(is.null(zoomObj@region_start)|is.null(zoomObj@region_end)){
      xlim <-c(min(zoomObj@start),max(zoomObj@end))
      objTx <- objTxLst[[zoomObj@chr]]
      irtx <- IRanges(start(objTx),end(objTx))
      binstx <- disjointBins(irtx)
      binmx <- max(binstx*10+5)
      qstrokeColor(painter) <- 'blue'
      qdrawText(painter,'Transcripts',sum(xlimZoom)/2,5,
                'center','bottom')
      qdrawRect(painter,start(objTx),(binstx*10)/binmx*5,end(objTx),(binstx*10+5)/binmx*5,stroke='blue',fill='blue')
      layerZoomTrackTx$setLimits(qrect(xlim,c(0,5+5)))
      ##      layerZoomTrackTx$setLimits(qrect(xlim,c(0,max(binstx)*10+5)))
    }else{
      xlim <- c(zoomObj@region_start,zoomObj@region_end)
      objTx <- objTxLst[[zoomObj@chr]]
      irtx <- IRanges(start(objTx),end(objTx))
      irhot <- IRanges(xlim[1],xlim[2])
      mat <- as.matrix(findOverlaps(irtx,irhot))
      query <- mat[,1]
      if(length(query)>0){
      irnewtx <- irtx[query]
      binstx <- disjointBins(irnewtx)
      binmx <- max(binstx*10+5)
        qdrawRect(painter,start(irnewtx),binstx*10/binmx*5,end(irnewtx),
                  (binstx*10+5)/binmx*5,stroke='blue',fill='blue')
        qstrokeColor(painter) <- 'blue'
        qdrawText(painter,'Transcripts',sum(xlimZoom)/2,6,
                'center','bottom')
        layerZoomTrackTx$setLimits(qrect(xlim,c(0,5+5)))
      }else{
        qdrawSegment(painter,xlim[1],1,xlim[2],1,stroke='blue')
                qdrawText(painter,'Transcripts',sum(xlimZoom)/2,6,
                'center','bottom')
        layerZoomTrackTx$setLimits(qrect(xlim,c(0,5+5)))
      }
    }
  }
  widgetZoom <<- Qt$QWidget()
  layoutZoom <<- Qt$QGridLayout()
  widgetZoom$setLayout(layoutZoom)
  
  sceneZoomChrom <<-qscene()
##  sceneZoomScale<<-qscene()
  sceneZoomTrack <<- qscene()
  layerZoomChrom<<-qlayer(sceneZoomChrom,paintFun=gDrawSingleChrom,
                          limits=qrect(c(0,550),c(-50,40)),cache=FALSE)
##  layerZoomScale<<-qlayer(sceneZoomScale,paintFun=scaleTrack,
                               ## limits=qrect(c(0,20),c(0,5)),
                               ## cache=FALSE)
  layerZoomTrack<<-qlayer(sceneZoomTrack,wheelFun=wheelZoom,
                          geometry=qrect(0,0,600,600),cache=FALSE)

  layerZoomTrackExon<<-qlayer(layerZoomTrack,paintFun=exonTrack,row=1,cache=FALSE)
  layerZoomTrackTx <<- qlayer(layerZoomTrack,paintFun=txTrack,row=2,cache=FALSE)
  layerZoomTrackCov<<-qlayer(layerZoomTrack,paintFun=covPaintFun,row=3,cache=FALSE)
  xlim <- c(min(zoomObj@start),max(zoomObj@end))
  layerZoomTrackExon$setLimits(qrect(xlim,c(0,5+5)))
  layerZoomTrackTx$setLimits(qrect(xlim,c(0,5+5)))
  layerZoomTrackCov$setLimits(qrect(xlim,c(-10,80+10)))
  viewZoomChrom <<-qplotView(sceneZoomChrom)
##  viewZoomScale <<-qplotView(sceneZoomScale)
  viewZoomTrack <<-qplotView(sceneZoomTrack,rescale='none')
  layoutZoom$addWidget(viewZoomChrom,0,0)
##  layoutZoom$addWidget(viewZoomScale,1,0)
  layoutZoom$addWidget(viewZoomTrack,2,0)
  layoutZoom$setRowStretch(0,1)
##  layoutZoom$setRowStretch(1,1)
  layoutZoom$setRowStretch(2,5)
##  layerZoomTrack$gridLayout()$setRowStretchFactor(0,1)
  layerZoomTrack$gridLayout()$setRowStretchFactor(1,4)
  layerZoomTrack$gridLayout()$setRowStretchFactor(2,4)
  layerZoomTrack$gridLayout()$setRowStretchFactor(3,5)
  layoutZoom$setVerticalSpacing(0)
})



