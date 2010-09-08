##================================================================##
##       Send Biomart Query 
##================================================================##

##----------------------------------------------------------------##
##            FOR class 'ZoomableViewBM'
##----------------------------------------------------------------##
setMethod('initialize','ZoomableViewBM',function(.Object,...){
  .Object@pars$width <- 15
  .Object@pars$scale <- 400
  .Object@pars$mar <- c(20,20,20,20)
  .Object@pars$skip.factor <- 20
  .Object@pars$bg.col <- 'white'
  .Object@pars$chrText.width <- 40
  .Object@pars$bg.alpha <- 1
  .Object@pars$name.width <- 100
  .Object@pars$col.exon <- rgb(1,0,0,1)
  .Object@pars$col.transcript <- rgb(0,1,0,1)
  .Object <- callNextMethod()
  .Object
})
## constructor
genZoomBrowserBM <- function(species,chr,...){
  load('~/Prolang/svn/repos/trunk/genomebrowser/pkg/genomebrowser/data/cytobands.rda')
  spe <- cytobands[[species]]
  spe.lst <- split(spe,spe$chr)
  mydf <- spe.lst[[chr]]
  new('ZoomBrowserBM',species=as.character(species),chr=as.character(chr),
      start=as.numeric(as.character(mydf$start)),
      end=as.numeric(as.character(mydf$end)),
      stain=as.character(mydf$stain),arm=as.character(mydf$arm),...)
}

setMethod('gplot','ZoomableViewBM',function(obj,...){
  if(!(is.null(obj@region_start))&!(is.null(obj@region_end))){
    obj@filters <- c('chromosome_name','start','end')
    chrname <- substr(obj@chr,4,nchar(obj@chr))
    if(!chrname %in% c('x','y','X','Y')) chrname <- as.numeric(chrname)
    obj@values <- list(chrname,obj@region_start,obj@region_end)
  }
  message('Loadindg data...')
  res <- getBMQ(obj)
  message('Done')
  ## initizlize
  chrom <- unique(obj@chr)
  mar <- getPar(obj,'mar')
  name.width <- getPar(obj,'name.width')
  width <- getPar(obj,'width')
  scale <- getPar(obj,'scale')
  skip.factor <- getPar(obj,'skip.factor')
  col.exon <- getPar(obj,'col.exon')
  col.transcript <- getPar(obj,'col.transcript')
  ## all the painter function, put it here because I use closure here
  gDrawSingleChrom<- function(layer,painter){
    stain <- unique(obj@stain)
    color <- paste('gray',round((1:length(unique(stain)))/length(unique(stain))*100),sep='')
    names(color) <- unique(stain)
    ## plus 20 cause leave black for text
    mx <- max(obj@end)
    scale <- scale*2/3
    chrText.width <- getPar(obj,'chrText.width')
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
    qdrawPolygon(painter,c(x,x1,x1),c(y,y0,y1),fill='darkred')
  }
  ## For plotting exons
  exonPaintFun <- function(layer,painter){
    start <- res$exon_chrom_start
    end <- res$exon_chrom_end
    regionStart <- obj@region_start
    regionEnd <- obj@region_end
    mx <- max(c(start,end,regionStart,regionEnd))
    mn <- min(c(start,end,regionStart,regionEnd))
    geneId <- unique(res$ensembl_gene_id)[1]
    start.pos <- (start-mn)/(mx-mn)*scale+name.width+mar[2]
    end.pos <- (end-mn)/(mx-mn)*scale+name.width+mar[2]
    regionStart.pos <- (regionStart-mn)/(mx-mn)*scale+name.width+mar[2]
    regionEnd.pos <- (regionEnd-mn)/(mx-mn)*scale+name.width+mar[2]
    ir <- IRanges(start=start.pos,end=end.pos)
    ir.gap <- gaps(ir,start=mar[2]+name.width,end=mar[2]+name.width+scale)
    qdrawRect(painter,start.pos,0,end.pos,0+width,fill=col.exon,stroke=col.exon)
    qdrawSegment(painter,start(ir.gap),width/2,end(ir.gap),width/2,stroke=col.exon)
    ##    qdrawText(painter,geneId,mar[2]+name.width-5,0,'right','top')
  }
  ## For plotting transcripts
  transcriptPaintFun <- function(layer,painter){
    mar <- getPar(obj,'mar')
    name.width <- getPar(obj,'name.width')
    width <- getPar(obj,'width')
    scale <- getPar(obj,'scale')
    start <- res$exon_chrom_start
    end <- res$exon_chrom_end
    regionStart <- obj@region_start
    regionEnd <- obj@region_end
    mx <- max(c(start,end,regionStart,regionEnd))
    mn <- min(c(start,end,regionStart,regionEnd))
    lst <- split(res,res$ensembl_transcript_id)
    regionStart.pos <- (regionStart-mn)/(mx-mn)*scale+name.width+mar[2]
    regionEnd.pos <- (regionEnd-mn)/(mx-mn)*scale+name.width+mar[2]
    for(i in 1:length(lst)){
      temp <- lst[[i]]
      start.pos <- (temp$exon_chrom_start-mn)/(mx-mn)*scale+name.width+mar[2]
      end.pos <- (temp$exon_chrom_end-mn)/(mx-mn)*scale+name.width+mar[2]
      ir <- IRanges(start=start.pos,end=end.pos)
      ir.gap <- gaps(ir,start=min(start.pos),end=max(end.pos))
      yaxis <- (i-1)*(width+skip.factor)
      if(nrow(as.data.frame(ir.gap))>0){
        if(unique(temp$strand)=='1'){
          apply(as.data.frame(ir.gap),1,function(x){
            if(x['width']>6){
              xseq <- seq(from=x['start'],to=x['end'],by=6)
              xseq1 <- xseq+2
              qdrawSegment(painter,xseq,yaxis+width/2-width/6,xseq1,yaxis+width/2,stroke=col.transcript)
              qdrawSegment(painter,xseq1,yaxis+width/2,xseq,yaxis+width/2+width/6,stroke=col.transcript)
            }
          })
        }else{
          apply(as.data.frame(ir.gap),1,function(x){
            if(x['width']>6){
              xseq <- seq(from=x['start'],to=x['end'],by=6)
              xseq1 <- xseq-2
              qdrawSegment(painter,xseq,yaxis+width/2-width/6,xseq1,yaxis+width/2,stroke=col.transcript)
              qdrawSegment(painter,xseq1,yaxis+width/2,xseq,yaxis+width/2+width/6,stroke=col.transcript)
            }
          })
        }
        tranId <- unique(temp$ensembl_transcript_id)[1]
        qdrawRect(painter,start.pos,yaxis,end.pos,yaxis+width,fill=col.transcript,stroke=col.transcript)
        qdrawSegment(painter,start(ir.gap),yaxis+width/2,end(ir.gap),yaxis+width/2,stroke=col.transcript)
        qdrawText(painter,tranId,mar[2]+name.width-5,yaxis,'right','top')
      }
    }
  }
  ## coverage paint function
  covPaintFun <- function(layer,painter){
    range1 <- r1
    range2 <- r2
    mx <- obj@region_end
    mn <- obj@region_start
    chr <- obj@chr
    peaks1 <- range1[[chr]]
    peaks2 <- range2[[chr]]
    ir1 <- as.vector(peaks1)[obj@region_start:obj@region_end]
    ir2 <- as.vector(peaks2)[obj@region_start:obj@region_end]
    ylim <- max(ir1,ir2)
    ir1 <- ir1/ylim*15
    ir2 <- ir2/ylim*15
    ylim <- 15
    idx <- (obj@region_start):(obj@region_end)
    idx <- (idx-mn)/(mx-mn)*scale+mar[2]+name.width
    qdrawRect(painter,idx,0+ylim,idx+1,ylim-ir1,fill='black')
    qdrawRect(painter,idx,0+ylim*2+20,idx+1,2*ylim+20-ir2,fill='black')
    qdrawText(painter,'r1',mar[2]+name.width-5,ylim,'right','top')
    qdrawText(painter,'r2',mar[2]+name.width-5,2*ylim+20,'right','top')
  }
  ## make a grid function to paint scale
  trackLayerPaintFun <- function(layer,painter){
    ## draw a y axis to seperate name and components
    qdrawSegment(painter,mar[2]+name.width,0,mar[2]+name.width,800,stroke=rgb(1,0,0,0.9))
    seqx <- seq(from=mar[2]+name.width+10,to=scale+mar[2]+name.width,by=10)
    qdrawSegment(painter,seqx,0,seqx,600,stroke=rgb(0,0,1,0.1))
  }
  s.zoom <<- qscene()
  root <- qlayer(s.zoom,geometry=qrect(0,0,500,600))
  root$gridLayout()$setRowStretchFactor(0,1)
  root$gridLayout()$setRowStretchFactor(1,4)
  chromLayer <- qlayer(root,paintFun=gDrawSingleChrom,row=0,col=0,cache=FALSE)
  trackLayer <<- qlayer(root,paintFun=trackLayerPaintFun,row=1,cache=FALSE)
  trackLayer$gridLayout()$setRowStretchFactor(0,1)
  trackLayer$gridLayout()$setRowStretchFactor(1,5)
  trackLayer$gridLayout()$setRowStretchFactor(2,1)
  if(nrow(res)>0){
    l1 <- qlayer(trackLayer,paintFun=exonPaintFun,row=0,col=0,cache=FALSE)
    l2 <- qlayer(trackLayer,paintFun=transcriptPaintFun,row=1,col=0,cache=FALSE)
  }
  l3 <- qlayer(trackLayer,paintFun=covPaintFun,row=2,col=0,cache=FALSE)
  view.zoom <<- qplotView(s.zoom,rescale='transform')
})

