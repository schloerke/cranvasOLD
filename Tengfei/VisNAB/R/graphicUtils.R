##-----------------------------------------------------------------##
##                     For plotting island
##-----------------------------------------------------------------##
gplotIsland <- function(range1,range2,id=NULL,cutoff=c(40,40)){
  if(!is.null(id)){
  pframe$isHighlighted<<-FALSE
  pframe$isHighlighted[id]<<-TRUE}
  islandPaintFun <- function(layer,painter){
    getIr <- function(covrange,chr){
      test <- covrange[[chr]]
      res <-  as.vector(seqselect(test,starts,ends))
    }
    object <- as.data.frame(pframe)
    if(any(object$isHighlighted)){
    temp <- object[object$isHighlighted,]
    starts <- temp[,'start']
    ends <- temp[,'end']
    temp$chromosome <- as.character(temp$chromosome)
    chrom <- temp[,'chromosome']
    chrom <- as.character(chrom)
    ir1 <- getIr(range1,chrom)
    ir2 <- getIr(range2,chrom)
    mxir <- max(c(ir1,ir2))
    idx <- starts:ends
    ## island
    qdrawText(painter,quote(ir1),starts-diff(c(starts,ends))*0.1,(mxir+20)/2,'center','bottom',rot=90)
    qdrawText(painter,quote(ir2),starts-diff(c(starts,ends))*0.1,(mxir+20)/2+mxir+20,'center','bottom',rot=90)
    qdrawLine(painter,idx,ir1)
    qdrawLine(painter,idx,ir2+mxir+20)
    ## axis
    qdrawSegment(painter,starts,0,ends,0) #x-axis
    qdrawSegment(painter,starts,mxir+20,ends,mxir+20) #x-axis
    qdrawSegment(painter,starts,0,starts,mxir+15) #y-axis
    qdrawSegment(painter,starts,mxir+20,starts,mxir*2+30) #y-axis
    ## cutoff
    qdrawSegment(painter,starts,0+cutoff[1],ends,0+cutoff[1],stroke='red') 
    qdrawSegment(painter,starts,0+mxir+20+cutoff[2],ends,0+mxir+20+cutoff[2],'red') 
    ## mark
    xseq <- seq(starts,ends,length.out=5)
    xlabel <- round(xseq)
    qdrawSegment(painter,xseq,0,xseq,-mxir*0.015,stroke='black')
    yseq1 <- seq(0,mxir,length.out=5)
    yseq2 <- seq(mxir+20,mxir*2+20,length.out=5)
    ylabel <- round(yseq1)
    xrange <- diff(c(starts,ends))
    qdrawSegment(painter,starts,c(yseq1,yseq2),starts-0.015*xrange/2,c(yseq1,yseq2),stroke='black')
    ## draw text first to make sure they are black
    qstrokeColor(painter) <- 'black'
    qdrawText(painter,xseq[c(1,5)],xseq[c(1,5)],-mxir*0.02,'center','top')
    qdrawText(painter,c(ylabel,ylabel),starts-0.04*xrange/2,c(yseq1,yseq2),'right')
    marx <- (ends-starts)*0.2
    mary <- (mxir*2+20)*0.2
    lmt <- qrect(c(starts-marx,ends+marx),c(0-mary,2*max(c(ir1,ir2))+20+mary))
    layerIsland$setLimits(lmt)
  }
  }
  sceneIsland <<- qscene()
  layerIsland <<- qlayer(sceneIsland,paintFun=islandPaintFun)
  viewIsland <<- qplotView(sceneIsland)
}




