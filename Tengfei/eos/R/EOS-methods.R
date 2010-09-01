##--------------------------------------------------------------##
##                         Methods
##--------------------------------------------------------------##

setGeneric('plot')

##----------------------------------------------------------------##
##                Methods for GraphicsPars
##----------------------------------------------------------------##

setGeneric('setPar',function(obj,...) standardGeneric('setPar'))
setMethod('setPar','GraphicPars',
          function(obj,name,value){
            assign(name,value,obj@pars)
          })

setGeneric('getPar',function(obj,...) standardGeneric('getPar'))
setMethod('getPar','GraphicPars',
          function(obj,name){
            if(!exists(name,obj@pars))
              stop(paste('No graphic parameter named',name,'could be found!'))
            get(name,obj@pars)
          })

setMethod('show','GraphicPars',function(object){
  sapply(ls(object@pars), function(x) {
    y <- getPar(object, x)
    if (length(y) > 10)
      y <- y[1:10]
    cat(x, " = ", toString(y), "\n")
  })
})

pushCon <- function(gp1, gp2) {
  if (is.null(gp1) && is.null(gp2))
    return(DisplayPars())
  if (is.null(gp1)) 
    return(gp2)
  if (is.null(gp2))
    return(gp1)
  sapply(ls(gp1@pars), function(nm) {
    setPar(gp2, nm, getPar(gp1, nm))
  })
  return(gp2)
}





##----------------------------------------------------------------##
##                Constructor for GraphicsPars
##----------------------------------------------------------------##

GraphicPars <- function(...){
  args <- list(...)
  gp <- new('GraphicPars')
  ##  i <- match(names(args),'gp')
  if(length(args)>0){
    lapply(1:length(args),function(i){
      setPar(gp,names(args)[i],args[[i]])
    })
  }
  gp
}



##--------------------------------------------------------------##
##               Constructor for EOSTrack
##--------------------------------------------------------------##

EOSTrack <- function(obj,type,...){
  gp1 <- GraphicPars(...)
  if(!extends(class(obj),'RangedData'))
    stop('Input object should be RangedData object.')
  if(!(type%in%.TYPES))
    stop(paste('typs should only be',.TYPES))
  gp2 <- GraphicPars()
  ##gp2@pars$order <- 1
  ##gp2@pars$width <-
  gp <- pushCon(gp1,gp2)
  res <- new('EOSTrack',data=obj,type=type,pars=gp@pars)
  res
}

##--------------------------------------------------------------##
##               Constructor for EOSView
##--------------------------------------------------------------##

EOSView <- function(obj,...){
  ## obj should be a list of EOSTrack
  if(!all(lapply(obj,function(x){
    class(x)=='EOSTrack'
  })))
    stop('All the track object should be EOSTrack')
  gp1 <- GraphicPars(...)
  gp2 <- GraphicPars()
  gp2@pars$width <- 20
  gp2@pars$length <- 100
  gp2@pars$skip <- 10
  gp2@pars$theme <- 'default'
  gp2@pars$spaceRate <- 0.01
  ##gp2@pars$scale <- 
  gp <- pushCon(gp1,gp2)
  res <- new('EOSView',listData=obj,pars=gp@pars)
}


setMethod('plot','EOSView',function(x,...){
  ## if(is.null(myscale)) myscale <- max(end(x@data))
  ## myscale <- x@pars$scale
  scene <- qscene()
  spaceRate <- x@pars$spaceRate
  sp <- spaceRate*360
  skip <- x@pars$skip
  len <- x@pars$length+(x@pars$width+20)*length(x@listData)
  ##for(n in 1:length(x@listData)){
  lapply(1:length(x@listData),function(n){
    rd <- x@listData[[n]]@data
    tp <- x@listData[[n]]@type
    l <- x@pars$length
    w <- x@pars$width
    if(tp=='sector'){
    ## rd$width <- width(rd)/sum(as.numeric(width(rd)))*(360-sp*length(rd))
    ## rd$start <- c(0,cumsum(rd$width)[-(length(rd))])+(1:length(rd)-1)*sp
    mw <- width(rd)/sum(as.numeric(width(rd)))*(360-sp*length(rd))
    ms <- c(0,cumsum(mw)[-(length(rd))])+(1:length(rd)-1)*sp
    paths <- lapply(1:length(rd),function(i){
      sa <- ms[i]
      sl <- mw[i]
      paths <- qglyphSector(0,0,length=l+w*(n-1)+skip*(n-1),width=w,
                            startAngle=sa,sweepLength=sl)
    })
      paintFun <- function(layer,painter){
      qdrawPath(painter,paths,fill=rainbow(length(paths)),stroke=NA)
    }
  }
    
    if(tp=='segment'){
      mw <- width(rd)/sum(as.numeric(width(rd)))*(360-sp*length(rd))
      ms <- c(0,cumsum(mw)[-(length(rd))])+(1:length(rd)-1)*sp
      ## compute the position
      mp <- ms+mw/2
      mp <- mp
      xy1 <- polar2xy(radius=l+w*(n-1)+skip*(n-1),mp)
      xy2 <- polar2xy(radius=l+w*(n-1)+skip*(n-1)+w,mp)
      paintFun <- function(layer,painter){
        qdrawSegment(painter,xy1$x,xy1$y,xy2$x,xy2$y)
      }
    }
    if(tp=='text'){
      mw <- width(rd)/sum(as.numeric(width(rd)))*(360-sp*length(rd))
      ms <- c(0,cumsum(mw)[-(length(rd))])+(1:length(rd)-1)*sp
      ## compute the position
      mp <- ms+mw/2
      xy1 <- polar2xy(radius=l+w*(n-1)+skip*(n-1),mp)
##      xy2 <- polar2xy(radius=l+w*(n-1)+skip*(n-1)+w,mp)
      paintFun <- function(layer,painter){
##        qdrawCircle(painter,xy1$x,xy1$y,r=2,fill='red')
        idx <- !(mp>90 & mp<270)
        qdrawText(painter,space(ird)[idx],xy1$x[idx],xy1$y[idx],halign='left',valign='bottom',rot=mp[idx],color=rainbow(length(mp))[idx])
        qdrawText(painter,space(ird)[!idx],xy1$x[!idx],xy1$y[!idx],halign='right',valign='bottom',rot=mp[!idx],color=rainbow(length(mp))[!idx])

      }
    }
      qlayer(scene,paintFun=paintFun,limits=qrect(c(-len,len),c(-len,len)),geometry=qrect(0,0,400,400))

  })
  ## layer <- qlayer(scene,paintFun=paintFun,limits=qrect(c(-len,len),c(-len,len)),geometry=qrect(0,0,400,400))
  view <- qplotView(scene)
  view$show()
  invisible(list(scene,view))
})


## write a function to translate the coordinates
## xy2polar need to be fixed, to the right place.
xy2polar <- function(x,y){
  angle <- atan(y/x)/pi*180
  radius <- sqrt(x^2+y^2)
  data.frame(radius=radius,angle=angle)
}

polar2xy <- function(radius,angle){
  x <- radius*cos(angle/360*(2*pi))
  y <- radius*sin(angle/360*(2*pi))
  data.frame(x=x,y=y)
}






