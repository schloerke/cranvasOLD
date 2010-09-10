## Just a simple control panel, so we can load file and send interactive command
## filter 1:
defH <- function(h,...) print('Hello World')
sliderHandler.topDiff <- function(h,...){
  filt <- svalue(h$obj)
  scatterObj@mutaframe$fill<<-'black'
  scatterObj@mutaframe$isSelected <<- FALSE
  id <- order(asumsDiff,decreasing=TRUE)[1:filt]
  scatterObj@mutaframe$fill[id]<<-'blue'
  scatterObj@mutaframe$isSelected[id] <<- TRUE
  qupdate(s.scatter)
  qupdate(s.bird)
  hgroup.table[,]<<-as.data.frame(scatterObj@mutaframe)[id,]
}

sliderHandler.topCommon <- function(h,...){
  filt <- svalue(h$obj)
  scatterObj@mutaframe$fill<<-'black'
  scatterObj@mutaframe$isSelected <<- FALSE
  id <- order(asumsDiff,decreasing=FALSE)[1:filt]
  scatterObj@mutaframe$fill[id]<<-'blue'
  scatterObj@mutaframe$isSelected[id] <<- TRUE
  qupdate(s.scatter)
  qupdate(s.bird)
  hgroup.table[,]<<-as.data.frame(scatterObj@mutaframe)[id,]
}


clearAllHandler <- function(h,...){
##   if(exists('vgroup.slider.first')){
##   svalue(vgroup.slider.first) <- 0
## }
  ## if(filter.num>1){
  ##   eapply(envExpand,function(x){
  ##     svalue(x)<-0
  ##   })
  ## }
  rm(list=ls(envExpand),envir=envExpand)
  filter.num<<-1
  scatterObj@mutaframe$fill<<-'black'
  scatterObj@mutaframe$isSelected <<- FALSE
  scatterObj@mutaframe$isHighlighted<<-FALSE
  if(l.wide.state){
  l.wide$close()
  l.wide.state<<- FALSE
}
##  qupdate(layerIsland)
  qupdate(viewZoomTrack)
  qupdate(viewZoomChrom)
  qupdate(v.scatter)
  qupdate(s.bird)
  isChromSelected <<- TRUE
  ## delete(vgroup.f1,vgroup.table)
  ## vgroup.table <<- gdf(as.data.frame(pframe),cont=vgroup.f1,expand=TRUE)
  toplefty<<-bottomrighty<<-posstart<<-posend<<-NULL
}

closeHandler <- function(h,...){
  mywindow$close()
  dispose(win)
}

createFilter.cb <- function(h,...){
   ## i <- i+1
   ## names <- paste('vg.fg.f1.comp',i,sep='')
   ## assign(names,ggroup(horizontal=TRUE,cont=vg.fg.f1.expandcomp))
  vg.fg.f1.comp2 <<- ggroup(horizontal=TRUE,cont=vg.fg.f1.expandcomp)
  cbox.major <<- gcombobox(cbox.filters,cont=vg.fg.f1.comp2,handler=cboxExpandFilter)
  sliderGroupExp<<-ggroup(horizontal=TRUE,cont=vg.fg.f1.comp2,expand=TRUE)
  topDiff <- gslider(0,nrow(scatterObj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
}

cboxExpandFilter <- function(h,...){
  delete(vg.fg.f1.comp2,sliderGroupExp)
  sliderGroupExp<<-ggroup(horizontal=TRUE,cont=vg.fg.f1.comp2,expand=TRUE)
  name <- svalue(h$obj)
  if(name=='Top Different Region'){
    topDiff <- gslider(0,nrow(scatterObj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
    attr(topDiff,'name') <- 'topDiff'
    assign('topDiff',topDiff,env=envExpand)
    filter.num <<- filter.num+1
  }
  if(name=='Top Similiar Region'){
    topSim <- gslider(0,nrow(scatterObj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
    attr(topSim,'name') <- 'topSim'
    assign('topSim',topSim,env=envExpand)
    filter.num <<- filter.num+1
  }
  if(name=='Top Smoothed Max Diff'){
    topSMD <- gslider(0,nrow(scatterObj@mutaframe),by=1,cont=sliderGroupExp,expand=TRUE)
    attr(topSMD,'name') <- 'topSmoothMaxDiff'
    assign('topSmoothMaxDiff',topSMD,env=envExpand)
    filter.num <<- filter.num+1
  }
  if(name=='Top Density Max Difference'){
    topTDMD <- gslider(0,nrow(scatterObj@mutaframe),by=0.1,cont=sliderGroupExp,expand=TRUE)
    attr(topTDMD,'name') <- 'topDensityMaxDiff'
    assign('topDensityMaxDiff',topTDMD,env=envExpand)
    filter.num <<- filter.num+1
  }
}

removeExpand.cb <- function(h,...){
  delete(vg.fg.f1.expand,vg.fg.f1.expandcomp)
  vg.fg.f1.expandcomp<<-ggroup(horizontal=FALSE,cont=vg.fg.f1.expand)
  clearAllHandler()
}

hideFilter <- function(h,...){
  delete(vg.fg,vg.fg.f1)
}

expandFilter <- function(h,...){
  add(vg.fg,vg.fg.f1,expand=TRUE)
}

changeFilterState.cb <- function(h,...){
  if(state){
    hideFilter()
    svalue(expandIcon) <- rightArrow
  }else{
    expandFilter()
    svalue(expandIcon) <- downArrow
  }
  state<<-!state
}

changeFilterState <- function(){
  if(state){
    hideFilter()
    svalue(expandIcon) <- rightArrow
  }else{
    expandFilter()
    svalue(expandIcon) <- downArrow
  }
  state<<-!state
}

applyFilterHandler <- function(h,...){
  scatterObj@mutaframe$fill<<-rep('black',nrow(scatterObj@mutaframe))
  scatterObj@mutaframe$isSelected <<- rep(FALSE,nrow(scatterObj@mutaframe))
  if(filter.num>0){
    ## nms <- svalue(cbox.major)
    ## nms <- switch(nms,
    ##               'Top Different Region'='topDiff',
    ##               'Top Similiar Region'='topSim',
    ##               'Top Smoothed Max Diff'='topSmoothMaxDiff',
    ##               'Top Density Max Different'='topDensityMaxDiff')
    filt <- eapply(envExpand,function(x){
      df <- data.frame(name=attr(x,'name'),svalue=svalue(x),stringsAsFactors=FALSE)
    })
    filt <- do.call('rbind',filt)
##    filt <- rbind(filt,c(nms,svalue(vgroup.slider.first)))
    nms <- filt$name
  }else{
    filt <- NULL
    nms <- svalue(cbox.major)
  }
  idgroup <- lapply(nms,getIDbyFilter,filt)
  if('Top Density Max Difference' %in% nms){
    l.wide<<- qlayer(s.bird,paintFun=wideRange,cache=TRUE)
    l.wide.state<<-TRUE
  }
  if(length(idgroup)>1&filter.num>1){
    if(svalue(cbox.logic)=='AND'){
     id <- interGroupID(idgroup,'AND')
    }else{
      id <- interGroupID(idgroup,'OR')
    }
  }else{
    id <- idgroup
    id <- as.numeric(unlist(id))
  }
  if(length(id)>0){
    scatterObj@mutaframe$fill[id]<<-'blue'
    scatterObj@mutaframe$isSelected[id] <<- TRUE
    if(TRUE){
      df <- as.data.frame(scatterObj@mutaframe)
      df <- df[order(df$start,decreasing=FALSE),]
      scatterObj@mutaframe$marker<<-0
    }
    qupdate(s.scatter)
    qupdate(s.bird)
    ## delete(vgroup.f1,vgroup.table)
    ## vgroup.table <<- gdf(as.data.frame(scatterObj@mutaframe)[id,],cont=vgroup.f1,expand=TRUE)
  }
}

## without data frame update
applyFilterHandler.nodf <- function(h,...){
  scatterObj@mutaframe$fill<<-'black'
  scatterObj@mutaframe$isSelected <<- FALSE
  if(filter.num>1){
    nms <- svalue(cbox.major)
    filt <- eapply(envExpand,function(x){
      df <- data.frame(name=attr(x,'name'),svalue=svalue(x),stringsAsFactors=FALSE)
    })
    filt <- do.call('rbind',filt)
    nms <- c(filt$name,nms)
  }else{
    filt <- NULL
    nms <- svalue(cbox.major)
  }
  idgroup <- lapply(nms,getIDbyFilter,filt)
  if(length(idgroup)>1&filter.num>1){
    hotden<<-NULL
    if(svalue(cbox.logic)=='AND'){
      id <- interGroupID(idgroup,'AND')
    }else{
      id <- interGroupID(idgroup,'OR')
    }
  }else{
    id <- idgroup
    id <- as.numeric(unlist(id))
  }
  if(id>0){
    scatterObj@mutaframe$fill[id]<<-'blue'
    scatterObj@mutaframe$isSelected[id] <<- TRUE
    qupdate(s.scatter)
    qupdate(s.bird)
  }
}

## call back for 'Apply' button of scatter plot
scatterApply.cb <- function(h,...){
  x.new.ori.nm <- svalue(checkX)
  y.new.ori.nm <- svalue(checkY)
  scatterObj@pars$xy<<-c(x.new.ori.nm,y.new.ori.nm)
  summary <- scatterObj@mutaframe
  px.ori<<-summary[[x.new.ori.nm]]
  py.ori<<-summary[[y.new.ori.nm]]
  mxx <- max(px.ori)
  mnx <- min(px.ori)
  mxy <- max(py.ori)
  mny <- min(py.ori)
  ## relative location
  scale <- scatterObj@pars$scale
  mar <- scatterObj@pars$mar
  px<<-(px.ori-mnx)/(mxx-mnx)*scale+mar[2]
  py<<-(py.ori-mny)/(mxy-mny)*scale+mar[3]
  pymax <<- max(py)
  pxmax <<- max(px)
  qupdate(s.scatter)
}

getIDbyFilter <- function(name,filt){
  if(filter.num>0){
    va.idx <- which(filt$name==name)
    va <- filt$svalue[va.idx]
    order(scatterObj@mutaframe$den,decreasing=TRUE)[1:va]
    id <- switch(name,
                 topDiff=order(scatterObj@mutaframe$asumsDiff,decreasing=TRUE)[1:va],
                 topSim=order(scatterObj@mutaframe$asumsDiff,decreasing=FALSE)[1:va],
                 topSmoothMaxDiff=order(scatterObj@mutaframe$smoothMaxDiff,decreasing=TRUE)[1:va],
                 topDensityMaxDiff=order(scatterObj@mutaframe$den,decreasing=TRUE)[1:va]
                 )
  }else{
  ##   va <- svalue(vgroup.slider.first)
  ##   if(name=='Top Different Region'){
  ##     id <- order(scatterObj@mutaframe$asumsDiff,decreasing=TRUE)[1:va]
  ##   }
  ##   if(name=='Top Similiar Region'){
  ##     id <-  order(scatterObj@mutaframe$asumsDiff,decreasing=FALSE)[1:va]
  ##   }
  ##   if(name=='Top Smoothed Region'){
  ##     id  <- order(scatterObj@mutaframe$smoothMaxDiff,decreasing=TRUE)[1:va]
  ##   }
  ##   if(name=='Top Density Max Difference'){
  ##     id <- order(scatterObj@mutaframe$den,decreasing=TRUE)[1:va]
  ##   }
    message('No filter selected!')
   }
  id
}

## intersect for group
interGroupID <- function(sets.list,logic=c('AND','OR')){
  myfun <- switch(logic,
                  AND=function(sets.list){
                    temp <- sets.list[[1]]
                    for(i in 2:(length(sets.list))){
                      temp <- intersect(sets.list[[i]],temp)
                    }
                    temp
                  },
                  OR=function(sets.list){
                    temp <- sets.list[[1]]
                    for(i in 2:(length(sets.list))){
                      temp <- union(sets.list[[i]],temp)
                    }
                    temp
                  })
  id <- myfun(sets.list)
  id
}


