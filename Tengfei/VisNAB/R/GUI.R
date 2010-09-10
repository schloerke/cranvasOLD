gGUI <- function(){
  pframe <- scatterObj@mutaframe

  df <- as.data.frame(pframe)
  scatterObj@mutaframe$asumsDiff <<- abs(as.numeric(as.character(df$asums1))-as.numeric(as.character(df$asums2)))
  state <<- FALSE
  filter.num <<- 1
  cbox.filters <<- c('None','Top Different Region','Top Similiar Region','Top Density Max Difference')
  options('guiToolkit'='RGtk2')
  win <<- gwindow('Control Panel',width=600,height=400)
  vgroup <<- ggroup(horizontal=FALSE,cont=win)
  mbl <- list(
              File=list(
                openFile=list(handler=defH,icon='open'),
                quit=list(handler=closeHandler,icon='cancel')
                )
              )
  tbl <- list(
              open=list(handler=defH,icon='open'),
              quit=list(handler=closeHandler,icon='cancel'),
              clear=list(handler=clearAllHandler,icon='clear')
              )
  mb <- gmenu(mbl,cont=vgroup)
  tb <- gtoolbar(tbl,cont=vgroup)
  nb <- gnotebook(cont=vgroup,expand=TRUE)
  vgroupMajor <- ggroup(horizontal=FALSE,cont=nb,expand=TRUE,label='Filter and Data')
  envExpand <<- new.env(emptyenv())
  rightArrow <<- system.file('images/1rightarrow.gif',package='gWidgets')
  downArrow <<- system.file('images/1downarrow.gif',package='gWidgets')
  expandFilterGroup <- ggroup(horizontal=TRUE,cont=vgroupMajor)
  expandIcon <<- gimage(rightArrow,cont=expandFilterGroup)
  expandLabel <<- glabel('Filter',cont=expandFilterGroup)
  addHandlerClicked(expandIcon,handler=changeFilterState.cb)
  addHandlerClicked(expandLabel,handler=changeFilterState.cb)
  ## create filter first
  vg.fg <<- ggroup(horizontal=FALSE,cont=vgroupMajor,expand=FALSE)
  vg.fg.f1 <<- gframe('Filter',cont=vg.fg,horizontal=FALSE)
  hideFilter()
  vg.fg.f1.hbox <<- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  vg.fg.f1.addButtonG <<-ggroup(horizontal=FALSE,cont=vg.fg.f1.hbox)
  vg.fg.f1.filter<<-ggroup(horizontal=FALSE,cont=vg.fg.f1.hbox,expand=TRUE)
  ## first filter fist
  addFilterButton <<- gbutton('  +  ',cont=vg.fg.f1.addButtonG,handler=createFilter.cb, expand=FALSE)
  ## vg.fg.f1.comp <<- ggroup(horizontal=TRUE,cont=vg.fg.f1.filter,expand=TRUE)
  ## vg.fg.f1.comp.c<<- ggroup(horizontal=TRUE,cont=vg.fg.f1.comp,expand=FALSE)
  ## vg.fg.f1.comp.s<<- ggroup(horizontal=TRUE,cont=vg.fg.f1.comp,expand=TRUE)
  ## ## combo box
  ## cbox.major <<- gcombobox(cbox.filters,cont=vg.fg.f1.comp.c,handler=function(h,...){
  ##   delete(vg.fg.f1.comp,vg.fg.f1.comp.s)
  ##   vg.fg.f1.comp.s<<- ggroup(horizontal=TRUE,cont=vg.fg.f1.comp,expand=TRUE)
  ##   va <- nrow(pframe)
  ##   va.by=1
  ##   vgroup.slider.first <<- gslider(0,va,by=va.by,cont=vg.fg.f1.comp.s,expand=TRUE)    })
  ## logic
  vg.fg.f1.expand <<- ggroup(horizontal=FALSE,cont=vg.fg.f1.filter)
  vg.fg.f1.expandcomp<<-ggroup(horizontal=FALSE,cont=vg.fg.f1.expand)
  vg.fg.f1.logic <- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  springLogic <- ggroup(horizontal=TRUE,cont=vg.fg.f1.logic,expand=TRUE)
  text.logic <- glabel('Logic',cont=vg.fg.f1.logic)
  cbox.logic <<- gcombobox(c('AND','OR'),cont=vg.fg.f1.logic)
  vg.fg.f1.button <- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  sprintFilterButton <- ggroup(horizontal=TRUE,cont=vg.fg.f1.button,expand=TRUE)
  applyFilterButton <- gbutton('Apply',cont=vg.fg.f1.button,
                               handler=applyFilterHandler)
  clearFilterButton <- gbutton('Clear',cont=vg.fg.f1.button,
                               handler=removeExpand.cb)
  vg.fg.f1.ucsc <- ggroup(horizontal=TRUE,cont=vg.fg.f1)
  ucscButton <- gbutton('View in UCSC Genome Browser',cont=vg.fg.f1.ucsc,handler=viewUCSC.cb)
  vgroup.noInUse <<- ggroup(horizontal=FALSE,cont=vg.fg,expand=TRUE)
  vgroup.f1 <<- gframe('Data Table',cont=vgroupMajor,expand=TRUE)
  vgroup.table <<- gdf(df,cont=vgroup.f1,expand=TRUE)
  ##scatterPlot control panel
  scatterG <- ggroup(horizontal=FALSE,cont=nb,label='Scatter plot')
  nm <- names(pframe)[!names(pframe)%in%c('fill','isSelected')]
  scatterXYG <- ggroup(horizontal=TRUE,cont=scatterG)
  scatterXG <- ggroup(horizontal=FALSE,cont=scatterXYG)
  scatterXLabel <- glabel('X',cont=scatterXG)
  checkX <<- gcombobox(nm,cont=scatterXG)
  svalue(checkX) <- scatterObj@pars$xy[1]
  scatterYG <- ggroup(horizontal=FALSE,cont=scatterXYG)
  scatterYLabel <- glabel('Y',cont=scatterYG)
  checkY <<- gcombobox(nm,cont=scatterYG)
  svalue(checkY) <- scatterObj@pars$xy[2]
  scatterButtonG <- ggroup(horizontal=TRUE,cont=scatterG)
  scatterButton <- gbutton('Apply',cont=scatterButtonG,handler=scatterApply.cb)
  addSpring(scatterButtonG)
  showWin()
}

viewUCSC.cb <- function(h,...){
  require(rtracklayer)
  viewUCSC()
}
viewUCSC <- function(){
    session <- browserSession('UCSC')
    currentChrom <- zoomObj@chr
    ir <- IRanges(start=zoomStarts,end=zoomEnds)
    targets <- GenomicData(ir,chrom=currentChrom,genome='mm9')
    track(session,'targets') <- targets
    browserView(session,range(targets),pack='targets')
}




