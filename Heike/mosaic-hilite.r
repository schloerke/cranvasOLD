source("../utilities/api-sketch.r")
source("../utilities/helper.r")
source("../utilities/axes.r")
require(stringr)
require(productplots)

find_x_label <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  axis.set <- subset(df, (b==min(b)) &  (level==max(level)))
  
  paste(vars[sapply(vars, function(x) return(length(unique(axis.set[,x]))>1))],"")
}

find_y_label <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  axis.set <- subset(df, (l==min(l)) & (level==max(level)))
  
  paste(vars[sapply(vars, function(x) return(length(unique(axis.set[,x]))>1))],"")
}



qmosaic <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, subset=NULL, colour="grey30", main=NULL, ...) {
  odata <- data


  
  data <- productplots:::prodcalc(odata, formula, divider, cascade, scale_max, na.rm = na.rm)
  .level <- max(data$level)-1

  extract.formula <- function(formula) {
	form <- parse_product_formula(formula)

	ncond <- length(form$cond)
	nmarg <- length(form$marg)
	
	if (ncond <= .level) { 
		fcond <- form$cond
		.level <- .level - ncond
	} else {
		fcond <- form$cond[(ncond-.level+1):ncond]
		.level <- 0
	}

	if (.level == 0) fmarg <- ""
	else {
	  if (nmarg <= .level) { 
		fmarg <- form$marg
	  } else {
		fmarg <- form$marg[(nmarg-.level+1):nmarg]
	  }
	}
# piece everything together
	formstring <- paste(form$wt,"~ ")

	if (length(fmarg) > 0) formstring <- paste(formstring, paste(fmarg, collapse= "+"))
	else formstring <- paste(formstring,"1")
		
	if (length(fcond) > 0) formstring <- paste(formstring, "|", paste(fcond, collapse= "+"))
	.activevars <<- c(fmarg, fcond)
	
	return(formstring)
  }


  setuphilite <- function(formula) {
	if (is.null(odata$hilite)) odata$hilite <- FALSE
	
	formulahil <- NULL
	dividerhil <- NULL
		
	if (sum(odata$hilite, na.rm=T) > 0) {
  #  browser()
	  form <- parse_product_formula(formula)
	  fmarg <- c("hilite", form$marg)
	  fcond <- form$cond
	  
	  formstring <- paste(form$wt,"~ ")
  
	  if (length(fmarg) > 0) formstring <- paste(formstring, paste(fmarg, collapse= "+"))
	  else formstring <- paste(formstring,"1")
		  
	  if (length(fcond) > 0) formstring <- paste(formstring, "|", paste(fcond, collapse= "+"))
  
	  formulahil <- as.formula(formstring)

	  dvd <- rev(rev(divider)[1:.level])
	  if (dvd[1] %in% c("hspine", "hbar")) dividerhil <- c("vspine",dvd)
	  else if (dvd[1] %in% c("vspine", "vbar")) dividerhil <- c("hspine",dvd)
	}	
	return(list(formulahil, dividerhil))
  }

#  hils <- setuphilite(formula=as.formula(extract.formula(formula)))  
#  formulahil <<- hils[[1]]
#  dividerhil <<- hils[[2]]
  
#  datahil <- productplots:::prodcalc(odata, formulahil, dividerhil, cascade, scale_max, na.rm = na.rm)

#browser()

  if (!missing(subset)) {
    sel <- eval(substitute(subset), data, parent.frame())
    data <- data[sel & !is.na(sel), ]
  }

  .bgcolor<-"grey80"
  .queryPos <- NULL
  .startBrush <- NULL
  .endBrush <- NULL
  .level <- max(data$level)
  
  .df.title <- FALSE
  .clevel <- 0
  form <- parse_product_formula(formula)
  .activevars <- c(form$marg, form$cond)
  .hilitingchanged <- TRUE
  .brush <- FALSE
  
  top <- data$t
  bottom <- data$b
  left <- data$l
  right <- data$r

  if (is.null(main)) .df.title <- TRUE
  xlab <- find_x_label(data)
  ylab <- find_y_label(data)

  dataRanges <- c(make_data_ranges(c(min(left), max(right))),make_data_ranges(c(min(bottom),max(top))))
#  bprint(dataRanges)

# space in window around plot (margins in base R)  
# this space depends on the labels needed on the left
# find out about these first:

  row <- productplots:::find_row_level(data)
  ylabels <- NULL
  if (!is.na(row))
  	ylabels <- productplots:::row_labels(data[data$level == row, ])

  if (.df.title) main <- as.character(formula)
  windowRanges <- make_window_ranges(dataRanges, xlab, ylab, ytickmarks=ylabels, main=main)

  lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])

  coords <- function(item, painter, exposed) {
	sx <- scale_x_product(data)
	sy <- scale_y_product(data)

	# grey background with grid lines
    draw_grid_with_positions_fun(painter, dataRanges, sx$breaks, sy$breaks, sx$minor_breaks, sy$minor_breaks)
	
	# put labels, if appropriate
	col <- productplots:::find_col_level(data)
	if (!is.na(col)) {
	  labels <- productplots:::col_labels(data[data$level == col, ])
	  
	  draw_x_axes_with_labels_fun(painter, dataRanges, axisLabel=labels$label, labelHoriPos=labels$pos, name=xlab)
	} else {
	  draw_x_axes_with_labels_fun(painter, dataRanges, axisLabel=rep("",length(sx$breaks)), labelHoriPos=sx$breaks, name=xlab)
	}

	if (!is.na(row)) {
	  labels <- productplots:::row_labels(data[data$level == row, ])
	  draw_y_axes_with_labels_fun(painter, dataRanges, axisLabel=labels$label, labelVertPos=labels$pos, name=ylab)
	  
	} else {
	  draw_y_axes_with_labels_fun(painter, dataRanges, axisLabel=rep("",length(sy$breaks)), labelVertPos=sy$breaks, name=ylab)
	}
	
  }


  mosaic.all <- function(item, painter, exposed) {
    all.data <- subset(data, level==.level)
    
    top <- all.data$t
    bottom <- all.data$b
    left <- all.data$l
    right <- all.data$r

    qdrawRect(painter,
      left, 
      bottom, 
      right,
      top, 
      fill=colour)

	if (.df.title) add_title_fun(painter, dataRanges, title=extract.formula(formula))
  }

  drawInfoString <- function(item, painter, exposed, ...) {
	  x <- .queryPos[1]
	  y <- .queryPos[2]

	  info <- subset(data, (y <= t) & (y >= b) & (x <= r) & (x >=l) & (level == .level))
#	  print(str(info))
	  if (nrow(info)>0) {
	    idx <- setdiff(names(data),c("l","t","r","b", ".wt","level", "hilite"))[1:.level]
	    
	    infostring <- character()
	    infodata <- as.character(unlist(info[1,idx]))
	    infostring <- paste(idx, infodata,collapse="\n", sep=":")
#	    print(infodata)
	    
   	    qstrokeColor(painter) <- "white"
		qdrawText(painter,
		  infostring,
		  x, y, 
		  valign="top",
		  halign="left")
	  }  
  }
  
  drawBrush <- function(item, painter, exposed) {
	  left = min(.startBrush[1], .endBrush[1])
	  right = max(.startBrush[1], .endBrush[1])
	  top = max(.startBrush[2], .endBrush[2])
	  bottom = min(.startBrush[2], .endBrush[2])

	  qdrawRect(painter,
		left, 
		bottom, 
		right,
		top, 
	    fill=rgb(0,0,0,alpha=0.3),
	    stroke="black")  
  }
  
  recalchiliting <- function() {
	hils <- setuphilite(formula=as.formula(extract.formula(formula)))  

	formulahil <<- hils[[1]]
	dividerhil <<- hils[[2]]
	datahil <<- productplots:::prodcalc(odata, formulahil, dividerhil, cascade, scale_max, na.rm = na.rm)
  }

  hilite <- function(item, painter, exposed, ...) {
    if (TRUE) {
      if (.hilitingchanged) {
	  	recalchiliting()
	  	.hilitingchanged <<- FALSE
      }

	  if (.brush) hdata <- subset(data, (hilite==TRUE) & (level == (.level)))
	  else hdata <- subset(datahil, (hilite==TRUE) & (level == (.level+1)))
	  
	  if (nrow(hdata)>0) {
	  
		top <- hdata$t
		bottom <- hdata$b
		left <- hdata$l
		right <- hdata$r
	
		qdrawRect(painter,
		  left, 
		  bottom, 
		  right,
		  top, 
		  fill="darkred")
	  }
    }

	if (!is.null(.queryPos)) {
		drawInfoString(item, painter, exposed)
	}

	if (!is.null(.endBrush)) {
		drawBrush(item, painter, exposed)
	}

  }
	
  hover <- function(item, event, ...) { 
    .queryPos <<- as.numeric(event$pos())
#    bprint(.queryPos)
	qupdate(hilitelayer)
  }

  hover.leave <- function(item, event, ...) {
	.queryPos <<- NULL
	qupdate(hilitelayer)
  }

  mousePressFun <- function(item, event, ...) {  
	#browser()
#	print("mousedown")
    .brush <<- TRUE
	if (is.null(.startBrush)) 
		.startBrush <<- as.numeric(event$pos())
	.queryPos <<- NULL
#	bprint(.startBrush)

	qupdate(hilitelayer)
  }

  setHiliting <- function() {
	left = min(.startBrush[1], .endBrush[1])
	right = max(.startBrush[1], .endBrush[1])
	top = max(.startBrush[2], .endBrush[2])
	bottom = min(.startBrush[2], .endBrush[2])

	data$hilite <<- (data$level == .level) & (data$l <= right) & (data$r >= left) & (data$b <= top) & (data$t >= bottom)  
	

  }

  getSelected <- function() {
	hdata <- subset(data, (hilite==TRUE) & (level == .level))[,.activevars]


	if (length(.activevars) >=2) {
#	browser()
	  conds <- adply(hdata, 1, function (x) {
	  	cond <- adply(cbind(names(x),as.character(unlist(x))), 1, function(y) {
			cstr <- ""
	  		if (is.na(y[2])) cstr <- paste("is.na(",y[1],")", sep="")
	  		else cstr <- paste("(",y[1],"=='",y[2],"')",sep="")
	  		return(cstr)
	  	}) 
		return(paste(cond$V1, collapse=" & "))
	  })
	} else {
	  conds <- ldply(hdata, function (x) {
	    cond <- paste(.activevars ,paste("'",as.character(unlist(x)),"'",sep=""), sep="==")
	    cond[which(is.na(x))] <- paste("is.na(",.activevars[1],")", sep="")
	    return(paste(cond, collapse=" & "))
	  })
	} 
	cond1 <- paste("(",conds[,ncol(conds)],")", sep="", collapse=" | ")

	idx <- with(odata, which(eval(parse(text=cond1))))

#	print(cond1)
#	print("hilited")
#	print(length(idx))
	return(idx)
  }
  
  drag <- function(item, event, ...) {  
	#browser()
#	print("dragging")
	.endBrush <<- as.numeric(event$pos())

	setHiliting()	

	qupdate(hilitelayer)
  }

  mouseReleaseFun <- function(item, event, ...) {	
	.endBrush <<- as.numeric(event$pos())
	setHiliting()	

	odata$hilite <<- FALSE
	odata$hilite[getSelected()] <<- TRUE
    .brush <<- FALSE
    .hilitingchanged <<- TRUE
	
	qupdate(hilitelayer)
	
    .startBrush <<- NULL
    .endBrush <<- NULL
  }

  
  keyPressFun <- function(item, event, ...) {
#	print(event$key())
	key <- event$key()

	datachanged <- FALSE
	formulachanged <- FALSE
	form <- parse_product_formula(formula)
	
	if (key == Qt$Qt$Key_Up) {		# arrow up
	  if (.level > 1) {
	    .level <<- .level - 1
	    .hilitingchanged <<- TRUE
	  }
	}
	if (key == Qt$Qt$Key_Down) {		# arrow down
	  if (.level < max(data$level)) {
	    .level <<- .level + 1
	    .hilitingchanged <<- TRUE
	  }
	}
	if (key == Qt$Qt$Key_Left) {		# arrow left
	# move variable into mosaic plot from left
	  if (.level < max(data$level)) {
	    lindx <- max(data$level)-.level+1
	 
		marg_n <- length(form$marg)
	    vars <- c(form$marg, form$cond)
		vars <- rev(c(rev(vars[-lindx]),vars[lindx]))
		form$marg <- vars[1:marg_n]
		form$cond <- setdiff(vars, form$marg)

		formulachanged <- TRUE
	  }
	}

	if (key == Qt$Qt$Key_C) { 	# 'c' or 'C' for 'condition'
	  if (.clevel < max(data$level)) {
	  	.clevel <<- .clevel+1

	    vars <- c(form$marg, form$cond)
		form$cond <- rev(rev(vars)[1:.clevel])
	    form$marg <- setdiff(vars, form$cond)	  	
		formulachanged <- TRUE
	  }
	}

	if (key == Qt$Qt$Key_U) { 	# 'u' or 'U' for 'unconditioning'
	#  if (lindx < max(data$level)) {
	  if (.clevel > 0) {
	  	.clevel <<- .clevel-1
	    vars <- c(form$marg, form$cond)
	    if (.clevel == 0) {
	    	form$cond <- character(0)
	    	form$marg <- vars
	    } else {
			form$cond <- rev(rev(vars)[1:.clevel])
		    form$marg <- setdiff(vars, form$cond)	  	
	    }

		formulachanged <- TRUE
	  }
	}

	if (key == Qt$Qt$Key_R) { 	# 'r' or 'R' for 'rotate'
		lindx <- max(data$level)-.level + 1

	  if (divider[lindx] %in% c('hbar','vbar')) 
	  	divider[lindx] <<- setdiff(c('hbar','vbar'),divider[lindx])
	  if (divider[lindx] %in% c('hspine','vspine')) 
	  	divider[lindx] <<- setdiff(c('hspine','vspine'),divider[lindx])
	  
		datachanged <- TRUE

#browser()
	}
	if (key == Qt$Qt$Key_S) { 	# 's' or 'S' for 'spine'
			lindx <- max(data$level)-.level + 1
	
	  if (divider[lindx] == 'vbar') 
	  	divider[lindx] <<- 'vspine'
	  if (divider[lindx] == 'hbar') 
	  	divider[lindx] <<- 'hspine'
	  
		datachanged <- TRUE
	}

	if (key == Qt$Qt$Key_B) { 	# 'b' or 'B' for 'bar'
			lindx <- max(data$level)-.level + 1
	
	  if (divider[lindx] == 'vspine') 
	  	divider[lindx] <<- 'vbar'
	  if (divider[lindx] == 'hspine') 
	  	divider[lindx] <<- 'hbar'

		datachanged <- TRUE
	}

	if (formulachanged) {
		formstring <- paste(form$wt,"~ ")

		if (length(form$marg) > 0) formstring <- paste(formstring, paste(form$marg, collapse= "+"))
		else formstring <- paste(formstring,"1")
		
		if (length(form$cond) > 0) formstring <- paste(formstring, "|", paste(form$cond, collapse= "+"))
		
		formula <<- as.formula(formstring)
		datachanged <- TRUE
	}
	
	if (datachanged) {
	  .hilitingchanged <<- TRUE

	  data <<- productplots:::prodcalc(odata, formula, divider, cascade, scale_max, na.rm = na.rm)


	}

	# should be updating the data set, then start all fresh ...
	# need to figure out how to properly deal with hiliting of parts of the boxes

	qupdate(bglayer)
	qupdate(datalayer)
	qupdate(hilitelayer)
  }

  scene = qscene()
  
  bglayer = qlayer(scene, coords, cache = TRUE, limits = lims, clip=FALSE)
  datalayer = qlayer(scene, mosaic.all,  cache = TRUE, limits = lims, clip=FALSE)
  hilitelayer = qlayer(scene, hilite, hoverMoveFun=hover, 
  									  hoverLeaveFun = hover.leave, 
  									  mousePressFun=mousePressFun,
  									  keyPressFun=keyPressFun,
  									  mouseMoveFun=drag, 
  									  mouseReleaseFun=mouseReleaseFun,
  									  cache = TRUE, limits = lims, clip=FALSE)

  
  view = qplotView(scene = scene)
  view
}

happy$hilite <- happy$marital =="married"
plot1 <- qmosaic(happy, ~ health+sex+happy, c("vspine","hspine","hspine"))  
#print(plot1)

#plot1 <- qmosaic(happy, ~ health+sex+happy, c("fluct","hspine"))  
print(plot1)
#happym <- mutaframe(happy)
#qmosaic(happym, ~ health+sex+happy, c("vspine","hspine","hspine"))  

#qmosaic(mutaframe(happy), ~ health+sex+happy, c("vspine","hspine","hspine"))  

#tc <- as.data.frame(Titanic)
#plot1 <- qmosaic(tc, Freq~Survived+Sex+Class+Age, c("vspine","hspine","hspine","hspine"))
#print(plot1)