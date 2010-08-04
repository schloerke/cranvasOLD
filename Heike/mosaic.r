source("utilities/api-sketch.r")
source("utilities/helper.r")
source("utilities/axes.r")
require(stringr)
require(productplots)

source("../Heike/hadley-productplots-83f5772/R/parse.r")

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


qmosaic <- function(data, formula, divider = mosaic(), cascade = 0, scale_max = TRUE, na.rm = FALSE, subset=NULL, colour="grey30", ...) {
  odata <- data
  data <- productplots:::prodcalc(odata, formula, divider, cascade, scale_max, na.rm = na.rm)
  if (is.null(data$hilite)) data$hilite <- FALSE


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

  top <- data$t
  bottom <- data$b
  left <- data$l
  right <- data$r

  
  xlab <- find_x_label(data)
  ylab <- find_y_label(data)

  dataRanges <- c(make_data_ranges(c(min(left), max(right))),make_data_ranges(c(min(bottom),max(top))))
  bprint(dataRanges)

# space in window around plot (margins in base R)  
# this space depends on the labels needed on the left
# find out about these first:

  row <- productplots:::find_row_level(data)
  ylabels <- NULL
  if (!is.na(row))
  	ylabels <- productplots:::row_labels(data[data$level == row, ])

#  if (is.null(main)) main <- as.character(formula)
  
  windowRanges <- make_window_ranges(dataRanges, xlab, ylab, ytickmarks=ylabels)

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
  
  hilite <- function(item, painter, exposed, ...) {
	if (!is.null(data$hilite)) {

	hdata <- subset(data, (hilite==TRUE) & (level == .level))
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
	
	qupdate(hilitelayer)
	
    .startBrush <<- NULL
    .endBrush <<- NULL
  }

  
  keyPressFun <- function(item, event, ...) {
	print(event$key())
	key <- event$key()
	# up - arrow: 16777235
	# down - arrow: 16777237
	# right: 16777236
	# left: 16777234

	datachanged <- FALSE
	formulachanged <- FALSE
	form <- parse_product_formula(formula)
	
	if (key == 16777235) {		# arrow up
	  if (.level > 1) .level <<- .level - 1
	}
	if (key == 16777237) {		# arrow down
	  if (.level < max(data$level)) .level <<- .level + 1
	}
	if (key == 16777234) {		# arrow left
	# move variable into mosaic plot from left
	  if (.level < max(data$level)) {
	    lindx <- max(data$level)-.level+1
	 
	    vars <- form$marg
		form$marg <- rev(c(rev(vars[-lindx]),vars[lindx]))

		formulachanged <- TRUE
	  }
	}

	if (key == 67) { 	# 'c' or 'C' for 'condition'
		lindx <- max(data$level)-.level +1
	#  if (.level < max(data$level)) {
	    bprint(formula)
	    vars <- form$marg[1:lindx]
	    form$cond <- c(setdiff(form$marg, vars),form$cond)
		form$marg <- vars

		bprint(formula)
		formulachanged <- TRUE
	#  }

#browser()
	}

	if (key == 85) { 	# 'u' or 'U' for 'unconditioning'
	#  if (lindx < max(data$level)) {
	    form$marg <- c(form$marg, form$cond)
	    form$cond <- character(0)

		formulachanged <- TRUE
	#  }

#browser()
	}

	if (key == 82) { 	# 'r' or 'R' for 'rotate'
		lindx <- max(data$level)-.level + 1

	  if (divider[lindx] %in% c('hbar','vbar')) 
	  	divider[lindx] <<- setdiff(c('hbar','vbar'),divider[lindx])
	  if (divider[lindx] %in% c('hspine','vspine')) 
	  	divider[lindx] <<- setdiff(c('hspine','vspine'),divider[lindx])
	  
		datachanged <- TRUE

#browser()
	}
	if (key == 83) { 	# 's' or 'S' for 'spine'
			lindx <- max(data$level)-.level + 1
	
	  if (divider[lindx] == 'vbar') 
	  	divider[lindx] <<- 'vspine'
	  if (divider[lindx] == 'hbar') 
	  	divider[lindx] <<- 'hspine'
	  
		datachanged <- TRUE
	}

	if (key == 66) { 	# 'b' or 'B' for 'bar'
			lindx <- max(data$level)-.level + 1
	
	  if (divider[lindx] == 'vspine') 
	  	divider[lindx] <<- 'vbar'
	  if (divider[lindx] == 'hspine') 
	  	divider[lindx] <<- 'hbar'

		datachanged <- TRUE
	}

	if (formulachanged) {
		bprint(formula)
		print(paste(form$wt,collapse= "+"))
		print(paste(form$marg,collapse= "+"))
		print(paste(form$cond,collapse= "+"))
		formstring <- paste(form$wt,"~", paste(form$marg, collapse= "+"))
		if (length(form$cond) > 0) formstring <- paste(formstring, "|", paste(form$cond, collapse= "+"))
		formula <<- as.formula(formstring)
		bprint(formula)

		datachanged <- TRUE
	}
	
	if (datachanged) {
	  data <<- productplots:::prodcalc(odata, formula, divider, cascade, scale_max, na.rm = na.rm)
      if (is.null(data$hilite)) data$hilite <<- FALSE	
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
  hilitelayer = qlayer(scene, hilite, hoverMoveEvent=hover, 
  									  hoverLeaveEvent = hover.leave, 
  									  mousePressFun=mousePressFun,
  									  keyPressFun=keyPressFun,
  									  mouseMoveFun=drag, 
  									  mouseReleaseFun=mouseReleaseFun,
  									  cache = TRUE, limits = lims, clip=FALSE)

  
  view = qplotView(scene = scene)
  view
}

plot1 <- qmosaic(happy, ~ health+sex+happy, c("vspine","hspine","hspine"))  
print(plot1)

plot1 <- qmosaic(happy, ~ health+sex+happy, c("fluct","hspine"))  
