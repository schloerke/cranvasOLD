source("bin.r")

has_column <- function(data, col) {
	col %in% names(data)
}

is_mutaframe <- function(data) {
	"mutaframe" %in% class(data)
}

muta_coerce <- function(data) {
	
	if (!is_mutaframe(data)) {
		message("Making data into a 'Mutaframe'.")
		mutaframe(data)
	} else {
		data
	}
}

column_coerce <- function(data, column, defaultVal) {
	
	if (!has_column(data, column)) {
		data[[column]] <- defaultVal
	}
	data	
}


#' Create a hist plot
#' Create a hist plot from 1-D numeric data
#'
#' @param data vector of numeric data to be made into a histogram
#' @param horizontal boolean to decide if the bars are horizontal or vertical
#' @param ... arguments supplied to hist() or the hist layer
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @keywords hplot
#' @examples
#'	# toture
#'		qhist(rnorm(1000000), floor(rnorm(1000000)*3))
#'
#'		# each column is split evenly
#'		qhist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - stack") 
#'
#'		# each column has similar height colors
#'		qhist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - dodge", position = "dodge") 
#'
#'		# range from 0 to 1
#'		qhist(rnorm(1000000), floor(runif(1000000)*15), title = "Toture - relative", position = "relative") 
#'
#'  # color tests
#'		# all color is defined
#'		qhist(mtcars$disp, horizontal = TRUE, fill = "gold", stroke = "red4")
#'
#'		# stacked items
#'		qhist(mtcars$disp, mtcars$cyl, stroke = "black", position = "stack", title = "mtcars - stack")
#'
#'		# raw value items
#'		qhist(mtcars$disp, mtcars$cyl, stroke = "black", position = "identity")
#'
#'		# dodged items
#'		qhist(mtcars$disp, mtcars$cyl, stroke = "black", position = "dodge")
#'
#'		# range from 0 to 1
#'		qhist(mtcars$disp, mtcars$cyl, stroke = "black", position = "relative")
qhist <- function(
	data, 
	splitBy = rep(1, length(data)), 
	horizontal = TRUE,
	position = "none",
	color = NULL,
	fill = NULL,
	stroke = NULL,
	title = NULL,
	name = names(data),
	ash = FALSE,
	...
) {
	
	mf_data <- data
	mf_data <- muta_coerce(mf_data)
	mf_data <- column_coerce(mf_data, ".brushed", FALSE)
	n_data <- data.frame(mf_data)
	
	
	bars_info <- continuous_to_bars(n_data[,1], splitBy, position, color, fill, stroke, ...)
	# bars <- bars_info$data
	# color <- bars$color  
	
	
	# contains c(x_min, x_max, y_min, y_max)
	if (horizontal) {
		ranges <- c(make_data_ranges(c(0, bars_info$data$top)), make_data_ranges(bars_info$breaks))
	} else {
		ranges <- c(make_data_ranges(bars_info$breaks), make_data_ranges( c(0, bars_info$data$top)))
	}
	
	if (horizontal) {
		ylab = name
		xlab = "count"
	} else {
		ylab = "count"
		xlab = name
	}
	
	
	# Draw Axes --------------------------------
	coords <- function(item, painter, exposed) {
		# grey background with grid lines
		if (horizontal) {
			draw_grid_with_positions_fun(painter, ranges, horiPos = make_pretty_axes(ranges[1:2], ranges[1], ranges[2]))
		} else {
			draw_grid_with_positions_fun(painter, ranges, vertPos = make_pretty_axes(ranges[3:4], ranges[3], ranges[4]))
		}
		
		# put labels, if appropriate
		draw_x_axes_fun(painter, ranges, xlab)
		draw_y_axes_fun(painter, ranges, ylab)
		
		# title
		if(!is.null(title))
			add_title_fun(painter, ranges, title)
	}
	
	
	# Draw Bars ----------------------------------
	hist.all <- function(item, painter, exposed) {
		if (horizontal) {
			qdrawRect(painter,
				xleft = c(bars_info$data$bottom), #left
				ybottom = c(bars_info$data$left), # bottom 
				xright = c(bars_info$data$top), # right
				ytop = c(bars_info$data$right), # top
				stroke = c(bars_info$data$stroke),
				fill = c(bars_info$data$fill)# fill
			)
		} else {
			qdrawRect(painter,
				xleft = c(bars_info$data$left), #left
				ybottom = c(bars_info$data$bottom), # bottom 
				xright = c(bars_info$data$right), # right
				ytop = c(bars_info$data$top), # top
				stroke = c(bars_info$data$stroke),
				fill = c(bars_info$data$fill)# fill
			)
		}
	}	
	
	# # Brushing -----------------------------------------------------------------
	# .startBrush <- NULL
	# .endBrush <- NULL
	# .brush <- FALSE
	# 
	# drawBrush <- function(item, painter, exposed) {
	# 	left = min(.startBrush[1], .endBrush[1])
	# 	right = max(.startBrush[1], .endBrush[1])
	# 	top = max(.startBrush[2], .endBrush[2])
	# 	bottom = min(.startBrush[2], .endBrush[2])
	# 
	# 	qdrawRect(painter, left, bottom, right, top, 
	# 		fill=rgb(0,0,0,alpha=0.3), stroke="black")  
	# 	}
	# 
	# 	recalchiliting <- function() {
	# 		hils <- setuphilite(formula=as.formula(extract.formula(formula)))  
	# 
	# 		formulahil <<- hils[[1]]
	# 		dividerhil <<- hils[[2]]
	# 		df <- data.frame(odata)
	# 		
	# 		df$.brushed <- factor(df$.brushed, levels=c("TRUE","FALSE"))
	# 
	# 		if (!is.null(formulahil)) 
	# 			datahil <<- prodcalc(df, formulahil, dividerhil, cascade, scale_max, na.rm = na.rm)
	# 		else if (nrow(datahil)>0) datahil <<- datahil[-(1:nrow(datahil)),]
	# 	}
	# 
	# 	brushing_draw <- function(item, painter, exposed, ...) {
	# 		if (TRUE) {
	# 			if (.brush) {
	# 				#		if (is.null(data$.brushed)) 
	# 				#          data$.brushed <- FALSE
	# 				hdata <- subset(data, (.brushed==TRUE) & (level == (.level)))
	# 			} else {
	# 					recalchiliting()
	# 					#	if (is.null(datahil$.brushed)) datahil$.brushed <- FALSE
	# 					hdata <- subset(datahil, (.brushed==TRUE) & (level == (.level+1)))
	# 			}
	# 
	# 			if (nrow(hdata)>0) {
	# 
	# 				top <- hdata$t
	# 				bottom <- hdata$b
	# 				left <- hdata$l
	# 				right <- hdata$r
	# 
	# 				#  .brush.attr = attr(odata, '.brush.attr')
	# 
	# 				brushcolor <- .brush.attr[,".brushed.color"]   
	# 				qdrawRect(painter, left, bottom, right, top, fill=brushcolor)
	# 			}
	# 		}
	# 
	# 		if (!is.null(.endBrush)) {
	# 			drawBrush(item, painter, exposed)
	# 		}
	# 	}
	# 
	# 			brushing_mouse_press <- function(item, event, ...) {  
	# 				.brush <<- TRUE
	# 				if (is.null(.startBrush)) {
	# 					.startBrush <<- as.numeric(event$pos())      
	# 				}
	# 				qupdate(brushing_layer)
	# 			}
	# 
	# 			brushing_mouse_move <- function(item, event, ...) {  
	# 				.endBrush <<- as.numeric(event$pos())
	# 
	# 				setHiliting()
	# 				qupdate(brushing_layer)
	# 			}
	# 
	# 			brushing_mouse_release <- function(item, event, ...) {    
	# 				.endBrush <<- as.numeric(event$pos())
	# 				setHiliting()    
	# 				qupdate(brushing_layer)
	# 
	# 
	# 				.brush <<- FALSE
	# 
	# 
	# 				.startBrush <<- NULL
	# 				.endBrush <<- NULL
	# 
	# 				setSelected()
	# 
	# 				#    print("changed?")
	# 				#    print(summary(row.attr$.brushed))
	# 
	# 				#      recalchiliting()
	# 				#	  qupdate(brushing_layer)
	# 			}
	# 
	# 			setHiliting <- function() {
	# 				left = min(.startBrush[1], .endBrush[1])
	# 				right = max(.startBrush[1], .endBrush[1])
	# 				top = max(.startBrush[2], .endBrush[2])
	# 				bottom = min(.startBrush[2], .endBrush[2])
	# 
	# 				data$.brushed <<- (data$level == .level) & (data$l <= right) & 
	# 				(data$r >= left) & (data$b <= top) & (data$t >= bottom)  
	# 			}
	# 
	# 			setSelected <- function() {
	# 				hdata <- subset(data, (.brushed==TRUE) & (level == .level), drop=FALSE)[,.activevars, drop=FALSE]
	# 				if (nrow(hdata) > 0) {
	# 					hdata$ID <- 1:nrow(hdata)
	# 					res.melt <- melt(hdata,id.var="ID")
	# 					res.cond <- adply(res.melt, 1, function(x) {
	# 						if (is.na(x$value)) cstr <- paste("is.na(",x$variable,")", sep="")
	# 						else cstr <- paste("(",x$variable,"=='",x$value,"')",sep="")
	# 						return(cond=cstr)
	# 						})
	# 						res.cond <- res.cond[,-3]
	# 						names(res.cond)[3] <- "value"
	# 						cast.res <- cast(res.cond, ID~., function(x) return(paste(x, collapse=" & ")))
	# 
	# 						cond1 <- paste("(",cast.res[,2],")", sep="",collapse=" | ")
	# 						idx <- with(data.frame(odata), which(eval(parse(text=cond1))))
	# 
	# 						.brushed <- rep(FALSE, nrow(odata))
	# 						if (length(idx)) .brushed[idx] <- TRUE
	# 
	# 						odata$.brushed <- .brushed
	# 						} else {
	# 							odata$.brushed <- FALSE
	# 						}
	# 						#   idx <- with(data.frame(odata), which(eval(parse(text=cond1))))
	# 						#   return(idx)
	# 					}

	.bar_queryPos <- NULL
	bar_hover_draw <- function(item, painter, exposed, ...) {
		# Don't draw when brushing
		# if (.brush) return()
		cat("\nBar Hover Draw\n")
		# Don't draw when brushing
		if (is.null(.bar_queryPos)) return()
		
		if (horizontal) {
			x <- .bar_queryPos[2]
			y <- .bar_queryPos[1]
		} else {
			x <- .bar_queryPos[1]
			y <- .bar_queryPos[2]			
		}
		
		section <- subset(bars_info$data, (y <= top) & (y >= bottom) & (x <= right) & (x >=left))
		print(head(section))
		
		# Nothing under mouse
		if (nrow(section) == 0) return()
		
		# Highlight the rect
		qdrawRect(painter,
			xleft = c(section$bottom), #left
			ybottom = c(section$left), # bottom 
			xright = c(section$top), # right
			ytop = c(section$right), # top
			stroke = c("orange"),
			fill = c(NA)# fill
		)
		
		# Work out label text
		infostring <- paste("\nlabel:", section[1,"label"], "group:", section[1,"group"],collapse="\n", sep=" ")
		
		qstrokeColor(painter) <- "white"
		cat("\nDraw 'label: ", section[1,"label"],"\nx: ", .bar_queryPos[1], "  y: ", .bar_queryPos[2], "\n")
		qdrawText(painter, infostring, .bar_queryPos[1], .bar_queryPos[2], valign="top", halign="left")
	}
	
	bar_hover <- function(item, event, ...) {
		# if (.brush) return() 
		
		.bar_queryPos <<- as.numeric(event$pos())
		# qupdate(querylayer)
		
		cat("\nBar Hover\n")
		print(as.numeric(event$pos()))
		qupdate(hoverlayer)
	}
	bar_leave <- function(item, event, ...) {
		# if (.brush) return() 
		# 
		# .bar_queryPos <<- as.numeric(event$pos())
		# qupdate(querylayer)
		cat("\nBar Leave\n")
		print(as.numeric(event$pos()))
		qupdate(hoverlayer)
	}

	windowRanges <- make_window_ranges(ranges, xlab, ylab)
	lims <- qrect(windowRanges[c(1,2)], windowRanges[c(3,4)])
	
	scene = qscene()
	
	bglayer = qlayer(scene, coords, limits = lims, clip = FALSE
		# , keyPressFun=keyPressFun
	)
	
	datalayer = qlayer(scene, hist.all, limits = lims, clip = FALSE)


  hoverlayer = qlayer(scene, bar_hover_draw, limits = lims, clip = FALSE,
    hoverMoveFun = bar_hover, hoverLeaveFun = bar_leave)

	# brushing_layer = qlayer(scene, brushing_draw, 
	# 	# mousePressFun = brushing_mouse_press, mouseMoveFun = brushing_mouse_move,  
	# 	# mouseReleaseFun = brushing_mouse_release, 
	# 	limits = lims, clip = FALSE
	# )

	# querylayer = qlayer(scene, query_draw, limits = lims, clip = FALSE,
	# 	# hoverMoveFun = query_hover, hoverLeaveFun = query_hover_leave
	# )

	# # update the brush layer in case of any modifications to the mutaframe
	# if (is.mutaframe(odata)) {
	# 	add_listener(odata, function(i,j) {
	# 		if (j == ".brushed") {
	# 			qupdate(brushing_layer)
	# 		}
	# 	})
	# }
	# add_listener(.brush.attr, function(i, j) {
	# 	# wouldn't need to call recalchiliting ...
	# 	qupdate(brushing_layer)
	# })
	
	qplotView(scene = scene)
}