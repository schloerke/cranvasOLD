#library(qtpaint)
#source("cranvas/Marie/api-sketch.r")
#source("cranvas/Marie/draw.R")


#' arrange facet
#' arrange faceting from data
#'
#' @param x
#' @param y
#' @param facets

qtfacet<-function(x,y,data,facets){
    ranges<-c(make_data_ranges(data[,x]),make_data_ranges(data[,y]))
	windowranges<-make_window_ranges(ranges)
	plot1<-make_new_plot(windowRanges=windowranges)
	
#determine the number of factoring grids
	Xgrids<-length(levels(factor(data[,facets[1]])))
	Ygrids<-length(levels(factor(data[,facets[2]])))
	xspan<-windowranges[2]-windowranges[1]
	yspan<-windowranges[4]-windowranges[3]
    for (i in 1:Xgrids){
		for (j in 1:Ygrids){
			#draw the grids
			draw_grid(plot1,dataRanges=c(windowranges[1],
										 windowranges[2]-(.003*xspan),
										 windowranges[3],
										 windowranges[4]-(0.003*yspan)),
					  row=as.integer(i),col=as.integer(j))
			#parse and draw data layer 
			left<-data[(data[,facets[1]]==levels(factor(data[,facets[1]]))[i]) & (data[,facets[2]]==levels(factor(data[,facets[2]]))[j]),][,x]
			bottom<-data[(data[,facets[1]]==levels(factor(data[,facets[1]]))[i]) & (data[,facets[2]]==levels(factor(data[,facets[2]]))[j]),][,y]
			plot1$add_layer(glyph(left=left,bottom=bottom,fill="black",stroke="black"),row=as.integer(i),col=as.integer(j))
		}
	}
	
	print(plot1)
														 
}