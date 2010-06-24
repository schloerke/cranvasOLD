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
	Ygrids<-length(levels(factor(data[,facets[1]])))
	Xgrids<-length(levels(factor(data[,facets[2]])))

	

	xspan<-windowranges[2]-windowranges[1]
	yspan<-windowranges[4]-windowranges[3]
	
#draw the factoring 
	for (i in 1:Xgrids){
		for (j in 1:Ygrids){
			margin<-getMargins(i,j,value=0.15,Xgrids,Ygrids,xspan,yspan)
		    						
#draw the grids
#BUG: does not draw same number of rows/columns for all plots in facet
			draw_grid(plot1,dataRanges=c(windowranges[1]+margin[1],
										 windowranges[2]-(.003*xspan)-margin[2],
										 windowranges[3]+margin[3],
										 windowranges[4]-(0.003*yspan)- margin[4]),
					  row=as.integer(i),col=as.integer(j))
#parse and draw data layer 
			left<-data[(data[,facets[1]]==levels(factor(data[,facets[1]]))[j]) & (data[,facets[2]]==levels(factor(data[,facets[2]]))[i]),][,x]
			bottom<-data[(data[,facets[1]]==levels(factor(data[,facets[1]]))[j]) & (data[,facets[2]]==levels(factor(data[,facets[2]]))[i]),][,y]
			plot1$add_layer(glyph(left=left,bottom=bottom,fill="black",stroke="black"),row=as.integer(i),col=as.integer(j),
							userlimits=qrect(c(windowranges[1]-margin[1],windowranges[2]-(0.003*xspan)+margin[2]),
											 c(windowranges[3]-margin[3],windowranges[4]-(0.003*yspan)+margin[4])))
			
		}
	}
	
#draw the axes layer
	axes<-function(item,painter){
     	qdrawRect(painter,xleft=1-(.15/Ygrids),ybottom=.15/Xgrids,xright=1-(.15/(2*Ygrids)),
				  ytop=1-(.15/(Xgrids)),stroke="grey80",fill="grey80")	
		qdrawRect(painter,xleft=.15/Ygrids,xright=1-(.15/Ygrids),ybottom=1-(.15/(2*Xgrids)),
				  ytop=1-(.15/(Xgrids)),stroke="grey80",fill="grey80")
		qstrokeColor(painter)<-"black"
		qfillColor(painter)<-"black"
		qdrawText(painter,text=levels(factor(data[,facets[1]])), x=getfacetpos(Ygrids),
				  y=1-(.75*(.15/Xgrids)),halign="center",valign="center")
		qdrawText(painter,text=levels(factor(data[,facets[2]])),x=1-(.45*(.15/Ygrids)),
					y=1-getfacetpos(Xgrids),halign="right",valign="top",rot=-90)
		qdrawText(painter,text=colnames(data)[x],x=.5, y=.15/(2*Xgrids),halign="center",valign="center")
		qdrawText(painter,text=colnames(data)[y],x=.15/(2*Ygrids),y=.5,halign="center",valign="center",rot=90)
		
	}
	
#add axis as an overlay layer	
    overlay<-plot1$view$overlay()
	axesOverlay<-qlayer(overlay,axes,limits=qrect(c(0,1),c(0,1)))
	return(plot1)
	
}

#determine margin values
getMargins<-function(i,j,value,Xgrids,Ygrids,xspan,yspan){
	margin<-c(0,0,0,0)
	if(i==1){
		margin[4]<-value*yspan
	}
	if(i==Xgrids){
		margin[3]<-value*yspan
	}
	if(j==1){
		margin[1]<-value*xspan
	}
	if(j==Ygrids){
		margin[2]<-value*xspan
	}
	return(margin)
}

getfacetpos<-function(grids){
	pos<-NULL
	for (i in 1: grids){
		pos<-c(pos,((i-1)*(1/grids)+(.5*(1/grids))))
	}
	
	return(pos)
}
