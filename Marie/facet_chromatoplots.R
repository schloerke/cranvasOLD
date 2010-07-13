library(qtpaint)
library(plumbr)
library(xcms)
library(chromatoplots)

source("/home/marie/Documents/cranvas/utilities/api-sketch.r")
source("/home/marie/Documents/cranvas/utilities/helper.r")
source("/home/marie/Documents/cranvas/utilities/axes.r")

#source("/home/marie/Documents/cranvas/Marie/facet_chromatoplots.R")
#expStruc.display(wd="/home/marie/Documents/cranvas/Marie/expDesign")


#
# @param wd root directory of experimental design
expStruc.display<-function(wd=NULL,...){

###PART 1
#format data into a data.frame object

# 1.A determine how many facets we want
# each row represents a treatment (e.g. control, condition_1, condition_2, etc)
# number of elements in each row is the number of datafiles for that condition
  facet_structure<-getFacetStructure(wd)
  
# 1.B Format the CDF data
  df1<-formatAllData(facet_structure)
 
###PART 2
# draw the background and axes

#2.A determine data limits

    ranges<-c(getAllRanges(facet_structure, df1, 1),getAllRanges(facet_structure,df1,2))
    windowranges<-make_window_ranges(ranges)
    plot1<-make_new_plot(width=1200,height=1200,windowRanges=c(windowranges[1]-1000,windowranges[2],windowranges[3]-150,windowranges[4]))
    xspan<-windowranges[2]-windowranges[1]
    yspan<-windowranges[4]-windowranges[3]
    Ygrids<-max(facet_structure$treatments.numExp)
    Xgrids<-length(facet_structure$treatments.numExp)
	
#2.B draw the factoring 
	for (i in 1:Xgrids){
		for (j in 1:Ygrids){

#2.C draw the grids
			draw_grid_with_positions(plot1,dataRanges=c(windowranges[1],
						     windowranges[2]-(.02*xspan),
						     windowranges[3],
						     windowranges[4]-(0.02*yspan)),horiPos=getfacetPrettyRangesX(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,margin=.02),vertPos=getfacetPrettyRangesY(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,marigin=0.02), row=as.integer(i),col=as.integer(j))
   			draw_grid_axes(plot1,dataRanges=c(windowranges[1],
						     windowranges[2]-(.02*xspan),
						     windowranges[3],
						     windowranges[4]-(0.02*yspan)),row=as.integer(i),col=as.integer(j),
                                         maxRows=Xgrids,maxCols=Ygrids,xspan=xspan,yspan=yspan)

			temp<-getwd()
			setwd(paste(facet_structure$dir,facet_structure$treatments[i],sep="/"))
			if(j<=length(list.files() ) ){
	  			data<-matrix(nrow=round(dim(df1[[i]][[j]])[1]/100)-1,ncol=3)
				  for (k in 1:dim(data)[1]){
					data[k,1]<-df1[[i]][[j]][100*k,1]
    					data[k,2]<-df1[[i]][[j]][100*k,2]
    					data[k,3]<-df1[[i]][[j]][100*k,3]
  			}
  			plot1$add_layer(glyph(
                      			left=data[,1],
                      			bottom=data[,2],
                      			fill=col2rgb(rgb(1-(log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
   							1- (log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
                                        		0,
                                          		0.25),
                                   		T),
                      			stroke=col2rgb(rgb(1-(log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
   					 		1-(log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
                                         		0,
                                          		0.25),
                                     		T),
                      			size=1),
                  			row=as.integer(i),col=as.integer(j),
					mousePressFun=function(...){drawZoomed(df1[[i]][[j]])})          
 			setwd(temp)
		}
	}
}

#the axes layer
#TODO: generalize for all expDesign structure
axes<-function(item,painter){
	qdrawRect(painter,xleft=1-(.15/Ygrids),ybottom=.15/Xgrids+.01,xright=1-(.15/(2*Ygrids)),
			  ytop=1-(.15/(Xgrids)),stroke="grey80",fill="grey80")	
	qdrawRect(painter,xleft=.0635,xright=.96,ybottom=1-(.15/(2*Xgrids)),
			  ytop=1-(.15/(Xgrids)),stroke="grey80",fill="grey80")
	qstrokeColor(painter)<-"black"
	qfillColor(painter)<-"black"
	qdrawText(painter,text=c(1,2,3,4), x=c(.15,.39,.63,.865),
		  y=1-(.75*(.15/Xgrids)),halign="center",valign="center")
	qdrawText(painter,text=facet_structure$treatments,x=1-(.45*(.15/Ygrids)),
			y=c(.73,.41,.1),halign="right",valign="center",rot=-90)
	qdrawText(painter,text="time",x=.5, y=.15/(3*Xgrids),halign="center",valign="center")
	qdrawText(painter,text= "m/z",x=.15/(15*Ygrids),y=.5,halign="center",valign="center",rot=90)
}
	
#add axis as an overlay layer and size plotting area
#TODO: generalize for all expDesigns	
    overlay<-plot1$view$overlay()
    axesOverlay<-qlayer(overlay,axes,limits=qrect(c(0,1),c(0,1)))
    print(plot1)
    plot1$root$geometry<-qrect(15,26,822,525)
    return(plot1)
}

getFacetStructure<-function(wd,...){
  temp.1<-getwd()
  setwd(wd)
  treatments<-dir()
  treatments.numExp<-matrix(nrow=length(treatments),ncol=1)
  colnames(treatments.numExp)<-"numExp"
  dir<-getwd()
  for (i in 1:length(treatments)){
    setwd(treatments[i])
    treatments.numExp[i,1]<-length(dir())
    setwd(dir)
  }
  setwd(temp.1)
  self<-structure (list(dir=dir,treatments=treatments,treatments.numExp=treatments.numExp))
  self
}

formatAllData<-function(facet_structure,...){
  temp.1<-getwd()
  df<-vector("list",length(facet_structure$treatments))
  for (i in 1:length(facet_structure$treatments)){
    df[[i]]<-vector("list", facet_structure$treatments.numExp[i])
    setwd(paste(facet_structure$dir,facet_structure$treatments[i],sep="/"))
    for (j in 1:facet_structure$treatments.numExp[i]){
      df[[i]][[j]]<-as.data.frame(rawMat(loadSample(paste(facet_structure$dir,facet_structure$treatments[i],dir()[j],sep="/"))))
    }
  }
  setwd(temp.1)
  return(df)
}

getAllRanges<-function(data,df,col,...){
  min<-NULL
  max<-NULL
  for (i in 1:length(data$treatments)){
    for (j in 1:data$treatments.numExp[i]){
      min<-min(make_data_ranges(df[[i]][[j]][,col]),min)
      max<-max(make_data_ranges(df[[i]][[j]][,col]),max)
    }
  }
  return(c(min,max))
}

  
#determine margin values
getMargins<-function(i,j,value,Xgrids,Ygrids,xspan,yspan){
	margin<-c(0,0,0,0)
#value=.15; yspan=4.8; xspan=28.8
#margin=c(0,.35,.05,0)
	if(i==1){
		margin[4]<-yspan/value
	}
	if(i==Xgrids){
		margin[3]<-yspan/value
	}
	if(j==1){
		margin[1]<-xspan/value
	}
	if(j==Ygrids){
		margin[2]<-xspan/value
	}
	return(margin)
}

#TODO: generalize for all data ranges
draw_grid_axes<-function(plotObj,dataRanges, row,maxRows,col,maxCols,xspan,yspan){
	if(row==maxRows){	

           draw_x_axes_with_labels(plotObj,dataRanges=dataRanges,axisLabels=c(0,1000,2000,3000),labelHoriPos=c(0,1000,2000,3000),name=NULL,row=row,col=col)
	}
        if(col==1){
   	   draw_y_axes_with_labels(plotObj,dataRanges=dataRanges,axisLabels=c(0,200,400,600,800),labelVertPos=c(0,200,400,600,800), name=NULL,row=row,col=col)
	}
}

#TODO: generalize for all data ranges
getfacetPrettyRangesX<-function(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,margin=.04,...){

return(c(0,1000,2000,3000,4000))
}

#TODO: generalize for all data ranges
getfacetPrettyRangesY<-function(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,margin=.04,...){
return(c(0,200,400,600,800))
}


drawZoomed<-function(data,...){
	ranges<-c(range(data[,1]),range(data[,2]))
        windowranges<-make_window_ranges(ranges)
    	xspan<-windowranges[2]-windowranges[1]
	yspan<-windowranges[4]-windowranges[3]
  
	plot2<-make_new_plot(	width=1200,
				height=1200,
				windowRanges=c(windowranges[1]-450,
				windowranges[2],
				windowranges[3]-100,
				windowranges[4]))
	draw_grid_with_positions(	plot2,
					dataRanges=c(windowranges[1],
					windowranges[2]-(.02*xspan),
					windowranges[3],
					windowranges[4]-(0.02*yspan)),
					horiPos=c(0,1000,2000,3000,4000),
					vertPos=c(0,200,400,600,800),
					row=0L, col=0L)
	draw_x_axes_with_labels(plot2,dataRanges=c(	windowranges[1],
						     	windowranges[2]-(.02*xspan),
						     	windowranges[3],
						     	windowranges[4]-(0.02*yspan)),
				axisLabels=c(0,1000,2000,3000),
				labelHoriPos=c(0,1000,2000,3000),
				name=NULL,row=0L,col=0L)
	draw_y_axes_with_labels(plot2,dataRanges=c(	windowranges[1],
						     	windowranges[2]-(.02*xspan),
						     	windowranges[3],
						     	windowranges[4]-(0.02*yspan)),
				axisLabels=c(0,200,400,600,800),
				labelVertPos=c(0,200,400,600,800),
				name=NULL,row=0L,col=0L)
	plot2$add_layer(glyph(	left=data[,1],
                      		bottom=data[,2],
                      		fill=col2rgb(rgb(1-(log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
   						1- (log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
                                       		0,
                                      		0.25),
                                	T),
                      		stroke=col2rgb(rgb(1-(log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
   				 		1-(log(data[,3])-min(log(data[,3])))/(max(log(data[,3]))-min(log(data[,3]))) ,
                                       		0,
                                       		0.25),
                                	T),
                      		size=1),
                  		row=0L,col=0L,
				mousePressFun=function(event,...){	
					print(ls(event))
					#plot2$root$geometry<-qrect(85,53,770,480)
					plot2$root$geometry<-qrect(	(plot2$root$geometry$left()-0.1*plot2$root$geometry$width()),
									(plot2$root$geometry$top()-0.1*plot2$root$geometry$height()),
									(plot2$root$geometry$right()+0.1*plot2$root$geometry$width()),
									(plot2$root$geometry$bottom()+0.1*plot2$root$geometry$height()))
				})     
	axes<-function(item,painter){
		qstrokeColor(painter)<-"black"
		qfillColor(painter)<-"black"
		qdrawText(painter,text="time",x=.5, y=.025,halign="center",valign="center")
		qdrawText(painter,text= "m/z",x=.01,y=.5,halign="center",valign="center",rot=90)
	}
	overlay<-plot2$view$overlay()
    	axesOverlay<-qlayer(overlay,axes,limits=qrect(c(0,1),c(0,1)))
	print(plot2)
}
	

