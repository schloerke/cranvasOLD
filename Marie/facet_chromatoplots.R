library(qtpaint)
library(plumbr)
library(xcms)
library(chromatoplots)

source("/home/marie/Documents/cranvas/utilities/api-sketch.r")
source("/home/marie/Documents/cranvas/utilities/helper.r")
source("/home/marie/Documents/cranvas/utilities/axes.r")


# @param wd root directory of experimental design
expStruc.display<-function(wd=NULL,...){

###PART 1
#format data into a data.frame object

# 1.A determine how many facets we want
# each row represents a treatment (e.g. control, condition_1, condition_2, etc)
# number of elements in each row is the number of datafiles for that condition
  facet_structure<-getFacetStructure(wd)
  
# 1.B Format the CDF data
  df<-formatAllData(facet_structure)
 
###PART 2
# draw the background and axes

#2.A determine data limits

    ranges<-c(getAllRanges(facet_structure, df, 1),getAllRanges(facet_structure,df,2))
    windowranges<-make_window_ranges(ranges)
    plot1<-make_new_plot(width=1200,height=1200,windowRanges=windowranges)
    xspan<-windowranges[2]-windowranges[1]
    yspan<-windowranges[4]-windowranges[3]
    Ygrids<-max(facet_structure$treatments.numExp)
    Xgrids<-length(facet_structure$treatments.numExp)
	
#2.B draw the factoring 
	for (i in 1:Xgrids){
		for (j in 1:Ygrids){
			margin<-getMargins(i,j,value=6.6,Xgrids,Ygrids,xspan,yspan)

#2.C draw the grids
			draw_grid_with_positions(plot1,dataRanges=c(windowranges[1]+margin[1],
						     windowranges[2]-(.02*xspan)-margin[2],
						     windowranges[3]+margin[3],
						     windowranges[4]-(0.01*yspan)- margin[4]),horiPos=getfacetPrettyRanges(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,margin=.02),vertPos=getfacetPrettyRanges(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,marigin=0.02), row=as.integer(i),col=as.integer(j))
   			draw_grid_axes(plot1,dataRanges=c(windowranges[1]+margin[1],
						     windowranges[2]-(.01*xspan)-margin[2],
						     windowranges[3]+margin[3],
						     windowranges[4]-(0.005*yspan)- margin[4]),row=as.integer(i),col=as.integer(j),
                                         maxRows=Xgrids,maxCols=Ygrids,xspan=xspan,yspan=yspan)
}}

	
#draw the axes layer
	axes<-function(item,painter){
     	qdrawRect(painter,xleft=1-(.15/Ygrids),ybottom=.15/Xgrids,xright=1-(.15/(2*Ygrids)),
				  ytop=1-(.15/(Xgrids)),stroke="grey80",fill="grey80")	
		qdrawRect(painter,xleft=.15/Ygrids,xright=1-(.15/Ygrids),ybottom=1-(.15/(2*Xgrids)),
				  ytop=1-(.15/(Xgrids)),stroke="grey80",fill="grey80")
		qstrokeColor(painter)<-"black"
		qfillColor(painter)<-"black"
		qdrawText(painter,text=c(1,2,3,4), x=c(.125,.33,.6,.8),
				  y=1-(.75*(.15/Xgrids)),halign="center",valign="center")
		qdrawText(painter,text=facet_structure$treatments,x=1-(.45*(.15/Ygrids)),
					y=1-c(.3,.6,.85),halign="right",valign="center",rot=-90)
		qdrawText(painter,text="time",x=.5, y=.15/(3*Xgrids),halign="center",valign="center")
		qdrawText(painter,text= "m/z",x=.15/(15*Ygrids),y=.5,halign="center",valign="center",rot=90)
		
	}
	
#add axis as an overlay layer	
    overlay<-plot1$view$overlay()
	axesOverlay<-qlayer(overlay,axes,limits=qrect(c(0,1),c(0,1)))
	return(plot1)


###Part 3
# draw the data
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


draw_grid_axes<-function(plotObj,dataRanges, row,maxRows,col,maxCols,xspan,yspan){
	if(row==maxRows){	
           draw_x_axes_with_labels(plotObj,dataRanges=dataRanges,axisLabels=getfacetPrettyRanges(i=row,j=col,Xgrids=maxRows,Ygrids=maxCols,windowranges=dataRanges,xspan=xspan,yspan=yspan),labelHoriPos=getfacetPrettyRanges(i=row,j=col,Xgrids=maxRows,Ygrids=maxCols,windowranges=dataRanges,xspan=xspan,yspan=yspan),name=NULL,row=row,col=col)

	}
        if(col==1){
   	   draw_y_axes_with_labels(plotObj,dataRanges=dataRanges,axisLabels=c(0,200,400,600,800),labelVertPos=c(0,200,400,600,800), name=NULL,row=row,col=col)
	}
}

getfacetPrettyRanges<-function(i,j,Xgrids,Ygrids,windowranges,xspan,yspan,margin=.04,...){
  print("xspan")
  print(xspan)
  print("yspan")
  print(yspan)
  if(j==1){
    print(paste("col=",j,sep=","))
    print(pretty(windowranges)[pretty(windowranges)> (.15*xspan) & pretty(windowranges)< ((1-margin)*xspan) ])
    return(pretty(windowranges)[pretty(windowranges)> (.15*xspan) & pretty(windowranges)< ((1-margin)*xspan) ])
  }
  if(j==Xgrids){
    print(paste("col=",j,sep=","))
    print(pretty(windowranges)[pretty(windowranges)> (.15*xspan) & pretty(windowranges)< ((1-margin -.15)*xspan) ])
    return(pretty(windowranges)[pretty(windowranges)> (.15*xspan) & pretty(windowranges)< ((1-margin -.15)*xspan) ])
  }
  else{
    print(pretty(windowranges)[pretty(windowranges)< ((1-margin-.15)*xspan) ])
    return(pretty(windowranges)[pretty(windowranges)< ((1-margin-.15)*xspan) ])
  }
}

