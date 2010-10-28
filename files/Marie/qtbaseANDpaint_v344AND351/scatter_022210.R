#draw a scatterplot plot object
#layers include: data, axes
#same scaling (limits) applied to all
qtScatterplot<-function(x,y,xlim=NULL,ylim=NULL,color="black",shape="circle",size="small",alpha=1,fill="grey",...){
  #determine the limits for entire plot (data layers and axis)
  ranges<-getlims(x,y,xlim,ylim)   
  #create the plot
  #window size 500 x 500; xrange and yrange from above
  plot1<-new_plot(500,500,xrange=c(ranges[1],ranges[2]),yrange=c(ranges[3],ranges[4]))
  #for different representations of the data (shape, color, etc) pass vecor arguments for shape, color, x, y
  plot1$add_layer(getGlyph2(shape=shape,color=color,x=x,y=y))

  #handling for how to draw axis
  #if x/y lims includes "0", axis is drawn there
  #if x/y lims do not include zero, axis is drawn on bottom and left [TODO: handling for when x/y lims <0 (draw axes at top and right)]
  if(ranges[3]>=0){
    y_left=c(ranges[1],ranges[2])
    y_bottom=c(ranges[3]+0.1*(ranges[4]-ranges[3]),ranges[3]+0.1*(ranges[4]-ranges[3]))
    x_labelpos=ranges[3]+0.1*(ranges[4]-ranges[3])
  } else{
    y_left=c(ranges[1],ranges[2])
    y_bottom=c(0,0)
    x_labelpos=0
  }
  if(ranges[1]>= 0){
    x_left=c(ranges[1]+0.1*(ranges[2]-ranges[1]),ranges[1]+0.1*(ranges[2]-ranges[1]))
    x_bottom=c(ranges[3],ranges[4])
    y_labelpos=ranges[1]+0.1*(ranges[2]-ranges[1])
  }else{
    x_left=c(0,0)
    x_bottom=c(ranges[3],ranges[4])
    y_labelpos=0
  }
  #draw x and y axes!
  plot1$add_layer(glyph(type="line",left=x_left,bottom=x_bottom,stroke=fill))
  plot1$add_layer(glyph(type="line",left=y_left,bottom=y_bottom,stroke=fill))
  #draw x and y labels!
  for(i in round(ranges[1]):round(ranges[2])){
    plot1$add_layer(glyph(type="text",text=as.character(i),left=i,bottom=x_labelpos, stroke=fill,valign="top"))
  }
  for(i in round(ranges[3]):round(ranges[4])){
    plot1$add_layer(glyph(type="text",text=as.character(i),left=y_labelpos,bottom=i, stroke=fill,halign="right"))
  }

}

#handle glyph call
getGlyph2<-function(shape,x,y,color,...){
  if(shape=='circle'){
    return(glyph(bottom=y,left=x,fill=color,stroke=color))
  }else if(shape=='square'){
    return(glyph(type='rect',bottom=y,top=y+.1,left=x,right=x+0.1,fill=color,stroke=color))
  }else if(shape=='rectangle'){
    return(glyph(type='rect',bottom=y,top=y+.1,left=x,right=x+0.2,fill=color,stroke=color))
  }}

#generate limits from x & y values
getRange2<-function(col){
  min<-NULL
  max<-NULL
  for(i in 1:length(col)){
    min<-min(min,min(col[i]-0.25)) #extend so that glyph is visible
    max<-max(max,max(col[i]-0.25))
  }
return(c(min,max))
}

#handle limits from user input (either use input or generate from x & y values)
getlims<-function(x,y,xlim,ylim,...){
  if(is.null(xlim) && is.null(ylim)){
    return (c(getRange2(x),getRange2(y)))
  }else if(is.null(xlim) && !is.null(ylim)){
    return(c(getRange2(x),ylim))
  }else if(!is.null(xlim) && is.null(ylim)){
    return(c(xlim,getRange2(y))) 
  }else{
    return(c(xlim,ylim))
  } 
}

#handles user input (either data.frame object or x & y vectors) for plotting by qtScatterplot1
qtScatter<-function(s_Layers=NULL,x=NULL,y=NULL,xlim=NULL,ylim=NULL,color="black",shape="circle",size="small",alpha=1,fill="grey",...){
  if(is.null(s_Layers) && !is.null(x) && !is.null(y)){
    return(qtScatterplot(x,y,xlim,ylim,color,shape,size,alpha,fill))
  }
  else if(!is.null(s_Layers) && (is.null(x) && is.null(y))){
    return(qtScatterplot(x=get(s_Layers[1]),y=get(s_Layers[2]),xlim,ylim,color=s_Layers[3],shape=s_Layers[4],alpha,fill))
  }else{
    return("please enter either layers data.frame OR x and y values")
  }
}