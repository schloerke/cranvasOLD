#qtbase v 0.8-4
#qtpaint v 0.7.9
#svn 425

qtpoint<-function(parent,x,y,color='black',size=5,alpha=1.0){
  layerID<-length(parent$root$childItems())+1
  mark<-glyph(left=x-parent$limits$left(),bottom=y-parent$limits$top(),fill=color,stroke=color,size=size,parent=parent)
  add_layer(parent,mark)
  modify_layer(layerID,parent,alpha)
}


qtrect<-function(parent,x,y,color="black",width=5,height=5,alpha=1.0){
  layerID<-length(parent$root$childItems())+1
  mark<-rect(left=x-(0.5*width)-parent$limits$left(),bottom=y-(0.5*height)-parent$limits$top(),width=width, height=height,fill=color,stroke=color,parent=parent)
  add_layer(parent, mark)
  modify_layer(layerID,parent,alpha)
}

