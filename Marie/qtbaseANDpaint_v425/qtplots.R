#qtbase v 0.8-4
#qtpaint v 0.7.9
#svn 425

qtpoint<-function(parent,x,y,color='black',size=5,alpha=1.0){
  layerID<-length(parent$root$childItems())+1
  mark<-glyph(left=x,bottom=y,fill=color,stroke=color,size=size)
  add_layer(parent,mark,geometry=qrect(0,0,parent$root$geometry$width(),parent$root$geometry$height()))
  modify_layer(layerID,parent,alpha)
}


qtline<-function(parent,x,y,color='black',width,alpha=1.0){
