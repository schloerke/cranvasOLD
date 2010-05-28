
library(qtpaint)
source("/Users/marie/Documents/cranvas/Marie/api-sketch.r")

n<-1000
x<-rnorm(n,50,25)
y<-rnorm(n,50,25)
df<-data.frame(X=x,Y=y)
labeled_df<-NULL
labeled<-NULL
margin <- 5
adjust <- c(margin, -margin)


pointIdentifier <- function(item, event) {
	off <- 20
	rect <- qrect(0, 0, off*2, off*2)
	mat <- item$deviceTransform(event)$inverted()
	rect <- mat$mapRect(rect)
	pos <- event$pos()
	rect$moveCenter(pos)
	hits <- item$primitives(rect)
	hitmat <- as.matrix(df[hits,])
	posmat <- matrix(pos, ncol=2)
	labeled <<- rep(FALSE, nrow(df))
	labeled[hits][Biobase::matchpt(posmat, hitmat)[,1]] <<- TRUE
}

axes <- function(item, painter) {
	qfont(painter) <- qfont(pointsize=12)
	pos <- as.matrix(item$geometry) + adjust
	qdrawText(painter, colnames(df)[1], pos[2], pos[4], "right", "bottom")
	qdrawText(painter, colnames(df)[2], pos[1], pos[3], "left", "top")
}

plot1<-new_plot(600,400,xrange=range(df[,1]),yrange=range(df[,2]))
plot1$add_layer(mark=glyph(left=df[,1],bottom=df[,2],stroke=NA,
        fill=col2rgb(rgb(1,seq(0,1,length=nrow(df)),0,0.5),T)),hoverMove=pointIdentifier)
plot1$add_layer(glyph(left=labeled_df[,1],bottom=labeled_df[,2],fill="black"))
overlay<-plot1$view$overlay()
axesOverlay<-qlayer(overlay,axes)

print(plot1$view)

for(i in 1:1000){
	labeled_df<-df[labeled,]
	plot1$modify_layer(2,new_mark=text(text=rownames(labeled_df), left=labeled_df[,1],bottom=labeled_df[,2],stroke="black"),
					   new_limit=qrect(range(df[,]),range(df[,2])))
	Sys.sleep(1/60)
}

