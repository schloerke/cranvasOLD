
library(qtutils)
library(qtpaint)
#source("api-sketch.r")
#source("updated_scatter.R")

df1 <- data.frame(x = c(1:10), y = c(1:10))
under_df<-data.frame(x=NULL,y=NULL)

bbase <- c(0,0)
h <- 0.2
w <- 0.2

bcolor <- "red"

view_size <- function(item) {
  qboundingRect(qtpaint:::qpaintingView(item))[2, ]
}

draw_points <- function(item, painter, exposed) {
  circle <- qpathCircle(0, 0, min(view_size(item)) / 200)
  qstrokeColor(painter) <- NA
  
  qfillColor(painter) <- "black"
  qdrawGlyph(painter, circle, df1[, 1], df1[, 2])
}

draw_brush <- function(item, painter, exposed) {
  qfillColor(painter) <- "white"
  qstrokeColor(painter) <- bcolor
  qlineWidth(painter) <- 2
  qdrawRect(painter, bbase[1], bbase[2], bbase[1]+w, bbase[2]-h)
}

draw_brushed <- function(item, painter, exposed) {
  brect <- qrect(bbase[1], bbase[2], bbase[1]+w, bbase[2]-h)
  under_df <<- df1[qprimitives(points, brect),]
  circle <- qpathCircle(0, 0, min(view_size(item)) / 200)
  qstrokeColor(painter) <- NA
  qfillColor(painter) <- bcolor
  qdrawGlyph(painter, circle, under_df[,1], under_df[,2])
  qupdate(brushed2)
}

draw_brushed2<-function(item, painter, exposed) {
  if(dim(under_df)[1]>0) {
    circle <- qpathCircle(0, 0, min(view_size(item)) / 200)
    qstrokeColor(painter) <- NA
    qfillColor(painter) <- bcolor
    qdrawGlyph(painter, circle, under_df[,1], under_df[,2])
  }
}

moveBrush <- function(event) {
  if (!is.null(drag_start)) {
    drag_end <- event$screenPos
    mat <- qdeviceMatrix(event$item, event$view)
    size <- qmap(mat, drag_end) - qmap(mat, drag_start)

    w <<- abs(size[1])
    h <<- abs(size[2])
    
    pt <- drag_start
  } else {
    pt <- event$screenPos
  }

  mat <- qdeviceMatrix(event$item, event$view)
  pt <- qmap(mat, pt)
  bbase <<- pt
  
  qupdate(brush)
}

drag_start <- NULL
start_drag <- function(event) {
  drag_start <<- event$screenPos
}

end_drag <- function(event) {
  drag_start <<- NULL
}


if (exists("view")) qclose(view)

##we want to see linked brushing in this plot
shape<-"circle"
xrange<-c(1,10)
yrange<-c(1,10)
color1<-"black"

scene2 <- qgraphicsScene()
root2 <- qlayer(scene2)

view2 <- qplotView(scene = scene2)




scene <- qgraphicsScene()
root <- qlayer(scene)

view <- qplotView(scene = scene)

points2 <- qlayer(root2, draw_points)
qlimits(points2) <- qrect(range(df1[,1]), range(df1[,2]))

points <- qlayer(root, draw_points,
  mouseMove = moveBrush,
  mouseReleaseFun = end_drag,
  mousePressFun = start_drag)
qlimits(points) <- qrect(range(df1[,1]), range(df1[,2]))

brush <- qlayer(root, draw_brush)
qcacheMode(brush) <- "none"
qlimits(brush) <- qlimits(points)

brushed <- qlayer(root, draw_brushed)
qcacheMode(brushed) <- "none"
qlimits(brushed) <- qlimits(points)

overlay<-qoverlay(view2)

brushed2<-qlayer(root2,draw_brushed2)
qcacheMode(brushed2)<-"none"
qlimits(brushed2)<-qlimits(points2)

print(view)
view2

