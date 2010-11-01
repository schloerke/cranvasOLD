# st<-read.csv("/Users/dicook/cranvas/code/files/Di/map-sample.csv") 
# st.all<-read.csv("/Users/dicook/cranvas/code/files/Di/cartogram-polygons.csv") 
# qmap(st)
# qmap(st.all, id="State", x="Longitude", y="Latitude")

qmap<-function(data, id="id", x="x", y="y", vars, ...) {

  #windowRanges <- make_window_ranges(ranges, xlab, ylab)
  cat(id, x, y, is.character(id), "\n")
  data.map <- data.frame(id=data[,id], x=data[,x], y=data[,y])

  xr <- range(data.map$x)
  yr <- range(data.map$y)
#  lims <- qrect(xr[1]-0.1*(xr[2]-xr[1]), yr[1]-0.1*(yr[2]-yr[1]), xr[1]+0.1*(xr[2]-xr[1]),yr[1]+0.1*(yr[2]-yr[1]))
  lims <- qrect(0, 0, 500, 500)
  cat(xr[1], xr[2], yr[1], yr[2], "\n")
  
  scene = qscene()
	
#  bglayer = qlayer(scene, coords, limits = lims, clip = FALSE
#		# , keyPressFun=keyPressFun
#  )
  
  mapdraw <- function(item, painter) {
    npolys<-length(unique(data.map$id))
    polyids<-unique(data.map$id)
    cat("num states ",npolys, "\n")
    for (i in 1:npolys) {
      cat("states ", i, polyids[i], "\n")
      sub <- subset(data.map, id == polyids[i])
      cat("sub ",sub[1,1], sub[1,2], sub[1,3], nrow(sub), "\n")
      x<-round((sub$x-xr[1])/(xr[2]-xr[1])*450+25,0)
      y<-round((sub$y-yr[1])/(yr[2]-yr[1])*450+25,0)
      qdrawPolygon(painter, x, y, stroke="black", fill="grey")
    }
  }
  datalayer = qlayer(scene, mapdraw, limits = lims, clip = FALSE)

  qplotView(scene = scene)

}
