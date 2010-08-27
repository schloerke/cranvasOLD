library(qtpaint)
scene = qscene()
figLayer = qlayer(scene,limits=qrect(0,0,675,550))
titlePainter = function(item, painter) {
    qdrawText(painter, 'The Useless Title', 300, 0,'center','bottom')
    tmp=as.matrix(titleLayer$boundingRect())
    qdrawRect(painter, tmp[1,1],tmp[1,2],tmp[2,1],tmp[2,2])
}
yAxisPainter = function(item, painter) {
    qdrawText(painter, as.character(pretty(1:400)), 37.5, pretty(1:400))
}
plotPainter = function(item, painter) {
    qdrawPoint(painter, runif(1000, 0, 600), runif(1000, 0, 400))
    qdrawRect(painter, 1, 1, 600-1, 400-1)
}
xAxisPainter = function(item, painter) {
    qdrawText(painter, as.character(pretty(1:600)), pretty(1:600), 37.5)  
}
mouseLocator = function(item, event){
    print(as.numeric(event$pos()))
}
titleLayer = qlayer(figLayer, titlePainter, mousePressFun=mouseLocator, row=2,col=1)
yaxis = qlayer(figLayer, yAxisPainter,mousePressFun=mouseLocator,row = 1,col=0)
plotLayer = qlayer(figLayer, plotPainter, mousePressFun=mouseLocator,row = 1, col = 1)
xaxis = qlayer(figLayer, xAxisPainter, mousePressFun=mouseLocator,row = 0, col = 1)
layout = figLayer$gridLayout()
layout$setRowMaximumHeight(0, 75)
layout$setRowMinimumHeight(0, 60)
layout$setRowPreferredHeight(1, 400)
layout$setRowMaximumHeight(2, 75)
layout$setRowMinimumHeight(2, 60)
layout$setColumnMaximumWidth(0, 75)
layout$setColumnMinimumWidth(0, 60)
layout$setColumnPreferredWidth(1, 600)
# layout$setRowStretchFactor(0, 0)
layout$setRowStretchFactor(1, 1)
# layout$setRowStretchFactor(2, 0)
# layout$setColumnStretchFactor(0, 0)
layout$setColumnStretchFactor(1, 1)

print(view<-qplotView(scene = scene))

