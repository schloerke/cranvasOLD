library(qtpaint)
scene = qscene()
figLayer = qlayer(scene)
titlePainter = function(item, painter) {
    qdrawText(painter, 'The Useless Title', 300, 37.5)
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
titleLayer = qlayer(figLayer, titlePainter, mousePressFun=mouseLocator, row=0,col=1)
yaxis = qlayer(figLayer, yAxisPainter,mousePressFun=mouseLocator,  row = 1)
plotLayer = qlayer(figLayer, plotPainter, mousePressFun=mouseLocator, row = 1, col = 1)
xaxis = qlayer(figLayer, xAxisPainter, mousePressFun=mouseLocator, row = 2, col = 1)
layout = figLayer$gridLayout()
layout$setRowFixedHeight(0, 75)
layout$setRowPreferredHeight(1, 400)
layout$setRowFixedHeight(2, 75)
layout$setColumnFixedWidth(0, 75)
layout$setColumnPreferredWidth(1, 600)
# layout$setRowStretchFactor(0, 0)
layout$setRowStretchFactor(1, 1)
# layout$setRowStretchFactor(2, 0)
# layout$setColumnStretchFactor(0, 0)
layout$setColumnStretchFactor(1, 1)
qplotView(scene = scene)

