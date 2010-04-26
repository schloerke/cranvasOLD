library(qtutils)
library(qtpaint)
options(warn = 2)

## brush status
.brushed = FALSE
## brush color
.bcolor = "black"
## brush range: horizontal and vertical
.brange = c(0.05, 0.05)
## mouse position
.bpos = c(NA, NA)
## drag start
.bstart = c(NA, NA)

qparallel = function(data, scale = I, col = "black", horizontal = TRUE, 
    axes=!is.null(colnames(data)), mar = c(0, 0, 0, 0)) {
    data = apply(data, 2, scale)
    col = rep(col, length.out = nrow(data))
    # switch x and y according to the direction
    if (horizontal) {
        x = col(data)
        y = data
    }
    else {
        x = data
        y = col(data)
    }
    xr = diff(range(x))
    yr = diff(range(y))
    lims = qrect(c(min(x) - xr * mar[2], max(x) + xr * mar[4]), c(min(y) - yr * 
        mar[1], max(y) + yr * mar[3]))
    qpcp = function(item, painter, exposed) {
        for (i in 1:nrow(data)) {
            qdrawLine(painter, x[i, ], y[i, ], stroke = col[i])
        }
    }
    pcpBrushStart = function(item, event) {
        .bstart <<- as.numeric(event$pos())
        
    }
    pcpIdentify = function(item, event) {
        pos = as.numeric(event$pos())
	# simple click: don't change .brange
        if (!all(pos == .bstart)) 
            .brange <<- pos - .bstart
        .brushed <<- rep(FALSE, nrow(data))
        if (horizontal) {
            xi = which.min(abs(x[1, ] - pos[1]))
            if (abs(xi - pos[1]) <= abs(.brange[1])) {
                .brushed <<- abs(y[, xi] - pos[2]) <= abs(.brange[2])
            }
        }
        else {
            yi = which.min(abs(y[1, ] - pos[2]))
            if (abs(yi - pos[2]) <= abs(.brange[2])) {
                .brushed <<- abs(x[, yi] - pos[1]) <= abs(.brange[1])
            }
            
        }
        .bpos <<- pos
        qupdate(pcp.brush)
    }
    pcpBrush = function(item, painter) {
        if (!any(is.na(.bpos))) {
            qlineWidth(painter) = 1
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2], 
                .bpos[1] + .brange[1], .bpos[2] + .brange[2], stroke = "green")
        }
        if (sum(.brushed) >= 1) {
            qlineWidth(painter) = 3
            x = x[.brushed, , drop = FALSE]
            y = y[.brushed, , drop = FALSE]
            
            col = col[.brushed]
            for (i in 1:nrow(x)) {
                qdrawLine(painter, x[i, ], y[i, ], stroke = .bcolor)
            }
        }
        
    }
#    pcpAxes=function(item, painter) {
#       qfont(painter) <- qfont(pointsize=12)
# 
# 	if(is.null(colnames(data))) colnames(data)=sprintf('V%d',1:ncol(data))
# 	if(horizontal) qdrawText(painter,colnames(data),1:ncol(data),.5,'center','center') else qdrawText(painter,colnames(data),min(data)- xr * mar[2]/2,1:ncol(data),'center','center')
#     }
    scene = qscene()
    
    pcp = qlayer(scene, qpcp, cache = TRUE, limits = lims)
    pcp.brush = qlayer(scene, pcpBrush, mousePressFun = pcpBrushStart, mouseReleaseFun = pcpIdentify, cache = FALSE, limits = lims)
#     qlayer(scene,pcpAxes,cache=TRUE,limits=lims)
    view=qplotView(scene = scene, opengl = TRUE)
    #overlay <- view$overlay()
    #if(axes) qlayer(overlay, pcpAxes)
    
    view
}

minmax = function(x) {
    xna = x[!is.na(x)]
    (x - min(xna))/(max(xna) - min(xna))
}

library(YaleToolkit)
data(NewHavenResidential)
nhr = sapply(NewHavenResidential, as.numeric)
nhr.sd = apply(nhr, 2, minmax)
qparallel(nhr.sd, col = c(rgb(1, 0, 0, 0.1), rgb(1, 1, 0, 0.1), rgb(0, 0, 1, 0.1))[nhr[, "zone"]], mar=rep(.05,4))

#qparallel(nhr.sd, col = c(rgb(1, 0, 0, 0.1), rgb(0, 0, 1, 0.1))[nhr[, 
#    "acType"]], horizontal = FALSE)
