library(qtutils)
library(qtpaint)

qparallel = function(data, scale = I, col = "black", 
    horizontal = TRUE, axes = !is.null(colnames(data)), mar = c(0, 
        0, 0, 0)) {
    ## parameters for the brush
    # brush status
    .brushed = FALSE
    # brush color
    .bcolor = "black"
    # brush range: horizontal and vertical
    .brange = c(0.05, 0.05)
    # mouse position
    .bpos = c(NA, NA)
    # drag start
    .bstart = c(NA, NA)
    # move brush?
    .bmove = FALSE
    
    data = apply(data, 2, scale)
    p = ncol(data)
    n = nrow(data)
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
    lims = qrect(c(min(x) - xr * mar[2], max(x) + xr * mar[4]), 
        c(min(y) - yr * mar[1], max(y) + yr * mar[3]))
    # given x, calculate y
    f = function(x0, y) {
        if (x0 <= 1) 
            return(y[1])
        else {
            if (x0 >= p) 
                return(y[p])
            else {
                i = max(which(1:p <= x0))
                return((y[i + 1] - y[i]) * (x0 - i) + y[i])
            }
        }
    }
    qpcp = function(item, painter, exposed) {
        for (i in 1:nrow(data)) {
            qdrawLine(painter, x[i, ], y[i, ], stroke = col[i])
        }
    }
    pcpBrushStart = function(item, event) {
        .bstart <<- as.numeric(event$pos())
        
    }
    pcpMove = function(item, event) {
        .bmove <<- !.bmove
    }
    pcpIdentify = function(item, event) {
        pos = as.numeric(event$pos())
        # simple click: don't change .brange
        if (!all(pos == .bstart) && !.bmove) 
            .brange <<- pos - .bstart
        .brushed <<- rep(FALSE, n)
        if (horizontal) {
            #             xi = which.min(abs(x[1, ] - pos[1]))
            #             if (abs(xi - pos[1]) <= abs(.brange[1])) {
            #                 .brushed <<- abs(y[, xi] - pos[2]) <= abs(.brange[2])
            #             }
            # the brush covers any x's?
            xi = which(abs(x[1, ] - pos[1]) <= abs(.brange[1]))
            # x on the boundary of the brush
            xb = pos[1] + c(-1, 1) * abs(.brange[1])
            tmp0 = matrix(nrow = n, ncol = 2)
            for (i in 1:n) {
                tmp0[i, ] = c(f(xb[1], y[i, ]), f(xb[2], y[i, 
                  ]))
            }
            if (length(xi)) 
                tmp0 = cbind(tmp0, y[, xi, drop = FALSE])
            .brushed <<- apply(tmp0, 1, function(xx) (min(xx) < 
                pos[2] + abs(.brange[2])) & (max(xx) > pos[2] - 
                abs(.brange[2])))
        }
        else {
            #             yi = which.min(abs(y[1, ] - pos[2]))
            #             if (abs(yi - pos[2]) <= abs(.brange[2])) {
            #                 .brushed <<- abs(x[, yi] - pos[1]) <= abs(.brange[1])
            #             }
            yi = which(abs(y[1, ] - pos[2]) <= abs(.brange[2]))
            # x on the boundary of the brush
            yb = pos[2] + c(-1, 1) * abs(.brange[2])
            tmp0 = matrix(nrow = n, ncol = 2)
            for (i in 1:n) {
                tmp0[i, ] = c(f(yb[1], x[i, ]), f(yb[2], x[i, 
                  ]))
            }
            if (length(yi)) 
                tmp0 = cbind(tmp0, x[, yi, drop = FALSE])
            .brushed <<- apply(tmp0, 1, function(xx) (min(xx) < 
                pos[1] + abs(.brange[1])) & (max(xx) > pos[1] - 
                abs(.brange[1])))
        }
        .bpos <<- pos
        qupdate(pcp.brush)
    }
    pcpBrush = function(item, painter) {
        if (!any(is.na(.bpos))) {
            qlineWidth(painter) = 2
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - 
                .brange[2], .bpos[1] + .brange[1], .bpos[2] + 
                .brange[2], stroke = "green")
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
    # if(is.null(colnames(data))) colnames(data)=sprintf('V%d',1:ncol(data))
    # if(horizontal) qdrawText(painter,colnames(data),1:ncol(data),.5,'center','center')
    # else qdrawText(painter,colnames(data),min(data)- xr * mar[2]/2,1:ncol(data),'center','center')
    #     }
    scene = qscene()
    
    pcp = qlayer(scene, qpcp, cache = TRUE, limits = lims)
    pcp.brush = qlayer(scene, pcpBrush, mousePressFun = pcpBrushStart, 
        mouseReleaseFun = pcpIdentify, mouseDoubleClickFun = pcpMove, 
        mouseMove = pcpIdentify, cache = FALSE, limits = lims)
    #     qlayer(scene,pcpAxes,cache=TRUE,limits=lims)
    view = qplotView(scene = scene, opengl = TRUE)
    #overlay <- view$overlay()
    #if(axes) qlayer(overlay, pcpAxes)
    
    view
}
