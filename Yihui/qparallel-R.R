library(qtpaint)
# rm(list=ls(all=TRUE))

qparallel = function(data, vars = names(data), scale = "range", 
    col = "black", horizontal = TRUE, mar = c(0.1, 0.1, 0.1, 
        0.1)) {
    ## parameters for the brush
    # brush status
    .brushed = FALSE
    # brush color
    .bcolor = "yellow"
    # background color
    .bgcolor = "grey80"
    # mouse position
    .bpos = c(NA, NA)
    # drag start
    .bstart = c(NA, NA)
    # move brush?
    .bmove = FALSE
    mar = rep(mar, length.out = 4)
    scale = switch(scale, range = function(x) {
        xna = x[!is.na(x)]
        (x - min(xna))/(max(xna) - min(xna))
    }, var = base:::scale, I = function(x) x, get(scale))
    data = as.data.frame(data)
    if (!is.null(vars)) {
        if (class(vars) == "formula") 
            vars = attr(terms(vars, data = data), "term.labels")
        data = subset(data, select = vars)
    }
    else vars = names(data)
    # omit NA's
    data=na.omit(data)
    # back up original data
    odata = data
    data = apply(sapply(data, as.numeric), 2, scale)
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
    xpretty = pretty(x)
    ypretty = pretty(y)
    # brush range: horizontal and vertical
    .brange = c(xr, yr)/20
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
        qdrawRect(painter,min(x),min(y),max(x),max(y),stroke=.bgcolor,fill=.bgcolor)
        qdrawSegment(painter, xpretty,min(y),xpretty,max(y),stroke='white')
        qdrawSegment(painter, min(x),ypretty,max(x),ypretty,stroke='white')
        print(system.time({
        for (i in 1:n) {
            qdrawLine(painter, x[i, ], y[i, ], stroke = col[i])
        }
        }))
    }
    pcpBrushStart = function(item, event) {
        .bstart <<- as.numeric(event$pos())
        
    }
    pcpMove = function(item, event) {
        .bmove <<- !.bmove
        message("Brush status: ", ifelse(.bmove, "MOVE", "DRAW"))
    }
    pcpIdentify = function(item, event) {
        pos = as.numeric(event$pos())
        # simple click: don't change .brange
        if (!all(pos == .bstart) && !.bmove) 
            .brange <<- pos - .bstart
        .brushed <<- rep(FALSE, n)
        if (horizontal) {
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
            qdash(painter)=c(1,3,1,3)
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - 
                .brange[2], .bpos[1] + .brange[1], .bpos[2] + 
                .brange[2], stroke = .bcolor)
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
    pcpAxes = function(item, painter) {
        qfont(painter) = qfont(pointsize = 10)
        if (horizontal) {
            qdrawText(painter, colnames(data), 1:p - seq(-mar[2], 
                mar[4], length.out = p)/(1 + mar[2] + mar[4]) * 
                xr, min(data) - (-mar[1]/(1 + mar[1] + mar[3])) * 
                yr/2, "center", "center")
            ytrans = ypretty - seq(-mar[1], mar[3], length.out = length(ypretty))/(1 + mar[1] + mar[3]) * yr
            # qdrawText(painter, ifelse(ypretty<=max(y) & ypretty>=min(y), as.character(ypretty),''), 1+mar[2]/(1+mar[2]+mar[4])/2*xr, ytrans, "center", "center")
            # qdrawSegment(painter, 1+mar[2]/(1+mar[2]+mar[4])*3/4*xr, ytrans, 1+mar[2]/(1+mar[2]+mar[4])*xr, ytrans, stroke='black')
        }
        else {
            qdrawText(painter, colnames(data), min(data) - (-mar[2]/(1 + 
                mar[2] + mar[4])) * xr/2, 1:p - seq(-mar[1], 
                mar[3], length.out = p)/(1 + mar[1] + mar[3]) * 
                yr, "center", "center")
            xtrans = xpretty - seq(-mar[2], mar[4], length.out = length(xpretty))/(1 + mar[2] + mar[4]) * xr
            qdrawText(painter, ifelse(xpretty<=max(x) & xpretty>=min(x),as.character(xpretty),''), xtrans, min(y)+mar[1]/(1+mar[1]+mar[3])/2*yr,  "center", "center")
        }
    }
    scene = qscene()
    
    pcp = qlayer(scene, qpcp, cache = TRUE, limits = lims)
    pcp.brush = qlayer(scene, pcpBrush, mousePressFun = pcpBrushStart, 
        mouseReleaseFun = pcpIdentify, mouseDoubleClickFun = pcpMove, 
        mouseMove = pcpIdentify, cache = FALSE, limits = lims)
    
    pcp.axes = qlayer(scene, pcpAxes, cache = TRUE, limits = qrect(range(x), 
        range(y)))
    
    view = qplotView(scene = scene)
    view
}
