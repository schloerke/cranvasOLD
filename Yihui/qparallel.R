# data: a data frame or matrix
# vars: variables to show in par-coords - can be a character vector (names), or a formula like '~ x1 + x2'
# scale: standardizing method - 'range' --> [0, 1]
# 			      'I' --> do nothing
# 			      'var' --> mean 0 var 1
# 			      'custom_function_name' --> use your own function (see examples.R)
# col: colour vector (will be cycled to the same length as nrow(data))
# horizontal: logical - arrange variables in horizontal or vertical direction
# boxplot: overlay boxplots on top of par-coords
# boxwex: width of boxplots
# jitter: NULL (no jittering) or a character vector to jitter variables (usually those categorical vars)
# amount: jitter amount
# mar: margin (in proportion to the whole canvas)
# verbose: print some extra information (mainly the time consumed in each step)
# 
# some brushing parameters are not put in the function arguments yet

qparallel = function(data, vars = names(data), scale = "range", 
    col = "black", horizontal = TRUE, boxplot = FALSE, boxwex, 
    jitter = NULL, amount = NULL, mar = c(0.1, 0.1, 0.1, 0.1), 
    verbose = getOption("verbose")) {
    ## parameters for the brush
    # brush status
    .brushed = FALSE
    # brush color
    .bcolor = "yellow"
    # background color
    .bgcolor = "grey80"
    # .bgcolor = rgb(0,0,0,0)
    # mouse position
    .bpos = c(NA, NA)
    # drag start
    .bstart = c(NA, NA)
    # move brush?
    .bmove = FALSE
    mar = rep(mar, length.out = 4)
    scale = switch(scale, range = function(x) {
        xna = x[!is.na(x)]
        if(all(x==x[1])) rep(.5,length(x)) else (x - min(xna))/(max(xna) - min(xna))
    }, var = function(x) if(all(x==x[1])) rep(0,length(x)) else base:::scale(x), I = function(x) x, get(scale))
    data = as.data.frame(data)
    if (!is.null(vars)) {
        if (class(vars) == "formula") 
            vars = attr(terms(vars, data = data), "term.labels")
        data = subset(data, select = vars)
    }
    else vars = names(data)
    # omit NA's
    data = na.omit(data)
    const.col=sapply(data, function(x) all(x==x[1]))
    if(any(const.col)) {
	data=data[,!const.col]
	vars=vars[!const.col]
	warning('removed constant column(s) ', paste(which(const.col), collapse=','))
    }
    # back up original data
    # odata = data
    data = sapply(data, as.numeric)
    p = ncol(data)
    n = nrow(data)
    col = rep(col, length.out = n)
    if (!is.null(jitter) && is.character(jitter)) {
        data[, jitter] = apply(data[, jitter, drop = FALSE], 
            2, base::jitter, amount = amount)
    }
    data = apply(data, 2, scale)
    # for boxplots
    bxpstats = apply(data, 2, function(x) boxplot.stats(x, do.conf = FALSE)$stats)
    if (missing(boxwex)) 
        boxwex = max(1/p, 0.2)
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
    tmpx = c(t(cbind(x, NA)))
    tmpy = c(t(cbind(y, NA)))
    nn = length(tmpx)
    tmpcol = rep(col, each = p + 1, length.out = nn - 1)
#     tmpx = c(t(x))
#     tmpy = c(t(y))
#     nn=length(tmpx)
#     tmpcol = rep(col, each = p, length.out = nn-1)
#     tmpcol[p*(1:(n-1))]=NA

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
    
    pcp_Segment = function(item, painter, exposed) {
        qdrawRect(painter, min(x), min(y), max(x), max(y), stroke = .bgcolor, 
            fill = .bgcolor)
        qdrawSegment(painter, xpretty, min(y), xpretty, max(y), 
            stroke = "white")
        qdrawSegment(painter, min(x), ypretty, max(x), ypretty, 
            stroke = "white")
        ntime = Sys.time()
        if (verbose) 
            message("(re)drawing pcp segments")
        qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], 
            tmpy[-1], stroke = tmpcol)
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    pcp_Boxplot = function(item, painter, exposed) {
        if (verbose) {
            message("Drawing boxplots")
            ntime = Sys.time()
        }
        qstrokeColor(painter) = "black"
        for (i in 1:p) {
            x0 = c(rep(i - boxwex/2, 6), i + boxwex/2, rep(i, 
                2))
            y0 = c(bxpstats[, i], rep(bxpstats[2, i], 2), bxpstats[c(1, 
                4), i])
            x1 = c(rep(i + boxwex/2, 5), i - boxwex/2, i + boxwex/2, 
                rep(i, 2))
            y1 = c(bxpstats[, i], rep(bxpstats[4, i], 2), bxpstats[c(2, 
                5), i])
            if (horizontal) {
                qdrawSegment(painter, x0, y0, x1, y1)
                qlineWidth(painter) = 3
                qdrawSegment(painter, x0[3], y0[3], x1[3], y1[3])
                qlineWidth(painter) = 1
            }
            else {
                qdrawSegment(painter, y0, x0, y1, x1)
                qlineWidth(painter) = 3
                qdrawSegment(painter, y0[3], x0[3], y1[3], x1[3])
                qlineWidth(painter) = 1
            }
        }
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    pcpBrushStart = function(item, event) {
        .bstart <<- as.numeric(event$pos())
    }
    pcpMove = function(item, event) {
        .bmove <<- !.bmove
        message("Brush status: ", ifelse(.bmove, "MOVE", "DRAW"))
    }
    pcpIdentify = function(item, event) {
        if (verbose) {
            message("identifying mouse location and looking for segments within range")
            ntime = Sys.time()
        }
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
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    pcpBrush = function(item, painter) {
        if (verbose) {
            message("drawing brushed segments")
            ntime = Sys.time()
        }
        
        if (!any(is.na(.bpos))) {
            qlineWidth(painter) = 2
            #qdash(painter)=c(1,3,1,3)
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - 
                .brange[2], .bpos[1] + .brange[1], .bpos[2] + 
                .brange[2], stroke = .bcolor)
        }
        if (sum(.brushed) >= 1) {
            qlineWidth(painter) = 3
            qstrokeColor(painter) = .bcolor
            x = x[.brushed, , drop = FALSE]
            y = y[.brushed, , drop = FALSE]
            tmpx = c(t(cbind(x, NA)))
            tmpy = c(t(cbind(y, NA)))
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], 
                tmpy[-1])
        }
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    pcpAxes = function(item, painter) {
        if (verbose) {
            message("drawing axes")
            ntime = Sys.time()
        }
        
        qfont(painter) = qfont(pointsize = 10)
        if (horizontal) {
            qdrawText(painter, vars, 1:p - seq(-mar[2], mar[4], 
                length.out = p)/(1 + mar[2] + mar[4]) * xr, min(data) - 
                (-mar[1]/(1 + mar[1] + mar[3])) * yr/2, "center", 
                "center")
            ytrans = ypretty - seq(-mar[1], mar[3], length.out = length(ypretty))/(1 + 
                mar[1] + mar[3]) * yr
            # qdrawText(painter, ifelse(ypretty<=max(y) & ypretty>=min(y), as.character(ypretty),''), 1+mar[2]/(1+mar[2]+mar[4])/2*xr, ytrans, 'center', 'center')
            # qdrawSegment(painter, 1+mar[2]/(1+mar[2]+mar[4])*3/4*xr, ytrans, 1+mar[2]/(1+mar[2]+mar[4])*xr, ytrans, stroke='black')
        }
        else {
            qdrawText(painter, vars, min(data) - (-mar[2]/(1 + 
                mar[2] + mar[4])) * xr/2, 1:p - seq(-mar[1], 
                mar[3], length.out = p)/(1 + mar[1] + mar[3]) * 
                yr, "center", "center")
            xtrans = xpretty - seq(-mar[2], mar[4], length.out = length(xpretty))/(1 + 
                mar[2] + mar[4]) * xr
            qdrawText(painter, ifelse(xpretty <= max(x) & xpretty >= 
                min(x), as.character(xpretty), ""), xtrans, min(y) + 
                mar[1]/(1 + mar[1] + mar[3])/2 * yr, "center", 
                "center")
        }
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    scene = qscene()
    pcp.main = qlayer(scene, pcp_Segment, limits = lims)
    if (boxplot) {
        pcp.boxplot = qlayer(scene, pcp_Boxplot, limits = lims)
    }
    pcp.brush = qlayer(scene, pcpBrush, mousePressFun = pcpBrushStart, 
        mouseReleaseFun = pcpIdentify, mouseDoubleClickFun = pcpMove, 
        mouseMove = pcpIdentify, limits = lims)
    
    pcp.axes = qlayer(scene, pcpAxes, limits = qrect(range(x), 
        range(y)))
    
    view = qplotView(scene = scene)
    view
}
