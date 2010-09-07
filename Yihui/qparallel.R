#' Create a parallel co-ordinates plot
#' Create a parallel co-ordinates plot from a data frame or matrix, with each line representing a row
#'
#' @param data a data frame or matrix
#' @param vars variables to show in par-coords - can be a character vector (names), or a formula like '~ x1 + x2'
#' @param scale standardizing method - 'range' --> [0, 1], 'I' --> do nothing, 'var' --> mean 0 var 1, 'custom_function_name' --> use your own function (see examples.R)
#' @param col colour vector (will be cycled to the same length as nrow(data))
#' @param horizontal logical - arrange variables in horizontal or vertical direction
#' @param boxplot overlay boxplots on top of par-coords
#' @param boxwex width of boxplots
#' @param jitter NULL (no jittering) or a character vector to jitter variables (usually those categorical vars)
#' @param amount jitter amount
#' @param mar margin (in proportion to the whole canvas)
#' @param main the title
#' @param verbose print some extra information (mainly the time consumed in each step)
#' @author Yihui Xie \\url{http://yihui.name}

# some brushing parameters are not put in the function arguments yet

qparallel = function(data, vars = names(data), scale = "range", 
    col = "black", horizontal = TRUE, boxplot = FALSE, boxwex, 
    jitter = NULL, amount = NULL, mar = c(0.04, 0.04, 0.04, 0.04), 
    main = paste("Parallel Coordinates Plot of", deparse(substitute(data))), 
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
    # in fact, don't have to omit NA's (NA's will produce disconnected segments)
    # data = na.omit(data)
    const.col = sapply(data, function(x) {
        x = na.omit(x)
        length(x) && all(x == x[1])
    })
    if (any(const.col)) {
        data = data[, !const.col]
        vars = vars[!const.col]
        warning("removed constant column(s) ", paste(which(const.col), 
            collapse = ","))
    }
    # back up original data
    # odata = data
    # which columns are numeric? we don't want boxplots for non-numeric vars
    numcol = sapply(data, class) == "numeric"
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
        xtickloc = 1:p
        xticklab = vars
        ytickloc = pretty(y)
        yticklab = format(ytickloc)
    }
    else {
        x = data
        y = col(data)
        xtickloc = pretty(x)
        xticklab = format(xtickloc)
        ytickloc = 1:p
        yticklab = vars
    }
    xspan = range(x, na.rm = TRUE)
    yspan = range(y, na.rm = TRUE)
    xr = diff(xspan)
    yr = diff(yspan)
    
    # brush range: horizontal and vertical
    .brange = c(xr, yr)/20
    lims = matrix(c(xspan + c(-1, 1) * xr * mar[c(2, 4)], yspan + 
        c(-1, 1) * yr * mar[c(1, 3)]), 2)
    
    ## for large data, store these large vectors beforehand or process them real-time?
    segx0 = as.vector(t.default(x[, 1:(p - 1)]))
    segx1 = as.vector(t.default(x[, 2:p]))
    segy0 = as.vector(t.default(y[, 1:(p - 1)]))
    segy1 = as.vector(t.default(y[, 2:p]))
    nn = n * (p - 1)
    segcol = rep(col, each = p - 1)
    
    ## needs vectorization here!!
    # given x, calculate y
    f = function(x0, y) {
        if (x0 <= 1) 
            return(y[1])
        else {
            if (x0 >= p) 
                return(y[p])
            else {
                i = floor(x0)
                return((y[i + 1] - y[i]) * (x0 - i) + y[i])
            }
        }
    }
    
    pcp_Segment = function(item, painter) {
        qdrawRect(painter, lims[1, 1], lims[1, 2], lims[2, 1], 
            lims[2, 2], stroke = .bgcolor, fill = .bgcolor)
        qdrawSegment(painter, xtickloc, lims[1, 2], xtickloc, 
            lims[2, 2], stroke = "white")
        qdrawSegment(painter, lims[1, 1], ytickloc, lims[2, 1], 
            ytickloc, stroke = "white")
        ntime = Sys.time()
        if (verbose) 
            message("drawing pcp segments")
        qdrawSegment(painter, segx0, segy0, segx1, segy1, stroke = segcol)
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    pcp_Boxplot = function(item, painter) {
        if (verbose) {
            message("Drawing boxplots")
            ntime = Sys.time()
        }
        qstrokeColor(painter) = "black"
        for (i in (1:p)[numcol]) {
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
            .brushed = apply(tmp0, 1, function(xx) (min(xx) < 
                pos[2] + abs(.brange[2])) & (max(xx) > pos[2] - 
                abs(.brange[2])))
            .brushed[is.na(.brushed)] = FALSE
            .brushed <<- .brushed
            #                 print(.brushed)
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
        if (sum(.brushed, na.rm = TRUE) >= 1) {
            qlineWidth(painter) = 3
            qstrokeColor(painter) = .bcolor
            x = x[.brushed, , drop = FALSE]
            y = y[.brushed, , drop = FALSE]
            #             print(y)
            tmpx = as.vector(t.default(cbind(x, NA)))
            tmpy = as.vector(t.default(cbind(y, NA)))
            nn = length(tmpx)
            #             print(str(x))
            #             print(nn)
            #             print(str(tmpy))
            #             print(str(tmpx))
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], 
                tmpy[-1])
        }
        if (verbose) 
            message(format(difftime(Sys.time(), ntime)))
    }
    
    scene = qscene()
    root = qlayer(scene)
    # title
    pcp.title = qlayer(root, function(item, painter) {
        qdrawText(painter, main, (lims[1] + lims[2])/2, 0, "center", 
            "bottom")
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), clip = FALSE, 
        row = 0, col = 1)
    # y-axis
    pcp.yaxis = qlayer(root, function(item, painter) {
        qdrawText(painter, yticklab, 0.9, ytickloc, "right", 
            "center")
        # qdrawSegment(painter, .92, ytickloc, 1, ytickloc, stroke='black')
    }, limits = qrect(c(0, 1), c(lims[3], lims[4])), clip = FALSE, 
        row = 1, col = 0)
    # x-axis
    pcp.xaxis = qlayer(root, function(item, painter) {
        qdrawText(painter, xticklab, xtickloc, 0.9, "center", 
            "top")
        xlabWidth = max(xr * mar[c(2, 4)], max(qstrWidth(painter, 
            xticklab[c(1, length(xticklab))]))/2)
        lims[, 1] <<- xspan + c(-1, 1) * xlabWidth
        # qdrawSegment(painter,xtickloc,.92,xtickloc,1,stroke='black')
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), clip = FALSE, 
        row = 2, col = 1)
    
    pcp.main = qlayer(root, pcp_Segment, limits = qrect(lims), 
        clip = FALSE, row = 1, col = 1)
    if (boxplot) {
        pcp.boxplot = qlayer(root, pcp_Boxplot, limits = qrect(lims), 
            row = 1, col = 1)
    }
    pcp.brush = qlayer(root, pcpBrush, mousePressFun = pcpBrushStart, 
        mouseReleaseFun = pcpIdentify, mouseDoubleClickFun = pcpMove, 
        mouseMove = pcpIdentify, limits = qrect(lims), row = 1, 
        col = 1)
    
    
    layout = root$gridLayout()
    layout$setRowStretchFactor(0, 1)
    layout$setRowStretchFactor(1, 6)
    layout$setRowStretchFactor(2, 1)
    layout$setColumnStretchFactor(0, 1)
    layout$setColumnStretchFactor(1, 6)
    
    view = qplotView(scene = scene)
    view
} 
