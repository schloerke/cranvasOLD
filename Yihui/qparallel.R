source("../utilities/interaction.R")

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

qparallel = function(data, vars = names(data), scale = "range", col = "black",
    horizontal = TRUE, boxplot = FALSE, boxwex, jitter = NULL, amount = NULL, mar = c(0.04,
        0.04, 0.04, 0.04), main, verbose = getOption("verbose")) {
    ## parameters for the brush
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
    .bmove = TRUE
    # the title string
    dataname = deparse(substitute(data))
    if (missing(main)) {
        main = paste("Parallel Coordinates Plot of", dataname)
    }
    # margins for the plot region
    mar = rep(mar, length.out = 4)
    scale = switch(scale, range = function(x) {
        xna = x[!is.na(x)]
        (x - min(xna))/(max(xna) - min(xna))
    }, var = base:::scale, I = identity, get(scale))
    data = as.data.frame(data)
    if (!is.null(vars)) {
        if (class(vars) == "formula")
            vars = attr(terms(vars, data = data), "term.labels")
        data = subset(data, select = vars)
    }
    else vars = names(data)
    ## TODO: handle missing values

    # constant columns (or nearly constants -- for safety with floating numbers)
    const.col = sapply(data, function(x) {
        x = na.omit(x)
        x = as.numeric(x)
        length(x) == 0 || diff(range(x)) < 1e-6
    })
    if (any(const.col)) {
        data = data[, !const.col]
        vars = vars[!const.col]
        warning("removed constant column(s) ", paste(which(const.col), collapse = ","))
    }
    # which columns are numeric? we don't want boxplots for non-numeric vars
    numcol = sapply(data, class) == "numeric"
    data = sapply(data, as.numeric)
    p = ncol(data)
    # we need >= 2 columns
    stopifnot(p > 1)
    n = nrow(data)
    col = rep(col, length.out = n)
    if (!is.null(jitter) && is.character(jitter)) {
        data[, jitter] = apply(data[, jitter, drop = FALSE], 2, base::jitter, amount = amount)
    }
    data = apply(data, 2, scale)
    # for boxplots
    bxpstats = apply(data, 2, function(x) boxplot.stats(x, do.conf = FALSE)$stats)
    # automatic box width
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
    lims = matrix(c(xspan + c(-1, 1) * xr * mar[c(2, 4)], yspan + c(-1, 1) * yr *
        mar[c(1, 3)]), 2)

    # creating starting and ending vectors, because indexing in real-time can be slow
    segx0 = as.vector(t.default(x[, 1:(p - 1)]))
    segx1 = as.vector(t.default(x[, 2:p]))
    segy0 = as.vector(t.default(y[, 1:(p - 1)]))
    segy1 = as.vector(t.default(y[, 2:p]))
    nn = n * (p - 1)
    segcol = rep(col, each = p - 1)

    ## use a mutaframe to store the interaction parameters
    # store the mutaframe in options() using the name plname -- any other more appropriate place??
    # plname is a string like plumbr.dataname, stored in global options
    # this 'option' will be created if it does not exist, otherwise just extract it from options() and use it later
    plname = paste("plumbr.", dataname, sep = "")
    if (is.null(getOption(plname))) {
        mf = mutaframe(brushed = rep(FALSE, n))
        attr(mf, "bcolor") = "yellow"

        # very nasty here... I hate eval()ing anything ! why does not R have a setOption() function beside getOption()?
        eval(parse(text = paste("options(\"", plname, "\"= mf)", sep = "")))
    }
    else {
        mf = getOption(plname)
    }
    # convention of notation:
    # pcp_Something means a drawing function for a layer; pcpSomething means an interaction; pcp.something is a layer object
    pcp_Grid = function(item, painter) {
        qdrawRect(painter, lims[1, 1], lims[1, 2], lims[2, 1], lims[2, 2], stroke = .bgcolor,
            fill = .bgcolor)
        qdrawSegment(painter, xtickloc, lims[1, 2], xtickloc, lims[2, 2], stroke = "white")
        qdrawSegment(painter, lims[1, 1], ytickloc, lims[2, 1], ytickloc, stroke = "white")
    }
    pcp_Segment = function(item, painter) {
        if (verbose) {
            ntime = Sys.time()
            message("drawing pcp segments")
        }
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
            x0 = c(rep(i - boxwex/2, 6), i + boxwex/2, rep(i, 2))
            y0 = c(bxpstats[, i], rep(bxpstats[2, i], 2), bxpstats[c(1, 4), i])
            x1 = c(rep(i + boxwex/2, 5), i - boxwex/2, i + boxwex/2, rep(i, 2))
            y1 = c(bxpstats[, i], rep(bxpstats[4, i], 2), bxpstats[c(2, 5), i])
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
        # on right click, we can resize the brush; left click: only move the brush
        if (event$button() == Qt$Qt$RightButton) {
            .bmove <<- FALSE
        }
        if (event$button() == Qt$Qt$LeftButton) {
            .bmove <<- TRUE
        }
    }
    pcpIdentify = function(layer, event) {
        if (verbose) {
            message("identifying mouse location and looking for segments within range")
            ntime = Sys.time()
        }
        pos = event$pos()
        .bpos <<- as.numeric(pos)
        # simple click: don't change .brange
        if (!all(.bpos == .bstart) && (!.bmove)) {
            .brange <<- .bpos - .bstart
        }
        # use an extra variable here instead of manipulating mf$brushed, which will cause updating pcp.brush
        .brushed = rep(FALSE, n)
        rect = qrect(matrix(c(.bpos - .brange, .bpos + .brange), 2, byrow = TRUE))
        hits = layer$locate(rect) + 1
        hits = ceiling(hits/(p - 1))
        .brushed[hits] = TRUE
        mf$brushed = .brushed
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
            qdrawRect(painter, .bpos[1] - .brange[1], .bpos[2] - .brange[2], .bpos[1] +
                .brange[1], .bpos[2] + .brange[2], stroke = .bcolor)
        }
        .brushed = mf$brushed
        if (sum(.brushed, na.rm = TRUE) >= 1) {
            qlineWidth(painter) = 3
            qstrokeColor(painter) = .bcolor
            x = x[.brushed, , drop = FALSE]
            y = y[.brushed, , drop = FALSE]
            tmpx = as.vector(t.default(cbind(x, NA)))
            tmpy = as.vector(t.default(cbind(y, NA)))
            nn = length(tmpx)
            qdrawSegment(painter, tmpx[-nn], tmpy[-nn], tmpx[-1], tmpy[-1])
        }
        if (verbose)
            message(format(difftime(Sys.time(), ntime)))
    }

    scene = qscene()
    root = qlayer(scene)
    # title
    pcp.title = qlayer(root, function(item, painter) {
        qdrawText(painter, main, (lims[1] + lims[2])/2, 0, "center", "bottom")
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), clip = FALSE, row = 0, col = 1)
    # y-axis
    pcp.yaxis = qlayer(root, function(item, painter) {
        qdrawText(painter, yticklab, 0.7, ytickloc, "right", "center")
        # qdrawSegment(painter, .92, ytickloc, 1, ytickloc, stroke='black')
    }, limits = qrect(c(0, 1), c(lims[3], lims[4])), clip = FALSE, row = 1, col = 0)
    # x-axis
    pcp.xaxis = qlayer(root, function(item, painter) {
        qdrawText(painter, xticklab, xtickloc, 0.9, "center", "top")
        xlabWidth = max(xr * mar[c(2, 4)], max(qstrWidth(painter, xticklab[c(1, length(xticklab))]))/2)
        lims[, 1] <<- xspan + c(-1, 1) * xlabWidth
        # qdrawSegment(painter,xtickloc,.92,xtickloc,1,stroke='black')
    }, limits = qrect(c(lims[1], lims[2]), c(0, 1)), clip = FALSE, row = 2, col = 1)
    pcp.grid = qlayer(root, pcp_Grid, limits = qrect(lims), clip = FALSE, row = 1,
        col = 1)
    pcp.main = qlayer(root, pcp_Segment, mousePressFun = pcpBrushStart, mouseReleaseFun = pcpIdentify,
        mouseMove = pcpIdentify, limits = qrect(lims), clip = FALSE, row = 1, col = 1)
    if (boxplot) {
        pcp.boxplot = qlayer(root, pcp_Boxplot, limits = qrect(lims), clip = FALSE,
            row = 1, col = 1)
    }
    pcp.brush = qlayer(root, pcpBrush, limits = qrect(lims), clip = FALSE, row = 1,
        col = 1)

    # update the brush layer in case of any modifications to the mutaframe
    add_listener(mf, function(i, j) {
        qupdate(pcp.brush)
    })

    layout = root$gridLayout()
    layout$setRowStretchFactor(0, 1)
    layout$setRowStretchFactor(1, 5)
    layout$setRowStretchFactor(2, 1)
    layout$setColumnStretchFactor(0, 1)
    layout$setColumnStretchFactor(1, 5)

    view = qplotView(scene = scene)
    view
}
