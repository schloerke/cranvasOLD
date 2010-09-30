##' Create a mutaframe from data with several attributes for future interaction
##'
##' Create a mutaframe from data with several attributes for future interaction:
##' first check if the names of some predefined row attributes (e.g. .color, .brushed)
##' exist in the data (will issue an error if this happens); then augment the ...
##' arguments to the data and convert the augmented data to a mutaframe; in the end
##' add some attributes to the mutaframe to control the appearance of elements for
##' interaction (e.g. the color of the brush).
##' @title Create a Mutaframe from Data with Several Attributes for Future Interaction
##' @param data a data frame (typically); it will be coerced to a data
##' frame
##' @param ... other attributes corresponding to rows such as colors,
##' sizes and so on
##' @return a mutaframe
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' iris0 = qmutaframe(iris, .color = 'red', .brushed = FALSE)
##' qparallel(iris0)
##' attr(iris0, '.brush.attr')$.brushed.size = 1
##' ## change the colors to green
##' iris0$.color = 'green'
##' ## or other random colors
##' iris0$.color = sample(1:8, nrow(iris), replace = TRUE)
##' ## 'brushing' by command line
##' for (i in 1:10) {
##'     iris0$.brushed = sample(c(TRUE, FALSE), nrow(iris), replace = TRUE)
##'     Sys.sleep(1)
##' }
##' ## change the brush color to green
##' attr(iris0, '.brush.attr')$.brush.color = 'green'
qmutaframe = function(data, ...) {
    if (!is.data.frame(data)) data = as.data.frame(data)
    ## check if the attribute exists
    ## row attributes needed by all plotting functions
    row_attrs=c('.color', '.size', '.brushed')
    ## once in a blue moon...
    conflict_attrs = row_attrs %in% colnames(data)
    if(any(conflict_attrs)) {
        stop(sprintf('variable names conflicts: %s already exist(s) in data',
                     paste(row_attrs[conflict_attrs], collapse = ', ')))
    }

    ## prevent converting from characters to factors
    old_opts = options(stringsAsFactors = FALSE)
    mf = mutaframe(data, ...)

    ## we need to store some attributes somewhere which are not corresponding to rows
    ## e.g. attrs related to the brush (scalars)
    attr(mf, '.brush.attr') = mutaframe(.brush.color = 'yellow', .brush.size = 1,
        .brushed.color = 'yellow', .brushed.size = 2, .brush.mode = 'none')
    ## here '.brush.mode' is explained in the documentation of mode_selection()

    ## and other possible attributes

    options(old_opts)

    mf
}
##' Logical operations under different selection mode.
##'
##' There are five selection modes:
##' \describe{
##'   \item{none}{ignore previous selection and completely start over}
##'   \item{and}{select the intersection, i.e. the objects that are selected by two successive brushing operations}
##'   \item{or}{select the union, i.e. any objects selected by all previous operations and the current operation}
##'   \item{xor}{toggle the selection}
##'   \item{not}{negation, i.e. exclude the objects under two successive brushing operations}
##' }
##' We can hold the key while brushing: A for 'and', O for 'or', X for 'xor' and N for 'not'.
##' @title Logical Operations Under Different Selection Mode
##' @param x logical: the previous selection status
##' @param y logical: the current selection status
##' @param mode the selection mode string; see Details
##' @return a logical vector indicating whether the objects are selected
##' @author Yihui Xie <\url{http://yihui.name}>
##' @examples
##' x1 = c(TRUE, TRUE, FALSE, FALSE)
##' x2 = c(FALSE, TRUE, TRUE, FALSE)
##' mode_selection(x1, x2, 'none')
##' mode_selection(x1, x2, 'and')
##' mode_selection(x1, x2, 'or')
##' mode_selection(x1, x2, 'xor')
##' mode_selection(x1, x2, 'not')
mode_selection = function(x, y, mode = 'none'){
    ## a series of logical operations
    ## if mode is not specified, return y, the current status
    switch(mode, none = y, and = x & y, or = x | y, xor = xor(x, y), not = x & !y,
           y)
}
