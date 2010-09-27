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
##' @param ... other attributes corresponding to rows such as colours,
##' sizes and so on
##' @return a mutaframe
##' @author Yihui Xie
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
    options(old_opts)

    ## we need to store some attributes somewhere which are not corresponding to rows
    ## e.g.
    attr(mf, '.brush.color') = 'yellow'

    mf
}
