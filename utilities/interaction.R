###
##' Create a Mutaframe from Data with Several Attributes for Future Interaction
##'
##' Create a Mutaframe from Data with Several Attributes for Future Interaction
##' @title Create a Mutaframe from Data with Several Attributes for Future Interaction
##' @param data a data frame (typically); it will be coerced to a data frame
##' @param ... other attributes corresponding to rows such as colours, sizes and so on
##' @return a mutaframe
##' @author Yihui Xie
qmutaframe = function(data, ...) {
    if (!is.data.frame(data)) data = as.data.frame(data)
    ## check if the attribute exists
    all_in = function(x1, x2) {
	sapply(x1, '%in%', x2)
    }
    ## row attributes needed by all plotting functions
    row_attrs=c('.color', '.size', '.brushed')
    ## once in a blue moon...
    conflict_attrs = all_in(row_attrs, colnames(data))
    if(any(conflict_attrs)) {
        stop(sprintf('variable names conflicts: %s already exist(s) in data',
                     paste(row_attrs[conflict_attrs], collapse = ', ')))
    }
    ## all possible row attributes that _might_ be needed
    ## row_attrs=c(row_attrs, '.shape', '.ltype')
    ## dot_names=names(list(...))
    ## if (!is.null(dot_names) && all_in(row_attrs, dot_names)) {

    ## }
    mf = mutaframe(data, ...)

    mf
}
