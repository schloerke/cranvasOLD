
##' Create a Mutaframe from Data with Several Attributes for Future Interaction
##'
##' Create a Mutaframe from Data with Several Attributes for Future Interaction
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
    all_in = function(x1, x2) {
	sapply(x1, '%in%', x2)
    }
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

f=function(x)x
