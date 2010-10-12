##' Data Imputation
##'
##' Impute data by various methods.
##' @title Data Imputation
##' @param x the numeric data matrix
##' @param method imputation method; one of the following:
##' \describe{
##'   \item{below.min}{replace missing values by a value 20\\% below the mininum}
##' }
##' @return the imputed data
##' @author Yihui Xie <\url{http://yihui.name}>
na.impute = function(x, method = 'below.min') {
    apply(x, 2, function(xx) {
        if (any(is.na(xx))) {
            xx[is.na(xx)] = switch(method,
              'below.min' = min(xx, na.rm = TRUE) - 0.2 * diff(range(xx, na.rm = TRUE)))
        }
        xx
    })
}
