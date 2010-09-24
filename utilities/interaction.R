## add attributes (mutaframes) to data for future interaction
qmutaframe = function(data) {
    # check if the attribute exists
    has_attr = function(which) {
	!is.null(attr(data, which))
    }

    data = as.data.frame(data)
    data = as.mutaframe(data)
    n = nrow(data)

    if (!has_attr("row.attr")) {
	row.attr = mutaframe(.color = rep("black", n), .size = rep(1L, n), .shape = rep(1L, n), .brushed = logical(n))
	attr(data, "row.attr") = row.attr
    }

    data
}
## query row attributes
get_row_attr = function(data) {
    attr(data, "row.attr")
}
