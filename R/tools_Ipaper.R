listk <- function (...) {
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)       
    Id_noname <- if (is.null(vars))
        seq_along(cols)       
    else which(vars == "")    
    if (length(Id_noname) > 0)
        vars[Id_noname] <- sapply(cols[Id_noname], deparse) 
    x <- setNames(list(...), vars)
    return(x)
}

last_lgl <- function(x) {
    ind <- which(x)
    ind[length(ind)]
}

first_lgl <- function(x) {
    ind <- which(x)
    ind[1]
}

#' @export
write_grid <- function(obj, file) {
    rgdal::writeGDAL(obj$grid, file)
}

#' @importFrom purrr is_empty
which.notempty <- function(x) {
    which(!sapply(x, is_empty))
}

which.empty <- function(x) {
    which(sapply(x, is_empty))
}
