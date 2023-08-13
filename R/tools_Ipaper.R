#' rdist.earth
#'
#' @param x1 A position matrix `[lat, lon]`, degree unit.
#' @param x2 same as x1
#'
#' @export
rdist.earth <- function(x1, x2 = NULL) {
  R <- 6378.388
  # coslat1 <- cos(x1[, 2])
  # sinlat1 <- sin(x1[, 2])
  # coslon1 <- cos(x1[, 1])
  # sinlon1 <- sin(x1[, 1])
  coslat1 <- cos((x1[, 2] * pi) / 180)
  sinlat1 <- sin((x1[, 2] * pi) / 180)
  coslon1 <- cos((x1[, 1] * pi) / 180)
  sinlon1 <- sin((x1[, 1] * pi) / 180)

  coslat2 <- cos((x2[, 2] * pi) / 180)
  sinlat2 <- sin((x2[, 2] * pi) / 180)
  coslon2 <- cos((x2[, 1] * pi) / 180)
  sinlon2 <- sin((x2[, 1] * pi) / 180)
  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*%
    t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
  return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
}



listk <- function(...) {
  cols <- as.list(substitute(list(...)))[-1]
  vars <- names(cols)
  Id_noname <- if (is.null(vars)) {
    seq_along(cols)
  } else {
    which(vars == "")
  }
  if (length(Id_noname) > 0) {
    vars[Id_noname] <- sapply(cols[Id_noname], deparse)
  }
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

#' @importFrom data.table is.data.table
reorder_name <- function(d, headvars = c(
                           "site", "date", "year", "doy", "d8",
                           "d16"
                         ), tailvars = "") {
  names <- names(d)
  headvars %<>% intersect(names)
  tailvars %<>% intersect(names)
  varnames <- c(
    headvars, setdiff(names, union(headvars, tailvars)),
    tailvars
  )
  if (is.data.table(d)) {
    d[, varnames, with = F]
  } else if (is.data.frame(d)) {
    d[, varnames]
  } else if (is.list(d)) {
    d[varnames]
  } else {
    stop("Unknown data type!")
  }
}


# ' flipud and fliplr
# ' @export
flipud <- function(x, ...) {
  I <- ncol(x):1
  ndim <- length(dim(x))
  if (ndim == 2) {
    x[, I]
  } else if (ndim == 3) {
    x[, I, , drop = FALSE]
  }
}

# ' @rdname flipud
# ' @export
fliplr <- function(x) {
  I <- nrow(x):1
  ndim <- length(dim(x))
  if (ndim == 2) {
    x[I, ]
  } else if (ndim == 3) {
    x[I, , , drop = FALSE]
  }
}
