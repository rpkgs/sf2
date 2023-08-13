#' @name idwST
#' @title Inverse Distance Weighting (IDW) function for spatio-temporal prediction. 
#' 
#' @description
#' This function performs spatio-temporal interpolation. Here \emph{idwST} is in a
#' local neighborhood. This interpolation method considers the value of a point can
#' be obtained from the weighted sum of values of the regionalized variable of
#' closest neighbors. The general formula for the IDW is given by: \deqn{
#' \hat{z}_0(st)=\sum_{i=1}^n \lambda_i z_i(st) } The expression for determining
#' the weights is: \deqn{ \lambda_i = \frac{d_{i0}^{-p}}{\sum_{i=1}^n d_{i0}^{-p}}
#' } The weight is controlled by a factor \emph{p} with each increment of the
#' distance, \eqn{d_{i0}} is the distance between the prediction position and each
#' of the measured positions.
#' 
#' The expression \eqn{d_{i0}} can be obtained by: 
#' \deqn{d_{i0}=\sqrt{(x_{i}-x_{0})^2+(y_{i}-y_{0})^2+C\cdot (t_{i}-t_{0})^2} } \eqn{x},
#' \eqn{y} and \eqn{t} correspond to the spatio-temporal coordinates, \emph{p}
#' (factor.p).
#' 
#' @details
#' idwST function generates individual spatio-temporal predictions from
#' IDW spatio-temporal interpolation. IDW is a type of deterministic method for
#' interpolation, the assigned values to unknown points are calculated with a
#' weighted average of the values available at the known points.
#' 
#' @param formula formula that defines a detrended linear model, use \eqn{z_{st}}\code{~1}.
#' @param location SpatialPointsDataFrame
#' @param data data.frame, `[ntime, 1+nsite]`, 1th column is `date` object. 
#' should contain the spatio-temporal dependent
#' variable, independent variables (statics and/or dynamics), spatial coordinates
#' and the time as an integer or numerical variable.
#' @param newdata location frame or spatial object with prediction/simulation
#' spatio-temporal locations; should contain attribute columns with the
#' independent variables (if present) and (if locations is a formula) the
#' coordinates and time with names, as defined in locations where you want to
#' generate new predictions
#' @param maxdist max distance (km)
#' @param nmax number of nearest observations that should be used for a \emph{idwST}
#' prediction, where nearest is defined in terms of the spatio-temporal locations
#' @param factor.p numeric; specify the inverse distance weighting power (\emph{p}
#' is the exponent that influences the weighting or optimal smoothing parameter)
#' @return Attributes columns contain coordinates, time, predictions, and the variance
#' column contains NA's.
#' 
#' @references
#' 1. Li L, Losser T, Yorke C, Piltner R. (2014). \emph{Fast inverse distance 
#' weighting-based spatiotemporal interpolation: a web-based application of 
#' interpolating daily fine particulate matter PM2:5 in the contiguous U.S. 
#' using parallel programming and k-d tree.} Int. J. Environ. Res. Public Health, 
#' 11: 9101-9141. [link](http://www.mdpi.com/journal/ijerph) 
#' 
#' @examples
#' \dontrun{
#' print('hello')
#' r <- idwST(sp_all, data, gridclip)
#' }
#' @importFrom magrittr `%>%`
NULL

#' @rdname idwST
#' @export
idwST.spatial <- function (location, newdata, data, 
    maxdist = Inf, nmax = NULL, factor.p = 2) 
{        
    dates <- data$date %>% format("t%Y%m%d")
    mat_z <- data[, -1] %>% as.matrix() %>% set_rownames(dates)

    r <- idwST0(dists, mat_z, maxdist, nmax, factor.p)
    r
}

#' @rdname idwST
#' @export
idwST.formula <- function(formula, location, newdata, 
    maxdist = Inf, nmax = NULL, factor.p = 2) {
    mat_z  = gstat:::extractFormula(formula, location, newdata)$y %>% as.matrix()
    r <- idwST0(location, newdata, mat_z, maxdist, nmax, factor.p)
    r
}


# idw0 <- function (formula, data, newdata, y, idp = 2) 
# {
#     s = coordinates(data)
#     s0 = coordinates(newdata)
#     if (missing(y)) 
#         y = extractFormula(formula, data, newdata)$y
#     D = 1/(sp::spDists(s0, s)^idp)
#     sumD = apply(D, 1, sum)
#     D %*% y/sumD
# }


#' Matrix product with NA values
#' 
#' `matmult_eigenMap` is about 11 times faster than `crossprod`.
#' 
#' @param x matrix
#' @param w matrix, weight.
#' 
#' @examples 
#' \dontrun{
#' A <- matrix(rnorm(10000), 1000, 1000)
#' B <- matrix(rnorm(10000), 1000, 1000)
#' 
#' rbenchmark::benchmark(
#'   r1 <- matmult(A, B, crossprod),
#'   # r2 <- matmult(A, B, Rfast::mat.mult),
#'   r3 <- matmult(A, B, matmult_arma),
#'   r4 <- matmult(A, B, matmult_eigen),
#'   r5 <- matmult(A, B, matmult_eigenMap),
#'   replications = 10)
#' }
#' @export
nanMatMult <- function(x, w, FUN = `crossprod`){
    x_isna <- is.na(x)
    x[x_isna] <- 0
    
    y0 <- FUN(x, w)
    w_fix <- FUN((!x_isna)*1, w)
    y0/w_fix
}
