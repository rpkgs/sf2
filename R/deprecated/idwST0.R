#' @name idwST0
#' @title Inverse Distance Weighting (IDW)
#' 
#' @examples
#' \dontrun{
#' r <- idwST0(location, newdata, mat_z, maxdist, nmax, factor.p)
#' }
#' @keywords internal
#' 
#' @importFrom matrixStats colOrderStats colSums2
idwST0 <- function(locations, newdata, mat_z, maxdist = Inf, nmax = NULL, 
    factor.p)
{
    # TODO: fix matmult_eigenMap in Ipaper
    if (is.null(nmax)) nmax <- nrow(locations)
    npixel <- nrow(coordinates(newdata))

    # s  = cbind(coordinates(locations))
    # s0 = cbind(coordinates(newdata)) %>% set_colnames(c("x", "y"))
    # S <- scale(s)
    # S_std <- cbind(standardize(s0[, 1], mean(s[, 1]), sd(s[, 1])), 
    #                standardize(s0[, 2], mean(s[, 2]), sd(s[, 2])))
    # mat_ws0 <- rdist(S,  S_std)
    # mat_ws0 <- rdist(s, s0)
    # dists <-  rdist.earth(s, s0)
    
    dists    <- spDists(locations, newdata)
    DIST_MIN <- 0.01
    dists[dists <= DIST_MIN] <- DIST_MIN
    
    np <- nrow(dists)

    # filter stations
    DIST_MAX <- colOrderStats(dists, which = nmax) 
    DIST_MAX <- pmin(maxdist, DIST_MAX) %>% t() %>% repmat(np, 1)    # `nmax` constrain
    # `maxdist` constrain
    dists[dists > DIST_MAX] <- NA
    
    mat_ws0 <- dists^(-factor.p) # mat_ws0
    mat_ws0[is.na(mat_ws0)] <- 0

    ## matrix solution ---------------------------------------------------------
    ws.colsum <- colSums2(mat_ws0, na.rm = TRUE) %>% t() 
    ws.colsum <- repmat(ws.colsum, np, 1)
    mat_ws <- mat_ws0/ws.colsum
    
    mat_ypred <- nanMatMult(mat_z, mat_ws, matmult_eigenMap) %>% t()

    colnames(mat_ypred) <- rownames(mat_z)
    df_ypred <- as.data.frame(mat_ypred)
    # browser()
    # rbenchmark::benchmark(
    #     y0 <- nanMatMult(mat_z, mat_ws, `%*%`),
    #     y1 <- nanMatMult(mat_z, mat_ws, matmult_eigenMap),
    #     replications = 1)
    # system.time(
    #     mat_ypred <- mat_z %*% mat_ws 
    # )
    # if (nrow(ypred) == 1) ypred %<>% as.numeric()
    # normalize weight by row, make its sum = 1
    
    ## second solution ---------------------------------------------------------
    # idw0 <- function(z, w0, nmax, factor.p) {
    #     I_order <- order(w0, decreasing = TRUE)
    #     vc <- I_order[1:nmax]
    #     w  <- w0[vc]
    #     Lambda <- w/sum(w)
    #     # Lambda <- dist.vec.cerca^(-factor.p)/sum(dist.vec.cerca^(-factor.p))
    #     pred <- t(Lambda) %*% z[vc]
    #     pred
    # }

    # Pred  <- rep(NA, length = npixel)
    # for (i in 1:npixel) {
    #     Pred[i] <- idw0(z, w0 = mat_ws0[, i], nmax, factor.p = factor.p)
    # }

    # df <- data.frame(s0, var1.pred = Pred)
    # df$pred2 <- ypred
    # df
    newdata@data <- df_ypred
    newdata
}

standardize <- function (x, m, sd) {
    (x - m)/sd
}

repmat <- function(x, n, m) {
    if (!is.matrix(x)) x <- as.matrix(x)
    matrix(1, n, m) %x% x
}
