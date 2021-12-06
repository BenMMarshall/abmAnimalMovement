#' Quick generate test landscapes
#'
#' @param row,col number of rows and columns of the final matrix. Beware the
#'   some functions will default with 100 then disaggregate to reach row and col
#'   requirements. This is to save memory.
#' @param mat A matrix to plot using ggplot2
#' @name genLandscape
#' @return a matrix usable as a environment layer / landscape
#'
#' @useDynLib abmAnimalMovement
NULL
#> NULL

#' @rdname genLandscape
#' @export
genLandscape_noise <- function(row, col){
  outMat <- matrix(runif(row*col, 0, 1), nrow = row, ncol = col)
  return(outMat)
}

#' @rdname genLandscape
#' @export
genLandscape_gradient <- function(row, col){
  gradRast <- NLMR::nlm_distancegradient(ncol = 100,
                                         nrow = 100,
                                         origin = c(10, 10, 10, 10))

  gradRast <- raster::disaggregate(gradRast, fact = row/100)

  outMat <- matrix(data = raster::getValues(gradRast),
                   nrow = row,
                   ncol = col)
  return(outMat)
}

#' @rdname genLandscape
#' @export
quick_plot_matrix <- function(mat){
  longEnvMat <- reshape2::melt(mat, c("col", "row"))
  return(ggplot2::ggplot() +
           ggplot2::geom_raster(data = longEnvMat,
                                ggplot2::aes(x = col, y = row, fill = value)))
}
