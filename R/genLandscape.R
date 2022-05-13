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

#' @rdname genLandscape
#' @export
genLandscape_quickTriple <- function(row, col, seed){
  gf1 <- NLMR::nlm_gaussianfield(ncol = col,
                                 nrow = row,
                                 resolution = 1,
                                 autocorr_range = 40,
                                 mag_var = 5,
                                 nug = 0.2,
                                 mean = 0.5,
                                 user_seed = seed,
                                 rescale = TRUE)

  forageQual <- gf1
  forageQual[forageQual[] < 0.6 & forageQual[] > 0.3] <-
    forageQual[forageQual[] < 0.6 & forageQual[] > 0.3] + 1
  forageQual[forageQual[] < 1] <- 0
  # set min 0 max 1, normalise the values between 1 and 0
  forageQual[] <- (forageQual[] - min(forageQual[], na.rm = TRUE)) /
    (max(forageQual[], na.rm = TRUE) - min(forageQual[], na.rm = TRUE))

  shelterQual <- gf1
  shelterQual[shelterQual[] < 0.6] <- shelterQual[shelterQual[] < 0.6] - 0.5
  shelterQual[shelterQual[] < 0] <- 0

  # Generate random re-used shelter sites -------------------------------------------
  ## This is a process for the animal remembering key shelter sites to re-use
  randomLocs <- data.frame("x" = sample(400:800, 200, replace = FALSE),
                           "y" = sample(400:800, 200, replace = FALSE))
  # see the "shelter quality" at each location
  randomLocs$shelterVals <- raster::extract(shelterQual, sp::SpatialPoints(randomLocs))
  # only look at those in good places
  randomLocs <- randomLocs[randomLocs$shelterVals > 0.5,]
  # randomly select 5
  chosenShelters <- randomLocs[randomLocs$shelterVals %in%
                                 sample(randomLocs$shelterVals, 5, prob = randomLocs$shelterVals),]

  # build a distance raster for them
  distanceRast <- raster::distanceFromPoints(shelterQual, xy = chosenShelters[,c("x","y")])

  # invert the raster so the higher values work with highly liklihood of usage
  distanceRast[] <- abs(distanceRast[] - max(distanceRast[]))

  # this adds a buffer around the points for sites that are larger than a single
  # cell, e.g., a foraging area, or badger set
  distanceRast[distanceRast[] < max(distanceRast[])-100] <- NA
  # set min 0 max 1, normalise the values between 1 and 0
  distanceRast[] <- (distanceRast[] - min(distanceRast[], na.rm = TRUE)) /
    (max(distanceRast[], na.rm = TRUE) - min(distanceRast[], na.rm = TRUE))

  distanceRast[is.na(distanceRast[])] <- 0

  return(list(
    "shelter" = matrix(data = raster::getValues(distanceRast),
                          nrow = row,
                          ncol = col),
    "forage" = matrix(data = raster::getValues(forageQual),
                      nrow = row,
                      ncol = col),
    "movement" = matrix(data = raster::getValues(shelterQual),
                       nrow = row,
                       ncol = col)
  ))
}
