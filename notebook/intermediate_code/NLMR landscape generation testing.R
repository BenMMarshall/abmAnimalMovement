
library(abmAnimalMovement)
library(raster)
library(scico)

genLandscape_gradient(1000, 1000)
genLandscape_noise(1000, 1000)

quick_plot_matrix(genLandscape_quickTriple(1000, 1000, 1)[1])

### PALETTE ###
palette <- c("#AD6DED", "#7D26D4", "#E87D13", "#965A1D", "#302010")
names(palette) <- c("purp1", "purp2", "2", "1", "0")
### SEED ###
set.seed(2022)

col <- 2000; row <- 2000; seed <- 2022;

gf1 <- NLMR::nlm_gaussianfield(ncol = col,
                               nrow = row,
                               resolution = 1,
                               autocorr_range = 40,
                               mag_var = 5,
                               nug = 0.2,
                               mean = 0.5,
                               user_seed = seed,
                               rescale = TRUE)
plot(gf1)

forageQual <- gf1
forageQual[forageQual[] < 0.6 & forageQual[] > 0.3] <-
  forageQual[forageQual[] < 0.6 & forageQual[] > 0.3] + 1
forageQual[forageQual[] < 1] <- 0
# set min 0 max 1, normalise the values between 1 and 0
forageQual[] <- (forageQual[] - min(forageQual[], na.rm = TRUE)) /
  (max(forageQual[], na.rm = TRUE) - min(forageQual[], na.rm = TRUE))

plot(forageQual)

shelterQual <- gf1
shelterQual[shelterQual[] < 0.6] <- shelterQual[shelterQual[] < 0.6] - 0.5
shelterQual[shelterQual[] < 0] <- 0

plot(shelterQual)
## This is a process for the animal remembering key shelter sites to re-use
randomLocs <- data.frame("x" = sample((col*0.35):(col*0.65), col/4, replace = FALSE),
                         "y" = sample((row*0.35):(row*0.65), row/4, replace = FALSE))
# see the "shelter quality" at each location
randomLocs$shelterVals <- raster::extract(shelterQual, sp::SpatialPoints(randomLocs))
# only look at those in good places
randomLocs <- randomLocs[randomLocs$shelterVals > 0.5,]
# randomly select 5
chosenShelters <- randomLocs[randomLocs$shelterVals %in%
                               sample(randomLocs$shelterVals, 5, prob = randomLocs$shelterVals),]

plot(shelterQual)
points(chosenShelters$x, chosenShelters$y)

moveQual <- NLMR::nlm_fbm(ncol = col,
              nrow = row,
              resolution = 1,
              fract_dim = 0.75,
              user_seed = seed,
              rescale = TRUE)
plot(moveQual)

# install.packages("devtools")
# devtools::install_github("marcosci/layer")
library(layer)
library(ggplot2)

# lower the res for plotting
forageQual_aggregate <- aggregate(forageQual, fact = 10)
moveQual_aggregate <- aggregate(moveQual, fact = 10)
shelterQual_aggregate <- aggregate(shelterQual, fact = 10)

tilt2 <- tilt_map(forageQual_aggregate, parallel = TRUE)
tilt1 <- tilt_map(moveQual_aggregate, x_shift = 0, y_shift = 2500, parallel = TRUE)
tilt0 <- tilt_map(shelterQual_aggregate, x_shift = 0, y_shift = 5000, parallel = TRUE)
tilt0p <- tilt_map(as(SpatialPoints(chosenShelters), "sf"),
                   x_shift = 0, y_shift = 5001, parallel = TRUE)

map_list <- list(tilt2, tilt1, tilt0, tilt0p)

plot_tiltedmaps(map_list,
                layer = c("value", "value", "value", NA),
                palette = c("lajolla", "lajolla", "lajolla", NA),
                color = palette["0"],
                alpha = 1)

ggsave("./output/figures/environmentalLayers.png",
       width = 120, height = 180, units = "mm", dpi = 300)

# OLD ---------------------------------------------------------------------

# install.packages("NLMR")
# install.packages("landscapetools")

# library(NLMR)
# library(raster)

## examples pulled from https://ropensci.github.io/NLMR/articles/getstarted.html

# MERGING two layers
sL1 <- NLMR::nlm_distancegradient(ncol = 100,
                                  nrow = 100,
                                  origin = c(10, 10, 10, 10))
sL2 <- NLMR::nlm_random(ncol = 100,
                        nrow = 100)

mL1 <- sL1 + sL2/10

mL1 <- raster::disaggregate(mL1, fact = 10)

raster::plot(mL1)

envGradMat <- matrix(data = raster::getValues(mL1),
                     nrow = 1000,
                     ncol = 1000)

longEnvMat <- reshape2::melt(envGradMat, c("col", "row"))

library(ggplot2)

ggplot() +
  geom_raster(data = longEnvMat, aes(x = col, y = row, fill = value))

# classified landscape example

nr <- NLMR::nlm_fbm(50, 100, fract_dim = 1.2)

nr_classified <- landscapetools::util_classify(nr, weighting = c(0.3, 0.3, 0.3))

raster::plot(nr_classified)

# Generating suitable testing landscapes ----------------------------------

## GRADIENT with noise ##

envGrad <- NLMR::nlm_distancegradient(ncol = 1000,
                                      nrow = 1000,
                                      origin = c(1, 1000, 1, 1)) # origin deals with where the gradient moves from

envNoise <- NLMR::nlm_random(ncol = 1000,
                             nrow = 1000)

# scaling factor will weight the impact of the second layer
envGradTest <- landscapetools::util_merge(envGrad, envNoise, scalingfactor = 0.5)

raster::plot(envGradTest)

## adequate way of converting the raster to a matrix for the walk function?
envGradMat <- matrix(data = raster::getValues(envGradTest),
       nrow = 1000,
       ncol = 1000)

longEnvMat <- reshape2::melt(envGradMat, c("col", "row"))

library(ggplot2)

ggplot() +
  geom_raster(data = longEnvMat, aes(x = col, y = row, fill = value))

## CLASSIFIED ##

clusterHabs <- NLMR::nlm_gaussianfield(ncol = 100,
                                       nrow = 100,
                                       autocorr_range = 20,
                                       mag_var = 5,
                                       nug = 0.2,
                                       mean = 0.5,
                                       user_seed = 2021)

raster::plot(clusterHabs)

### DONT RUN REQUIRES TOO MUCH RAM?????
classHabs <- landscapetools::util_classify(clusterHabs, n = 3)

raster::plot(classHabs)

classHabs1000 <- raster::disaggregate(classHabs, fact = 10)

raster::plot(classHabs1000)

classHabsMat <- matrix(data = raster::getValues(classHabs1000),
                        nrow = 1000,
                        ncol = 1000)

longClassHabsMat <- reshape2::melt(classHabsMat, c("col", "row"))

ggplot() +
  geom_raster(data = longClassHabsMat, aes(x = col, y = row, fill = value))

## We do need a solution for people who may only be able to supply low res
## landscapes, we either need to rescale them (but that may be too memory
## intensive for large lanscapes) or fix the rounding in the step lengths and
## how they connect to cell calls. See below (latter options is likely more
## work, but more flexible, preferable).

# To address a mismatch between raster res and step length we'd need to divide
# the step lengths by the size of raster cell (e.g., 100 to convert a 100 by 100
# cell to 1 m by 1 m: ie the same res as the steps) and then floor round to geet
# the matrix/cell value. This should work, but will need an inout for the res of
# cells in environmental layers. Would be easier to enforce the layers to all be
# the same res.
