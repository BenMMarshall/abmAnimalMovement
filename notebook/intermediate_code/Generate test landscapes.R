
library(abmAnimalMovement)

# Noise only --------------------------------------------------------------

envNoiseTest <- genLandscape_noise(1000, 1000)

quick_plot_matrix(envNoiseTest)

# Basic gradient ----------------------------------------------------------

envGradMat <- genLandscape_gradient(1000, 1000)

quick_plot_matrix(envGradMat)


# Draft code for generating suite of layers -------------------------------

library(NLMR)
library(raster)
library(landscapetools)

## Movement layers
# - resistance

## Resource layer
# - foraging resources
# - sheltering locations
# - memory

quickLandList <- genLandscape_quickTriple(1000, 1000, 1)

plot(quickLandList$forage)
plot(quickLandList[[2]])
plot(quickLandList[[3]])

# Core landscape to draw from ---------------------------------------------
gf1 <- NLMR::nlm_gaussianfield(ncol = 1000,
                                nrow = 1000,
                                resolution = 1,
                                autocorr_range = 40,
                                mag_var = 5,
                                nug = 0.2,
                                mean = 0.5,
                                user_seed = 1,
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

# Generate random re-used shelter sites -------------------------------------------
## This is a process for the animal remembering key shelter sites to re-use
set.seed(2021)

randomLocs <- data.frame("x" = sample(400:800, 200, replace = FALSE),
                         "y" = sample(400:800, 200, replace = FALSE))
# see the "shelter quality" at each location
randomLocs$shelterVals <- extract(shelterQual, SpatialPoints(randomLocs))
# only look at those in good places
randomLocs <- randomLocs[randomLocs$shelterVals > 0.5,]
# randomly select 5
chosenShelters <- randomLocs[randomLocs$shelterVals %in%
                               sample(randomLocs$shelterVals, 5, prob = randomLocs$shelterVals),]
# plot them to see where they fall
points(chosenShelters$x, chosenShelters$y)

# build a distance raster for them
distanceRast <- distanceFromPoints(shelterQual, xy = chosenShelters[,c("x","y")])
plot(distanceRast)

# invert the raster so the higher values work with highly liklihood of usage
distanceRast[] <- abs(distanceRast[] - max(distanceRast[]))

# this adds a buffer around the points for sites that are larger than a single
# cell, e.g., a foraging area, or badger set
distanceRast[distanceRast[] < max(distanceRast[])-20] <- NA
# set min 0 max 1, normalise the values between 1 and 0
distanceRast[] <- (distanceRast[] - min(distanceRast[], na.rm = TRUE)) /
  (max(distanceRast[], na.rm = TRUE) - min(distanceRast[], na.rm = TRUE))

distanceRast[is.na(distanceRast[])] <- 0

plot(forageQual)
plot(shelterQual)
plot(distanceRast)
