
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

mL1 <- sL1 + sL2

raster::plot(mL1)

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
