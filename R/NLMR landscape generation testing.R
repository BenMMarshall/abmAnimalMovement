
# install.packages("NLMR")

library(NLMR)
library(raster)

## examples pulled from https://ropensci.github.io/NLMR/articles/getstarted.html

# MERGING two layers
sL1 <- NLMR::nlm_distancegradient(ncol = 100,
                                  nrow = 100,
                                  origin = c(10, 10, 10, 10))
sL2 <- NLMR::nlm_random(ncol = 100,
                        nrow = 100)

mL1 <- sL1 + sL2

plot(mL1)

# classified landscape example

nr <- NLMR::nlm_fbm(50, 100, fract_dim = 1.2)

nr_classified <- landscapetools::util_classify(nr, weighting = c(0.3, 0.3, 0.3))

plot(nr_classified)
