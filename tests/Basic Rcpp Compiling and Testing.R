# both required for header files
# install.packages("Rcpp")
# install.packages("BH")
# library(Rcpp)
# library(BH)

library(abmAnimalMovement)
library(ggplot2)
library(reshape2)
library(scico)

# Basic Rcpp Compiling and Testing ----------------------------------------

set.seed(2021)

# Sampling sub-function testing -------------------------------------------

sampleOut <- NULL
for(i in 1:1000){
  sampleOut[i] <- sample_test(1:5, c(0.4, 0.1, 0.1, 0.1, 0.2))
}
hist(sampleOut)

# Matrix BG creation ------------------------------------------------------


# ?basic_walk()
row <- 1000; col <- 1000
# random matrix
envMatTest <- matrix(runif(row*col, 0, 1), nrow = row, ncol = col)
# split matrix, animal should remain more often in one side
# envMatTest <- matrix(c(rep(1, row*col/2),
#                        rep(5, row*col/2)),
#                      nrow = row, ncol = col)

colnames(envMatTest) <- 1:col
rownames(envMatTest) <- 1:row

# make it work nicely with ggplot2
longMatData <- melt(envMatTest, c("col", "row"))
head(longMatData)

(plotBgEnv <- ggplot() +
    geom_raster(data = longMatData,
                aes(x = col, y = row, fill = value)))

# Random walk testing -----------------------------------------------------

basicRes <- basic_walk(start = c(500,500),
                       steps = 100,
                       options = 10,
                       normmean = 2,
                       normsd = 1,
                       meanang = 0,
                       sdang = 180,
                       envMat1 = envMatTest)

basicRes

plotBgEnv +
  geom_point(data = as.data.frame(basicRes$OptionsAll),
            aes(x = V1, y = V2, colour = V3)) +
  geom_path(data = as.data.frame(basicRes$Locations),
            aes(x = V1, y = V2)) +
  geom_point(data = as.data.frame(basicRes$Locations),
            aes(x = V1, y = V2)) +
  scale_colour_scico(palette = "buda") +
  coord_cartesian(xlim = range(basicRes$Locations[,1]), ylim = range(basicRes$Locations[,2])) +
  theme_bw() +
  theme(aspect.ratio = 1)

# Vonmises testing --------------------------------------------------------
library(abmAnimalMovement)
library(ggplot2)

# mu should vary between 0 and 2*pi for circular stuff
# https://www.zeileis.org/news/circtree/
# vmRes <- vonmises(1000, 10, 360, 2)

# might be worth adapting to C++
CircStats::rvm()

ggplot() +
  geom_density(data = as.data.frame(vmRes), aes(vmRes))


# Rfast::rvonmises()
# not sure if that one helps

# Using Rcpp to call and compile Cpp function directly --------------------

####
### will only work if // [[Rcpp::export]] present ###
####
Rcpp::sourceCpp("./src/walk_options_xy.cpp")

walkRes <- walk_options_xy(startx = 20,
                           starty = 25,
                           steps = 200,
                           options = 12,
                           normmean = 5,
                           normsd = 5,
                           meanang = 0,
                           sdang = 180)

plot(walkRes[,1], walkRes[,2])
lines(walkRes[,1], walkRes[,2])

# possible benchmarking method -----------------------------------------------------

# install.packages("microbenchmark")

microbenchmark::microbenchmark(
  mat[100,230],
  getValueFromRaster(mat, 99, 229)
)
