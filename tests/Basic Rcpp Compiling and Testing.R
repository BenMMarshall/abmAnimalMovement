library(abmAnimalMovement)
# library(Rcpp)
library(ggplot2)
library(reshape2)
library(scico)

# Basic Rcpp Compiling and Testing ----------------------------------------

sample_test(1:5)

# ?basic_walk()
row <- 1000; col <- 1000
envMatTest <- matrix(runif(row*col, 0, 1), nrow = row, ncol = col)
colnames(envMatTest) <- 1:col
rownames(envMatTest) <- 1:row

longMatData <- melt(envMatTest, c("row", "col"))
head(longMatData)

plotBgEnv <- ggplot() +
  geom_raster(data = longMatData,
            aes(x = col, y = row, fill = value))

basicRes <- basic_walk(start = c(500,500),
                       steps = 100,
                       options = 5,
                       normmean = 5,
                       normsd = 2,
                       meanang = 0,
                       sdang = 180,
                       envMat1 = envMatTest)


plotBgEnv +
  geom_point(data = as.data.frame(basicRes$OptionsAll),
            aes(x = V1, y = V2, colour = V3)) +
  geom_path(data = as.data.frame(basicRes$Locations),
            aes(x = V1, y = V2)) +
  geom_point(data = as.data.frame(basicRes$Locations),
            aes(x = V1, y = V2)) +
  scale_colour_scico(palette = "buda") +
  coord_cartesian(xlim = range(basicRes$Locations[,1]), ylim = range(basicRes$Locations[,2]))


# Plotting output ---------------------------------------------------------

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
