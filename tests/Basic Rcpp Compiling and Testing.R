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

# so this works, remember that the index should be one lower cos Cpp starts at zero
find_max(vect = c(4,6,5))
find_max(vect = c(4,6,5,10))
find_max(vect = 0:5)
find_max(vect = c(4,6,5,4,5,4,32,2,4,4,6,1,7,73,3,2,23,54))

# Sampling sub-function testing -------------------------------------------

# https://www.r-bloggers.com/2018/09/using-rs-set-seed-to-set-seeds-for-use-in-c-c-including-rcpp/
get_seed <- function() {
  sample.int(.Machine$integer.max, 1)
}

get_seed()

sample_options(c(0.4, 0.1, 0.7, 0.1, 0.1, 0.2), get_seed())

sampleOut <- NULL
for(i in 1:10000){
  sampleOut[i] <- sample_options(c(0.25, 0.15, 0.5, 0.05, 0.05), get_seed())
}
hist(sampleOut)
table(sampleOut) / 10000

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
                       k_step = 4,
                       s_step = 1,
                       mu_angle = 0,
                       k_angle = 0.05,
                       envMat1 = envMatTest)

basicRes

plotBgEnv +
  geom_point(data = data.frame(x = basicRes$oall_x,
                               y = basicRes$oall_y,
                               step = basicRes$oall_step),
            aes(x = x, y = y, colour = step)) +
  geom_path(data = data.frame(x = basicRes$loc_x,
                               y = basicRes$loc_y),
             aes(x = x, y = y)) +
  geom_point(data = data.frame(x = basicRes$loc_x,
                               y = basicRes$loc_y),
             aes(x = x, y = y)) +
  scale_colour_scico(palette = "buda") +
  coord_cartesian(xlim = range(basicRes$loc_x), ylim = range(basicRes$loc_y)) +
  theme_bw() +
  theme(aspect.ratio = 1)

# Vonmises testing --------------------------------------------------------

library(abmAnimalMovement)
library(ggplot2)

set.seed(2021)

vonOut <- vonmises(N = 1000, MU = 0, KAPPA = 0.1)
hist(vonOut)

vonResVarying <- do.call(rbind, lapply(
  c(0.01, seq(0.05, 0.8, 0.05)),
  function(k){
    print(k)
    return(data.frame(kappa = paste("KAPPA =",
                                    format(k, nsmall = 2)), # format forces the zeroes to remain
                      draws = vonmises(N = 2000, MU = 0, KAPPA = k)))
  }))

sum(vonResVarying$draws > pi)
sum(vonResVarying$draws < -pi)

# mu should vary between 0 and 2*pi for circular stuff
# https://www.zeileis.org/news/circtree/

# soltuion now adapted to C++
# CircStats::rvm()

# \u03c0 is unicode for pi
## display the varying levels of angle concentration.
ggplot() +
  geom_histogram(data = vonResVarying, aes(x = draws, fill = as.factor(kappa)),
                 binwidth = 0.25) +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = c("-\u03c0", "-\u03c0/2", "0", "\u03c0/2", "\u03c0"),
                     limits = c(-pi, pi), # implementing pi limits cuts of a few points, might be an approximation issue.
                     # might be a source of issues in the random walk, might need a redraw if those limits are exceeded. Not sure
                     # if it'd break something in the walk. Could implement check for safety.
                     # Seems to be an approximation issue with ggplot, as checking for values >pi | <-pi return zero results
                     expand = c(0,0)
                     ) +
  coord_polar() +
  theme_void() +
  theme(axis.text.x = element_text(),
        strip.text = element_text(face = 4, hjust = 0),
        panel.grid.major.x = element_line(colour = "grey15")) +
  facet_wrap(.~kappa)


# Testing matrix conversion and back --------------------------------------

row <- 1000; col <- 1000
# random matrix
envMatTest <- matrix(runif(row*col, 0, 1), nrow = row, ncol = col)
# split matrix, animal should remain more often in one side
# chaning something so we can spot if mistakes are made
envMatTest[1:250, 1:250] <- 1


colnames(envMatTest) <- 1:col
rownames(envMatTest) <- 1:row

matrix_combine(envMatTest)


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
