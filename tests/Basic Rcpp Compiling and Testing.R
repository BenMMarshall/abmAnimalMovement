library(abmAnimalMovement)
# library(Rcpp)

# Basic Rcpp Compiling and Testing ----------------------------------------

sample_test(1:5)

# ?basic_walk()
# row <- 1000; col <- 1000
# envMatTest <- matrix(runif(row*col, 0, 1), nrow = row, ncol = col)

basic_walk(start = c(50,50),
           steps = 10,
           options = 5,
           normmean = 5,
           normsd = 2,
           meanang = 0,
           sdang = 180,
           envMat1 = envMatTest)

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
