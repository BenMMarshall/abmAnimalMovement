library(abmAnimalMovement)
# library(Rcpp)

# Basic Rcpp Compiling and Testing ----------------------------------------
?basic_walk()

basic_walk(start = c(20,25),
           steps = 200,
           options = 12,
           normmean = 5,
           normsd = 5,
           meanang = 0,
           sdang = 180)

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
