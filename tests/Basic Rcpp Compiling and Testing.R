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

set.seed(2022)

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
  sampleOut[i] <- sample_options(c(2, -0.2, 5, 0.5, 0.05), get_seed())
}
hist(sampleOut)
table(sampleOut) / 10000


# Dist to des normalise ---------------------------------------------------

# distsTests <- c(50,20,324,234,683,29,101)
distsTests <- rgamma(12, 5, 0.5)

distInvert = abs(distsTests - max(distsTests))
weights_toDes = (distInvert - min(distsTests)) /
  (max(distsTests) - min(distsTests))

weights_toDes^2

# Matrix BG creation ------------------------------------------------------

envNoiseTest <- genLandscape_noise(2000, 2000)
# quick_plot_matrix(envNoiseTest)
#
# envGradMat <- genLandscape_gradient(1000, 1000)
# quick_plot_matrix(envGradMat)

landcapeLayersList <- genLandscape_quickTriple(2000, 2000, seed = 1)

# weaken the impact of the movement layer
# landcapeLayersList$shelter[] <- landcapeLayersList$shelter[]/2
# remove the impact for testing
# landcapeLayersList$shelter[] <- 0

# plotBgEnv <- quick_plot_matrix(landcapeLayersList$memShelter)
plotBgEnv <- quick_plot_matrix(landcapeLayersList$shelter)
# plotBgEnv <- quick_plot_matrix(landcapeLayersList$forage)

plotBgEnv

# Select envMat to use ----------------------------------------------------

# envMatTest <- envNoiseTest
# plotBgEnv <- quick_plot_matrix(envMatTest)

# Generate transitional matrix  --------------------------------------------

b0 <- c(0.95, 0.008, 0.008) # rest
b1 <- c(0.0005, 0.98, 0.005) # explore/move
b2 <- c(0.0005, 0.005, 0.95) # forage

behaveMatTest <- rbind(b0, b1, b2)

behaveMatTest[1,]

# Random walk testing -----------------------------------------------------

simRes <- abm_simulate(start = c(1000,1000),
                       steps = 24*60 *7,
                       des_options = 20,
                       options = 12,
                       k_step = c(5, 4, 2),
                       s_step = c(0.5, 2, 1),
                       mu_angle = c(0, 0, 0),
                       k_angle = c(0.1, 0.2, 0.05),
                       behave_Tmat = behaveMatTest,
                       rest_Cycle = c(0.5, 0.05, 24, 12),
                       memShelterMatrix = landcapeLayersList$memShelter,
                       forageMatrix = landcapeLayersList$forage,
                       move_Options = landcapeLayersList$shelter) # just using a place holder layer for testing

simRes$options

plotBgEnv +
  geom_point(data = simRes$options,
            aes(x = x, y = y), alpha = 0.05, colour = "orange") +
  geom_path(data = simRes$locations,
             aes(x = x, y = y), alpha = 0.15) +
  geom_point(data = simRes$locations,
             aes(x = x, y = y, shape = as.factor(behave)),
             alpha = 0.45) +
  # geom_segment(data = data.frame(xend = 1000,
  #                                yend = 1000,
  #                                x = simRes$loc_x,
  #                                y = simRes$loc_y),
  #   aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.025) +
  geom_point(data = data.frame(x = 1020,
                               y = 1020),
             aes(x = x, y = y),
             size = 2, colour = "red",
             alpha = 0.45) +
  scale_colour_scico(palette = "buda") +
  coord_cartesian(xlim = range(simRes$locations$x), ylim = range(simRes$locations$y)) +
  theme_bw() +
  theme(aspect.ratio = 1)


# Animate movement --------------------------------------------------------

# install.packages("gifski")
# install.packages("av")
library(gganimate)
library(dplyr)

?`gganimate-package`

locations <- data.frame(
  step = simRes$loc_step,
  x = simRes$loc_x,
  y = simRes$loc_y,
  behave = simRes$loc_behave
)

p <- locations %>%
  filter(step < 200) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2, colour = "black") +
  transition_states(step, 1, 1) +
  shadow_mark(size = 1, colour = 'grey')

p <-
  # plotBgEnv +
  ggplot() +
  # geom_point(data = data.frame(x = simRes$oall_x,
  #                              y = simRes$oall_y,
  #                              step = simRes$oall_step),
  #            aes(x = x, y = y), alpha = 0.05, colour = "orange") +
  geom_path(data = locations,
            aes(x = x, y = y), alpha = 0.05) +
  geom_point(data = locations,
             aes(x = x, y = y),
             alpha = 0.45, size = 3) +
  geom_point(data = data.frame(x = 1020,
                               y = 1020),
             aes(x = x, y = y),
             size = 2, colour = "red",
             alpha = 0.45) +
  scale_colour_scico(palette = "buda") +
  coord_cartesian(xlim = range(simRes$loc_x), ylim = range(simRes$loc_y)) +
  labs(title = "Behavioural state: {locations$behave[locations$step == round(frame_along)][1]}") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  # transition_states(step, 1, 1) +
  transition_reveal(step) +
  # shadow_mark(size = 1, colour = "black") +
  # shadow_wake(wake_length = 1, alpha = FALSE) +
  # ease_aes("quadratic-in") +
  NULL

animate(p,
        width = 1080,
        height = 1080,
        res = 240,
        fps = 60,
        duration = dim(locations)[1] / 60,
        unit = "px",
        renderer =
          av_renderer(file = NULL, vfilter = "null", codec = NULL, audio = NULL))
# animate(p, renderer = gifski_renderer())
anim_save(path = "./output/figures/",
          # file = "animated_locations.gif",
          file = "animated_locations.mp4",
          animation = last_animation())

# Behaviour state switching check -----------------------------------------

simRes$loc_behave
behaveTrans <- list("vector", length(simRes$loc_behave))
behaveTransDF <- data.frame("behaveS" = rep(NA, length(simRes$loc_behave)),
                            "behaveE" = rep(NA, length(simRes$loc_behave)))
for(i in 1:length(simRes$loc_behave)){

  behaveTransDF[i,"behaveS"] <- simRes$loc_behave[i]
  behaveTransDF[i,"behaveE"] <- simRes$loc_behave[i+1]

  behaveTrans[i] <- paste0(simRes$loc_behave[i], "->", simRes$loc_behave[i+1])
}
table(unlist(behaveTrans))


observedBehaveChanges <- behaveTransDF %>%
  filter(!is.na(behaveE)) %>%
  group_by(behaveS, behaveE) %>%
  count() %>%
  group_by(behaveS) %>%
  mutate(totStepInState = sum(n),
         obsProb = n/totStepInState)
observedBehaveChanges

ggplot(observedBehaveChanges) +
  geom_raster(aes(x = behaveS, y = behaveE, fill = obsProb)) +
  scale_fill_scico(palette = "lajolla")

longBehaveMat <- reshape2::melt(behaveMatTest, c("behaveE", "behaveS"))

ggplot(longBehaveMat) +
  geom_raster(aes(x = behaveE, y = behaveS, fill = value)) +
  scale_fill_scico(palette = "lajolla")

data.frame(
  "i" = 1:length(simRes$loc_behave)/60,
  "behave" = simRes$loc_behave) %>%
  ggplot() +
  geom_path(aes(x = i, y = behave), size = 0.5, alpha = 0.5) +
  geom_point(aes(x = i, y = behave, colour = as.factor(behave)), size = 0.5) +
  scale_x_continuous(breaks = seq(0, 72, 12))

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
citation("CircStats")
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
