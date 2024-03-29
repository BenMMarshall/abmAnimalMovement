# both required for header files
# install.packages("Rcpp")
# install.packages("BH")
# library(Rcpp)
# library(BH)

library(abmAnimalMovement)
library(ggplot2)
library(reshape2)
library(patchwork)
library(scico)
library(dplyr)

# ??abmAnimalMovement::abm_simulate

##### SET CONSTANTS ##### ------------------------------------------------------

### PALETTE ###
palette <- c("#AD6DED", "#7D26D4", "#E87D13", "#965A1D", "#302010")
names(palette) <- c("purp1", "purp2", "2", "1", "0")
### SEED ###
set.seed(2022)

##### Generate environmental rasters ##### -------------------------------------

envNoiseTest <- genLandscape_noise(2000, 2000)
# quick_plot_matrix(envNoiseTest)

# envGradMat <- genLandscape_gradient(1000, 1000)
# quick_plot_matrix(envGradMat)

landscapeLayersList <- genLandscape_quickTriple(2000, 2000, seed = 1)

# Select which environment for post-simulation mapping
# plotBgEnv <- quick_plot_matrix(landcapeLayersList$memShelter)
# plotBgEnv <- quick_plot_matrix(landcapeLayersList$shelter)
# plotBgEnv <- quick_plot_matrix(landscapeLayersList$forage)

##### Create behavioural transitional matrix ###### ----------------------------

b0 <- c(0.98, 0.001, 0.00001) # rest
b1 <- c(0.0005, 0.99, 0.02) # explore/move
b2 <- c(0.0005, 0.02, 0.97) # forage

behaveMatTest <- rbind(b0, b1, b2)

behaveMatTest[1,]

shelterLocs <- data.frame(
  "x" = c(1000, 990),
  "y" = c(975, 960)
)

avoid <- data.frame(
  "x" = c(950, 950, 950, 950),
  "y" = c(950, 960, 970,  1010)
)
# avoid <- data.frame(
#   "x" = rep(950, 10),
#   "y" = seq(950, 1010, length.out = 20)
# )

restData <- c(0.65, 0, 24, 24)

## multiple cycle additions
c0 <- c(0.2, 0, 24, 24*4) # digest
c1 <- c(0.15, 0, 24, 24*28) # seasonal

cycleMat <- rbind(c0, c1)

cycleMat[1,]

# how long to run the sim for
simSteps <- 24*60 *28
##### Run core simulation function ##### ---------------------------------------

simRes <- abm_simulate(start = c(900,1000),
                       timesteps = simSteps,
                       des_options = 10,
                       # options = 5,
                       options = 12,
                       k_step = c(1, 3, 2),
                       s_step = c(0.5, 1, 1),
                       mu_angle = c(0, 0, 0),
                       k_angle = c(0.6, 0.99, 0.6),
                       rescale_step2cell = 1,

                       shelterLocations = shelterLocs,
                       shelterSize = 1,
                       avoidPoints = avoid,

                       destinationRange = c(35, 2),
                       destinationDirection = c(0, 0.01),
                       destinationTransformation = 2,
                       destinationModifier = 2,
                       avoidTransformation = 2,
                       avoidModifier = 4,

                       behave_Tmat = behaveMatTest,

                       rest_Cycle = restData,
                       additional_Cycles = cycleMat,

                       shelteringMatrix = landscapeLayersList$shelter,
                       foragingMatrix = landscapeLayersList$forage,
                       movementMatrix = landscapeLayersList$movement) # just using a place holder layer for testing

simRes$destinations
# sum(simRes$others$destinations_list$des_desBehave)
# data.frame(
#   simRes$others$destinations_list$des_desOptsstep,
#   simRes$others$destinations_list$des_desOptsx,
#   simRes$others$destinations_list$des_desOptsy,
#   simRes$others$destinations_list$des_desBehave
# ) %>%
#   filter(simRes.others.destinations_list.des_desBehave==0)

##### Quick plot testing ##### ------------------------------

abmMapPlot(simResults = simRes,
           # shelterLocations = shelterLocs,
           # avoidance = avoid,
           timeLimits = c(1, 60*24* 3))
# ggsave("./notebook/intermediate_code/output/figures/movement.pdf")
timeLimits <- c(1, 60*24* 3)
optionData <- simRes$options
optionData <- optionData[optionData$step > timeLimits[1] &
                           optionData$step < timeLimits[2],]
range(optionData$x)
range(optionData$y)
quick_plot_matrix(landcapeLayersList$forage) +
  coord_cartesian(xlim = range(optionData$x),
                  ylim = range(optionData$y)) +
  theme(aspect.ratio = 1) +
  scale_fill_gradient(low = "#cc6e10", high = "#f5bc82")
ggsave("./notebook/intermediate_code/output/figures/raster_F.pdf", width = 9, height = 9)

quick_plot_matrix(landcapeLayersList$shelter) +
  coord_cartesian(xlim = range(optionData$x),
                  ylim = range(optionData$y)) +
  theme(aspect.ratio = 1) +
  scale_fill_gradient(low = "#965A1D", high = "#e3aa6f")
ggsave("./notebook/intermediate_code/output/figures/raster_M.pdf", width = 9, height = 9)

abmCyclePlot(simResults = simRes,
             timeLimits = c(1, 60*24* 7))

###### THERE IS A POSSIBLE ISSUE WITH THE FIRST SET OF OPTIONS step 0
simRes$options[simRes$options$x< 500,]
######################################################################

##### Behaviour state cycling and switching ##### ------------------------------

# number of hours
simSteps/60

### CYCLING ###
behaveProb <- sapply(1:simSteps/60, function(x){
  cycle_draw(x, restData[1], restData[2], restData[3], restData[4])
})
behaviourPlotData <- data.frame(
  "i" = 1:length(simRes$locations$behave)/60,
  "behaveObs" = simRes$locations$behave,
  "behaveRest" = behaveProb
)

cycleMat

behAddCycleVec <- vector("list", length = nrow(cycleMat))
for(i in 1:nrow(cycleMat)){
  behaveProb <- sapply(1:simSteps/60, function(x){
    cycle_draw(x, cycleMat[i,1], cycleMat[i,2], cycleMat[i,3], cycleMat[i,4])
  })

  behAddCycleVec[[i]] <- data.frame(
    "i" = 1:length(simRes$locations$behave)/60,
    "behaveObs" = simRes$locations$behave,
    "behaveRest" = behaveProb,
    "cycle" = paste0("cycle_", i)
  )
}

plotList <- lapply(behAddCycleVec, function(x){
  plotExpRest <- ggplot(x) +
     geom_path(aes(x = i, y = behaveRest), colour = palette["0"]) +
     scale_x_continuous(breaks = seq(0, simSteps/60, 24)) +
     theme_bw() +
     theme(legend.position = "none",
           axis.title = element_text(angle = 0,
                                     face = 2,
                                     hjust = 1),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_text(angle = 0,
                                       face = 2,
                                       hjust = 1),
           plot.background = element_blank(),
           # panel.background = element_blank(),
           panel.border = element_blank(),
           panel.grid = element_blank(),
           axis.line = element_line(size = 0.5),

           panel.grid.major.y = element_line(linetype = 2,
                                             size = 0.5,
                                             colour = "grey75")
     ) +
     labs(y = "Rest prob.\nmodifier", x = "Hour")
})

# depends on the offest and starting scenario
daylight <- data.frame(
  "sDay" = seq(6,simSteps/60-18,24),
  "eDay" = seq(18,simSteps/60,24)
  )

(plotObsBehave <- ggplot(behaviourPlotData) +
    geom_rect(data = daylight, aes(xmin = sDay, xmax = eDay,
                                   ymin = 0.95, ymax = 1.05),
              fill = "yellow", alpha = 0.25) +
    geom_path(aes(x = i, y = behaveObs), size = 0.5, alpha = 0.5) +
    geom_point(aes(x = i, y = behaveObs, colour = as.factor(behaveObs)), size = 0.5) +
    scale_x_continuous(breaks = seq(0, simSteps/60, 24)) +
    scale_y_continuous(breaks = c(0, 1, 2),
                       labels = c("0 - Rest", "1 - Explore", "2 - Forage")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1),
          axis.title.y = element_text(angle = 0,
                                      face = 2,
                                      hjust = 1),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          plot.background = element_blank(),
          # panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(size = 0.5),

          panel.grid.major.y = element_line(linetype = 2,
                                            size = 0.5,
                                            colour = "grey75")
          ) +
    scale_colour_manual(values = palette[3:5]) +
    labs(colour = "Behaviour"))

(plotExpRest <- ggplot(behaviourPlotData) +
    geom_path(aes(x = i, y = behaveRest), colour = palette["0"]) +
    scale_x_continuous(breaks = seq(0, simSteps/60, 24)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(angle = 0,
                                      face = 2,
                                      hjust = 1),
          plot.background = element_blank(),
          # panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(size = 0.5),

          panel.grid.major.y = element_line(linetype = 2,
                                            size = 0.5,
                                            colour = "grey75")
    ) +
    labs(y = "Rest prob.\nmodifier", x = "Hour"))

plotList[[length(plotList)]] <- plotList[[length(plotList)]] +
  theme(axis.text.x = element_text(),
        axis.title.x = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1))

plotObsBehave / plotExpRest / do.call(patchwork::wrap_plots, c(plotList, ncol=1)) +
  plot_layout(heights = c(1,
                          1/(nrow(cycleMat)+1),
                          1-(1/(nrow(cycleMat)+1))
                          ))

ggsave("./notebook/intermediate_code/output/figures/behaviourCycle.png",
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/behaviourCycle.pdf",
       width = 180, height = 120, units = "mm")

### BEHAVE TRANSITIONS ###
simRes$locations$behave
behaveTrans <- list("vector", length(simRes$locations$behave))
behaveTransDF <- data.frame("behaveS" = rep(NA, length(simRes$locations$behave)),
                            "behaveE" = rep(NA, length(simRes$locations$behave)))
for(i in 1:length(simRes$locations$behave)){

  behaveTransDF[i,"behaveS"] <- simRes$locations$behave[i]
  behaveTransDF[i,"behaveE"] <- simRes$locations$behave[i+1]

  behaveTrans[i] <- paste0(simRes$locations$behave[i], "->", simRes$locations$behave[i+1])
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


observedBehaveChanges <- observedBehaveChanges %>%
  select(behaveS, behaveE, "value" = obsProb) %>%
  mutate(ObsExp = "Observed")

longBehaveMat <- reshape2::melt(behaveMatTest, c("behaveE", "behaveS"))
expectedBehaveChanges <- longBehaveMat %>%
          mutate(ObsExp = "Expected",
                 behaveE = as.numeric(sub("^.", "", behaveE)),
                 behaveS = behaveS - 1)

rbind(observedBehaveChanges, expectedBehaveChanges) %>%
  ggplot() +
  geom_raster(aes(x = behaveE, y = behaveS, fill = value)) +
  geom_text(aes(x = behaveE, y = behaveS, label = round(value, digits = 3))) +
  scale_fill_scico(palette = "lajolla", begin = 0.1, end = 0.95) +
  facet_wrap(ObsExp~.) +
  coord_cartesian(expand = 0) +
  scale_y_continuous(breaks = c(0, 1, 2),
                     labels = c("0 - Rest", "1 - Explore", "2 - Forage")) +
  scale_x_continuous(breaks = c(0, 1, 2),
                     labels = c("0 - Rest", "1 - Explore", "2 - Forage")) +
  theme_bw() +
  theme(legend.position = "none",
        aspect.ratio = 1,
        axis.title = element_text(angle = 0,
                                  face = 2,
                                  hjust = 0.5),
        axis.title.y = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1),
        # plot.background = element_blank(),
        # panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = 4,
                                  hjust = 0),
        axis.line = element_line(size = 0.5)) +
  labs(x = "Ending behaviour", y = "Starting\nbehaviour")

ggsave("./notebook/intermediate_code/output/figures/behaviourTransitions.png",
       width = 180, height = 180, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/behaviourTransitions.pdf",
       width = 180, height = 180, units = "mm")

##### Review step lengths and turn angles ##### --------------------------------

### STEP LENGTHS ###
stepData <- simRes$locations %>%
  mutate(sl = sqrt(
    (x - lag(x))^2 +
      (y - lag(y))^2))

stepText <- stepData %>%
  mutate(lessShelter = sl < simRes$inputs$in_sSiteSize) %>%
  group_by(behave,
           lessShelter) %>%
  count() %>%
  filter(!is.na(lessShelter)) %>%
  group_by(behave) %>%
  mutate(total = sum(n),
         per = round(n/total*100, digits = 1)) %>%
  filter(lessShelter) %>%
  summarise(
    behave = behave[1],
    text = paste0("Zero rate: ", n, "/", total, " (", per, "%)"))

stepData %>%
  ggplot() +
  geom_density(aes(x = sl, fill = as.factor(behave)),
               colour = NA) +
  geom_text(data = stepText,
    aes(x = Inf, y = Inf, label = text),
    hjust = 1, vjust = 1, fontface = 3) +
  facet_wrap(behave~., ncol = 1, scales = "free_y",
             labeller = labeller(.cols = c("0" = "0 - Rest",
                                           "1" = "1 - Explore",
                                           "2" = "2 - Forage"))
  ) +
  scale_fill_manual(values = palette[c("0", "1", "2")]) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(angle = 0,
                                  face = 2,
                                  hjust = 0.5),
        axis.title.y = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = 4,
                                  hjust = 0),
        axis.line = element_line(size = 0.5)) +
  labs(x = "Step length (m)", y = "Density")

ggsave("./notebook/intermediate_code/output/figures/stepBehaviours.png",
       width = 180, height = 160, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/stepBehaviours.pdf",
       width = 180, height = 160, units = "mm")

### TURN ANGLE ###
for(beh in 0:2){

  taPlot <- simRes$locations %>%
    filter(sl > 1, behave == beh) %>%
    ggplot() +
    geom_histogram(aes(x = ta, fill = as.factor(behave)),
                   colour = NA, binwidth = 10) +
    facet_wrap(behave~., ncol = 1,
               labeller = labeller(.cols = c("0" = "0 - Rest",
                                             "1" = "1 - Explore",
                                             "2" = "2 - Forage"))
    ) +
    scale_x_continuous(breaks = seq(-135, 180, 45),
                       limits = c(-180, 180),
                       expand = c(0,0)
    ) +
    coord_polar(theta = "x", start = pi) +
    scale_fill_manual(values = palette[c("0", "1", "2")]) +
    theme_bw() +
    theme(legend.position = "none",
          aspect.ratio = 1,
          axis.title = element_text(angle = 0,
                                    face = 2,
                                    hjust = 0.5),
          axis.title.y = element_text(angle = 0,
                                      face = 2,
                                      hjust = 1),
          axis.text.x = element_text(
            angle = 0, size = 6, hjust = 0.5, vjust = 0.5),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, face = 4,
                                    hjust = 0),
          axis.line = element_line(size = 0.5)) +
    labs(x = "Turn angle (degrees)", y = "Count")

  assign(paste0("taPlot_", beh), taPlot)

}

# have to loop and combine to fix y axis issues
taPlot_0 / taPlot_1 / taPlot_2

ggsave("./notebook/intermediate_code/output/figures/turnangleBehaviours.png",
       width = 180, height = 160, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/turnangleBehaviours.pdf",
       width = 180, height = 160, units = "mm")

##### Mapping movements ##### --------------------------------------------------

plotBgEnv +
  geom_point(data = simRes$options,
             aes(x = x, y = y),
             size = 0.35,
             alpha = 0.05, colour = palette["purp1"]) +
  geom_path(data = simRes$locations,
            aes(x = x, y = y), alpha = 0.15) +
  geom_point(data = simRes$locations,
             size = 0.65,
             aes(x = x, y = y, shape = as.factor(behave)),
             alpha = 0.45) +
  ## forage
  geom_point(data = simRes$locations,
             aes(x = destination_x, y = destination_y),
             pch = 17,
             size = 2, colour = palette["2"],
             alpha = 0.85) +
  ## shelter
  geom_point(data = shelterLocs,
             aes(x = x, y = y),
             pch = 16,
             size = 4, colour = palette["0"],
             alpha = 1) +
  geom_point(data = shelterLocs,
             aes(x = x, y = y),
             pch = "S",
             size = 3, colour = "white",
             alpha = 1) +
  ## avoidance
  geom_point(data = avoid,
             aes(x = x, y = y),
             pch = 16,
             size = 4, colour = "white",
             alpha = 1) +
  geom_point(data = avoid,
             aes(x = x, y = y),
             pch = "A",
             size = 3, colour = "black",
             alpha = 1) +
  scale_colour_scico(palette = "buda") +
  coord_cartesian(xlim = range(simRes$locations$x), ylim = range(simRes$locations$y)) +
  # coord_cartesian(xlim = range(simRes$options$x), ylim = range(simRes$options$y)) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.title = element_text(angle = 0,
                                  face = 2,
                                  hjust = 1),
        axis.title.y = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1),
        # plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5)) +
  labs(x = "Easting", y = "Northing", shape = "Behaviour", fill = "Environmental\nquality")

ggsave("./notebook/intermediate_code/output/figures/overallMapping.png",
       width = 180, height = 180, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/overallMapping.pdf",
       width = 180, height = 180, units = "mm")

##### Sampling function testing ##### ------------------------------------------

# https://www.r-bloggers.com/2018/09/using-rs-set-seed-to-set-seeds-for-use-in-c-c-including-rcpp/
get_seed <- function() {
  sample.int(.Machine$integer.max, 1)
}

ndraws <- 10000

sampleOut <- NULL
for(i in 1:ndraws){
  sampleOut[i] <- sample_options(c(2, -0.2, 5, 0.5, 0.05), get_seed())
}
# hist(sampleOut)
# table(sampleOut) / ndraws

data.frame(
  "value" = c("2", "5", "0.5", "0.05", "-0.2"),
  "result" = c(table(sampleOut), 0)) %>%
  ggplot() +
  geom_col(aes(x = reorder(value, -result), y = result)) +

  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(angle = 0,
                                  face = 2,
                                  hjust = 0.5),
        axis.title.y = element_text(angle = 0,
                                    face = 2,
                                    hjust = 1),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = 4,
                                  hjust = 0),
        axis.line = element_line(size = 0.5)) +
  labs(x = "Probability", y = paste0("Count\n(of ", ndraws, ")"))

ggsave("./notebook/intermediate_code/output/figures/sampleOptions.png",
       width = 160, height = 160, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/sampleOptions.pdf",
       width = 160, height = 160, units = "mm")

##### Von-Mises draw testing ##### ---------------------------------------------

citation("CircStats")
# mu should vary between 0 and 2*pi for circular stuff

vonOut <- vonmises(N = 1000, MU = 0, KAPPA = 0.1)
hist(vonOut)

vonResVarying <- do.call(rbind, lapply(
  c(0.01, seq(0.05, 0.8, 0.1)),
  function(k){
    print(k)
    return(data.frame(kappa = paste("KAPPA =",
                                    format(k, nsmall = 2)), # format forces the zeroes to remain
                      draws = vonmises(N = 2000, MU = 0, KAPPA = k)))
  }))

sum(vonResVarying$draws > pi)
sum(vonResVarying$draws < -pi)

# \u03c0 is unicode for pi
## display the varying levels of angle concentration.
ggplot() +
  geom_histogram(data = vonResVarying, aes(x = draws),
                 fill = "black", alpha = 0.5,
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
  theme(axis.text.x = element_text(vjust = 1),
        strip.text = element_text(face = 4, hjust = 0.5),
        # panel.grid.major.x = element_line(colour = "grey15"),
        plot.background = element_rect(colour = NA, fill = "white"),
        panel.background = element_rect(colour = NA, fill = "white")) +
  facet_wrap(.~kappa)

ggsave("./notebook/intermediate_code/output/figures/vonmisesKappa.png",
       width = 180, height = 180, units = "mm", dpi = 300)
ggsave("./notebook/intermediate_code/output/figures/vonmisesKappa.pdf",
       width = 180, height = 180, units = "mm")

##### Animate movement ##### ---------------------------------------------------

# install.packages("gifski")
# install.packages("av")
library(gganimate)
library(dplyr)

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
anim_save(path = "./notebook/intermediate_code/output/figures/",
          # file = "animated_locations.gif",
          file = "animated_locations.mp4",
          animation = last_animation())

