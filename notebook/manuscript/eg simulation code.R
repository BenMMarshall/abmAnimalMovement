
library(abmAnimalMovement) # core package
library(raster) # raster manipulation

# pre-made landscapes from the NLMR package can be found at https://doi.org/10.5281/zenodo.6992495
# to enable the running of this example
landscapeLayersList <- readRDS(here::here("notebook",
                                          "manuscript",
                                          "eg_simdata",
                                          paste0("eg_landscapedata_completelist.RDS")))

BADGER_shelter <- landscapeLayersList$shelter
BADGER_forage <- landscapeLayersList$forage
BADGER_move <- landscapeLayersList$movement

sampledShelters <- sampleRandom(raster(landscapeLayersList$shelter), 2,
                                ext = extent(0.45, 0.65, 0.45, 0.65),
                                rowcol = TRUE)

BADGER_shelterLocs <- data.frame(
  "x" = sampledShelters[,2],
  "y" = sampledShelters[,1])

BADGER_shelterSize <- 8

BADGER_k_step <- c(0.3*60, 1.25*60, 0.25*60)
BADGER_s_step <- c(0.8, 0.25, 0.5)
BADGER_mu_angle <- c(0, 0, 0)
BADGER_k_angle <- c(0.6, 0.99, 0.6)

BADGER_destinationRange <- c(3, 120)
BADGER_destinationDirection <- c(0, 0.01)
BADGER_destinationTransformation <- 2
BADGER_destinationModifier <- 2

BADGER_rescale <- 5

BADGER_avoidLocs <- data.frame(
  "x" = c(1205, 1500, 1165),
  "y" = c(980, 1090, 1250))

BADGER_avoidTransformation <- 2
BADGER_avoidModifier <- 4

BADGER_rest_Cycle <- c(0.12, 0, 24, 24)

# additional cycle
c0 <- c(0.075, 0, 24* (365/2), 24* 365) # seasonal

BADGER_additional_Cycles <- rbind(c0)
BADGER_additional_Cycles

b0 <- c(0.97, 0.01, 0.001) # shelter
b1 <- c(0.0002, 0.95, 0.0008) # explore/move
b2 <- c(0.001, 0.00001, 0.99) # forage
Default_behaveMatrix <- rbind(b0, b1, b2)
colnames(Default_behaveMatrix) <- c("b0", "b1", "b2")

BADGER_behaveMatrix <- Default_behaveMatrix

startLocation <- sample(900:1100, 2, replace = TRUE)

simSteps <- 24*60 *365
des_options <- 10
options <- 12

simOut <- abm_simulate(
  # a data frame with x and y coordinates
  start = startLocation,
  # an integer describing the length of the simulation
  timesteps = simSteps,
  # an integer describing the number of foraging destination options an animal
  # is offered
  des_options = 10,
  # an integer describing the number of movement options an animal is offered
  options = 12,
  # a data frame providing x and y coordinates of the shelter locations
  shelterLocations = BADGER_shelterLocs,
  # a value describing the radius around shelter sites that movement step
  # lengths are reduced
  shelterSize = BADGER_shelterSize,
  # a data frame providing x and y coordinates of the avoidance locations
  avoidPoints = BADGER_avoidLocs,
  #  a numeric vector of length two that contains the shape and scale values
  #  that describe the Gamma distribution for potential foraging destinations
  destinationRange = BADGER_destinationRange,
  #  a numeric vector of length two that contains the mean and concentration values
  #  that describe the Von Mises distribution for potential foraging destinations
  destinationDirection = BADGER_destinationDirection,
  # a value to choose the type of transformation applied to the animal's
  # attraction to a chosen destination
  destinationTransformation = BADGER_destinationTransformation,
  # a value modifying the animal's attraction to a chosen destination
  destinationModifier = BADGER_destinationModifier,
  # a value to chose the type of transformation applied to the animal's
  # avoidance to avoidance locations
  avoidTransformation = BADGER_avoidTransformation,
  # a value modifying the animal's avoidance of avoidance locations
  avoidModifier = BADGER_avoidModifier,
  # a vector of three numbers describing the three behavioural states' Gamma
  # distributions' shape parameter for step lengths
  k_step = BADGER_k_step,
  # a vector of three numbers describing the three behavioural states' Gamma
  # distributions' scale parameter for step lengths
  s_step = BADGER_s_step,
  # a vector of three numbers describing the three behavioural states' Von Mises
  # distributions' mean parameter for turn angles
  mu_angle = BADGER_mu_angle,
  # a vector of three numbers describing the three behavioural states' Von Mises
  # distributions' concentration parameter for turn angles
  k_angle = BADGER_k_angle,
  # a numeric value to specify the size of the environmental matrices cells
  rescale_step2cell = BADGER_rescale,
  # a 3x3 numeric matrix describing the transition probabilities between the
  # three behavioural states
  behave_Tmat = BADGER_behaveMatrix,
  # a vector length 4 for amplitude, midline, offset and frequency to define the
  # sheltering/active cycle
  rest_Cycle = BADGER_rest_Cycle,
  # a data.frame 4 columns wide for amplitude, midline, offset and frequency to
  # define any additional activity cycles, where each row is another cycle
  additional_Cycles = BADGER_additional_Cycles,
  # three arguments for the three matrices describing the landscape the
  # simulated animal occupies
  shelteringMatrix = BADGER_shelter,
  foragingMatrix = BADGER_forage,
  movementMatrix = BADGER_move)

simOut
