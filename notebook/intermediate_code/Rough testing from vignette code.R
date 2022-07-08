
# simSteps <- 24*60 *365
simSteps <- 24*60 *90
des_options <- 10
options <- 12

vecSpecies <- c("BADGER", "VULTURE", "KINGCOBRA")
simResultsList <- vector("list", length = 3)
names(simResultsList) <- vecSpecies

species <- "BADGER"
species <- "VULTURE"
# species <- "KINGCOBRA"

simResultsList[[species]] <- abm_simulate(

  # constants between all species simulations
  start = startLocation,
  timesteps = simSteps,
  des_options = des_options,
  options = options,

  k_step = if(length(ls(pattern = paste0("^", species, ".*k_step"))) > 0){
    get(ls(pattern = paste0("^", species, ".*k_step")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  s_step = if(length(ls(pattern = paste0("^", species, ".*s_step"))) > 0){
    get(ls(pattern = paste0("^", species, ".*s_step")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  mu_angle = if(length(ls(pattern = paste0("^", species, ".*mu_angle"))) > 0){
    get(ls(pattern = paste0("^", species, ".*mu_angle")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  k_angle = if(length(ls(pattern = paste0("^", species, ".*k_angle"))) > 0){
    get(ls(pattern = paste0("^", species, ".*k_angle")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  rescale_step2cell = if(length(ls(pattern = paste0("^", species, ".*rescale"))) > 0){
    get(ls(pattern = paste0("^", species, ".*rescale")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  shelterLocations = if(length(ls(pattern = paste0("^", species, ".*shelterLocs"))) > 0){
    get(ls(pattern = paste0("^", species, ".*shelterLocs")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  shelterSize = if(length(ls(pattern = paste0("^", species, ".*shelterSize"))) > 0){
    get(ls(pattern = paste0("^", species, ".*shelterSize")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  avoidPoints = if(length(ls(pattern = paste0("^", species, ".*avoidLocs"))) > 0){
    get(ls(pattern = paste0("^", species, ".*avoidLocs")))
  } else {
    # DEFAULT HERE, required a location, but if avoidance not provided see 0
    # weighting in avoidModifier
    data.frame("x" = startLocation[1],
               "y" = startLocation[2])
  },

  destinationRange = if(length(ls(pattern = paste0("^", species, ".*destinationRange"))) > 0){
    get(ls(pattern = paste0("^", species, ".*destinationRange")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  destinationDirection = if(length(ls(pattern =
                                      paste0("^", species, ".*destinationDirection"))) > 0){
    get(ls(pattern = paste0("^", species, ".*destinationDirection")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  destinationTransformation = if(length(ls(pattern =
                                           paste0("^", species, ".*destinationTransformation"))) > 0){
    get(ls(pattern = paste0("^", species, ".*destinationTransformation")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  destinationModifier = if(length(ls(pattern = paste0("^", species, ".*destinationModifier"))) > 0){
    get(ls(pattern = paste0("^", species, ".*destinationModifier")))
  } else {
    # DEFAULT HERE, Shouldn't reach here when knit
    stop("Missing parameter in sim loop.")
  },

  avoidTransformation = if(length(ls(pattern = paste0("^", species, ".*avoidTransformation"))) > 0){
    get(ls(pattern = paste0("^", species, ".*avoidTransformation")))
  } else {
    0
  },

  avoidModifier = if(length(ls(pattern = paste0("^", species, ".*avoidModifier"))) > 0){
    get(ls(pattern = paste0("^", species, ".*avoidModifier")))
  } else {
    0
  },

  behave_Tmat = if(length(ls(pattern = paste0("^", species, ".*behaveMatrix"))) > 0){
    get(ls(pattern = paste0("^", species, ".*behaveMatrix")))
  } else {
    # DEFAULT HERE
    Default_behaveMatrix
  },

  rest_Cycle = if(length(ls(pattern = paste0("^", species, ".*rest_Cycle"))) > 0){
    get(ls(pattern = paste0("^", species, ".*rest_Cycle")))
  } else {
    # DEFAULT HERE, Diurnal rest cycle, but may not work well with additional cycles
    c(0.65, 0, 24, 24)
  },

  additional_Cycles = if(length(ls(pattern = paste0("^", species, ".*additional_Cycles"))) > 0){
    get(ls(pattern = paste0("^", species, ".*additional_Cycles")))
  } else {
    # DEFAULT HERE
    BADGER_additional_Cycles
  },

  shelteringMatrix = if(length(ls(pattern = paste0("^", species, ".*shelteringMatrix"))) > 0){
    get(ls(pattern = paste0("^", sp, ".*shelteringMatrix")))
  } else {
    # DEFAULT HERE
    landscapeLayersList$shelter
  }
  ,
  foragingMatrix = if(length(ls(pattern = paste0("^", species, ".*forageMatrix"))) > 0){
    get(ls(pattern = paste0("^", species, ".*forageMatrix")))
  } else {
    # DEFAULT HERE
    landscapeLayersList$forage
  }
  ,
  movementMatrix = if(length(ls(pattern = paste0("^", species, ".*movementMatrix"))) > 0){
    get(ls(pattern = paste0("^", species, ".*movementMatrix")))
  } else {
    # DEFAULT HERE
    landscapeLayersList$movement
  }
)

simResultsList$KINGCOBRA$locations
View(simResultsList$KINGCOBRA$locations)
