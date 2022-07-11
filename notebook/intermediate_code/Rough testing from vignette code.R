
b0 <- c(0.97, 0.01, 0.001) # rest
b1 <- c(0.0002, 0.95, 0.0008) # explore/move
b2 <- c(0.001, 0.00001, 0.99) # forage

# W <- b2
# ((W - min(W)) /
#   (max(W) - min(W)))

(Default_behaveMatrix <- rbind(b0, b1, b2))

BADGER_rest_Cycle <- c(0.12, 0, 24, 24)

# additional cycle
c0 <- c(0.075, 0, 24* (365/2), 24* 365) # seasonal

BADGER_additional_Cycles <- rbind(c0)
BADGER_additional_Cycles

BADGER_behaveMatrix <- Default_behaveMatrix
# BADGER_behaveMatrix[1,1] <- 0.85
# BADGER_behaveMatrix[1,2] <- 0.1
# BADGER_behaveMatrix[1,3] <- 0.05
# BADGER_behaveMatrix[2,3] <- 0.002
BADGER_behaveMatrix

VULTURE_rest_Cycle <- c(0.1, 0, 24, 24)

## multiple cycle additions
c0 <- c(0.025, 0, 24* (365/2), 24* 365) # seasonal

VULTURE_additional_Cycles <- rbind(c0)

VULTURE_behaveMatrix <- Default_behaveMatrix
# VULTURE_behaveMatrix[2,2] <- 0.990
VULTURE_behaveMatrix[2,3] <- 0.0002
VULTURE_behaveMatrix[3,2] <- 0.000015
# VULTURE_behaveMatrix[1,1] <- 0.6
# VULTURE_behaveMatrix[1,2] <- 0.01
# VULTURE_behaveMatrix[3,3] <- 0.999

KINGCOBRA_rest_Cycle <- c(0.14, 0, 24, 24)

## multiple cycle additions
c0 <- c(0.12, 0, 24, 24*4) # digest
c1 <- c(0.05, 0, 24 * (365/2), 24* 365 ) # seasonal

KINGCOBRA_additional_Cycles <- rbind(c0, c1)

KINGCOBRA_behaveMatrix <- Default_behaveMatrix
KINGCOBRA_behaveMatrix[1,1] <- 0.95
KINGCOBRA_behaveMatrix[1,2] <- 0.005
KINGCOBRA_behaveMatrix[3,1] <- 0.00025
KINGCOBRA_behaveMatrix[3,2] <- 0.00001
KINGCOBRA_behaveMatrix[3,3] <- 0.999

simSteps <- 24*60 *365
des_options <- 10
options <- 12

vecSpecies <- c("BADGER", "VULTURE", "KINGCOBRA")
simResultsList <- vector("list", length = 3)
names(simResultsList) <- vecSpecies

for(species in vecSpecies){
  print(species)
  # species <- vecSpecies[1]
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

}

cyclePlotList <- vector("list", length = 3)
names(cyclePlotList) <- vecSpecies
for(species in vecSpecies){

  simRes <- simResultsList[[species]]

  ### CYCLING ###
  behaveProb <- sapply(1:simSteps/60, function(x){
    cycle_draw(x,
               simRes$inputs$in_rest_Cycle_A,
               simRes$inputs$in_rest_Cycle_M,
               simRes$inputs$in_rest_Cycle_PHI,
               simRes$inputs$in_rest_Cycle_TAU)
  })
  behaviourPlotData <- data.frame(
    "i" = 1:simSteps,
    "behaveObs" = simRes$locations$behave,
    "behaveRest" = behaveProb,
    "cycle" = "rest"
  )

  behAddCycleVec <- vector("list",
                           length = length(simRes$inputs$in_add_Cycle_A))
  for(i in 1:length(simRes$inputs$in_add_Cycle_A)){
    behaveProb <- sapply(1:simSteps/60, function(x){
      cycle_draw(x,
                 simRes$inputs$in_add_Cycle_A[i],
                 simRes$inputs$in_add_Cycle_M[i],
                 simRes$inputs$in_add_Cycle_PHI[i],
                 simRes$inputs$in_add_Cycle_TAU[i])
    })

    behAddCycleVec[[i]] <- data.frame(
      "i" = 1:simSteps,
      "behaveObs" = simRes$locations$behave,
      "behaveRest" = behaveProb,
      "cycle" = paste0("cycle_", i)
    )
  }

  cycleData <- rbind(behaviourPlotData, do.call(rbind, behAddCycleVec))

  zoom_breaks <- function(x){
    if(max(x) > 5){
      seq(0, 31*3, 7)
    }else{
      seq(0, 4, 1)
    }
  }

  tempPlot <- cycleData %>%
    mutate(i = i/60/24) %>%
    filter(i < 31*3) %>%
    ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = seq(-1,1,0.5), linetype = 2,
                        colour = "grey85") +
    ggplot2::geom_path(ggplot2::aes(x = i, y = behaveRest, group = cycle),
                       colour = ifelse(
                         species == "BADGER", "#4F0E99", ifelse(species == "VULTURE",
                                                                "#7D26D4",
                                                                "#AD6DED")
                       )) +
    ggplot2::geom_line(ggplot2::aes(x = i, y = behaveObs + 1.4), size = 0.5,
                       colour = "grey75") +
    ggplot2::geom_point(ggplot2::aes(x = i, y = behaveObs + 1.4,
                                     colour = as.factor(behaveObs + 1.4)),
                        size = 0.5, pch = ".") +
    ggplot2::scale_y_continuous(breaks = c(seq(-0.5, 0.5, 0.5), 1.4, 2.4, 3.4),
                                labels = c(seq(-0.5, 0.5, 0.5),
                                           "<span style='color:#302010'>0 - Shelter</span>",
                                           "<span style='color:#965A1D'>1 - Explore</span>",
                                           "<span style='color:#E87D13'>2 - Forage</span>")) +
    ggplot2::scale_colour_manual(values = unname(palette[c("0", "1", "2")])) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(colour = "#191919"),
                   line = ggplot2::element_line(colour = "#191919"),
                   axis.title = ggplot2::element_text(angle = 0,
                                                      size = 8,
                                                      face = 2,
                                                      hjust = 1),
                   axis.title.y = ggplot2::element_text(angle = 0,
                                                        face = 2,
                                                        hjust = 1,
                                                        vjust = 0.25,
                                                        margin = margin(0,10,0,10)),
                   plot.title = element_markdown(size = 12, face = 2),
                   plot.title.position = "plot",
                   axis.text = element_text(size = 5),
                   axis.text.y = element_markdown(),
                   strip.background = element_rect(fill = "grey90", colour = NA),
                   plot.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(size = 0.5),
                   panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(y = ifelse(species == "BADGER",
                             "Observed\nbehaviour\n&\nRest prob.\nmodifier", ""),
                  x = "Days",
                  title = ifelse(
                    species == "BADGER",
                    "<span style='color:#4F0E99'>A - Badger</span>",
                    ifelse(species == "VULTURE",
                           "<span style='color:#7D26D4'>B - Vulture</span>",
                           "<span style='color:#AD6DED'>C - King Cobra</span>")
                  )) +
    ggforce::facet_zoom(xlim = c(0, 4)) +
    ggplot2::scale_x_continuous(breaks = zoom_breaks)

  cyclePlotList[[species]] <- tempPlot

}

# cyclePlotList$BADGER
# cyclePlotList$VULTURE
# cyclePlotList$KINGCOBRA
patchwork::wrap_plots(cyclePlotList, ncol = 1)


# options(dplyr.summarise.inform = FALSE)
#
# tempList <- vector("list", length(vecSpecies))
# i <- 0
# for(species in vecSpecies){
#   i <- i+1
#   simResultsList[[species]]$locations$species <- species
#   tempList[[i]] <- simResultsList[[species]]$locations
# }
# allSimLocations <- do.call(rbind, tempList)
#
# allSimLocations %>%
#   mutate(
#     day = floor(timestep/60/24)+1) %>%
#   group_by(species, day) %>%
#   summarise(perRest = sum(behave == 0) / (60*24) *100) %>%
#   ungroup() %>%
#   mutate(species = factor(
#     case_when(
#       species == "BADGER" ~ "<span style='color:#4F0E99'>Badger</span>",
#       species == "VULTURE" ~ "<span style='color:#7D26D4'>Vulture</span>",
#       species == "KINGCOBRA" ~ "<span style='color:#AD6DED'>King Cobra</span>"
#     ), levels = c(
#       "<span style='color:#4F0E99'>Badger</span>",
#       "<span style='color:#7D26D4'>Vulture</span>",
#       "<span style='color:#AD6DED'>King Cobra</span>"))) %>%
#   ggplot() +
#   geom_col(aes(x = day, y = perRest, fill = species)) +
#   scale_fill_manual(values = unname(palette[c("BADGER", "VULTURE", "KING\nCOBRA")])) +
#   scale_x_continuous(breaks = c(seq(0,300,100), 365)) +
#   facet_wrap(.~species, ncol = 1) +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "none",
#                  text = ggplot2::element_text(colour = "#191919"),
#                  line = ggplot2::element_line(colour = "#191919"),
#                  axis.title = ggplot2::element_text(angle = 0,
#                                                     face = 2,
#                                                     size = 8,
#                                                     hjust = 1),
#                  axis.title.y = ggplot2::element_text(angle = 0,
#                                                       face = 2,
#                                                       hjust = 1,
#                                                       vjust = 1,
#                                                       margin = margin(0,10,0,10)),
#                  plot.title = element_text(size = 10, face = 2),
#                  plot.title.position = "plot",
#                  axis.text = element_text(size = 6),
#                  axis.text.y = element_markdown(),
#                  strip.background = element_rect(fill = NA, colour = NA),
#                  strip.text = element_markdown(face = 4, hjust = 0, size = 8),
#                  plot.background = ggplot2::element_blank(),
#                  panel.border = ggplot2::element_blank(),
#                  panel.grid = ggplot2::element_blank(),
#                  axis.line = ggplot2::element_line(size = 0.5),
#                  panel.grid.major.y = ggplot2::element_blank()
#   ) +
#   ggplot2::labs(y = "Daily\npercentage\nresting",
#                 x = "Days")
