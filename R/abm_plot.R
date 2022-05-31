#' @title Plot simulation outputs
#'
#' @param simResults The resulting object created by [abm_simuate()].
#' @param timeLimits The start timestep of data to be plotted, can be helpful for
#'   plotting a more manageable section of large simulation (optional).
#' @name abm_plot
#' @return Returns a ggplot2 plot
#'
#' @seealso [abm_simuate()] for the required input.
#'
#' @useDynLib abmAnimalMovement
NULL

#' @rdname abm_plot
#' @export
abmMapPlot <- function(simResults,
                       timeLimits = NA,
                       plotOptions = TRUE
){

  palette <- c("#AD6DED", "#7D26D4", "#E87D13", "#965A1D", "#302010")
  names(palette) <- c("purp1", "purp2", "2", "1", "0")

  realisedData <- simResults$locations
  optionData <- simResults$options

  if(!is.na(timeLimits[1]) & !is.na(timeLimits[2])){
    realisedData <- realisedData[realisedData$timestep > timeLimits[1] &
                                   realisedData$timestep < timeLimits[2],]
    optionData <- optionData[optionData$timestep > timeLimits[1] &
                               optionData$timestep < timeLimits[2],]
  }

  realisedData$behave[realisedData$behave == 0] <- "0 - Resting"
  realisedData$behave[realisedData$behave == 1] <- "1 - Exploring"
  realisedData$behave[realisedData$behave == 2] <- "2 - Foraging"

  shelterLocations <- data.frame(
    "x" = simResults$inputs$in_shelter_locs_x,
    "y" = simResults$inputs$in_shelter_locs_y
  )

  avoidance <- data.frame(
    "x" = simResults$inputs$in_avoidPoints_x,
    "y" = simResults$inputs$in_avoidPoints_y
  )

  outPlot <- ggplot2::ggplot() +
    ggplot2::geom_path(data = realisedData,
                       ggplot2::aes(x = x, y = y),
                       alpha = 0.15) +
    ggplot2::geom_point(data = realisedData,
                        size = 0.65,
                        ggplot2::aes(x = x, y = y, shape = as.factor(behave)),
                        alpha = 0.45,
                        colour = "grey10") +
    ## forage
    ggplot2::geom_point(data = realisedData,
                        ggplot2::aes(x = destination_x, y = destination_y),
                        pch = 15,
                        size = 0.25, colour = palette["2"],
                        alpha = 0.25) +
    ## shelter
    ggplot2::geom_point(data = shelterLocations,
                        ggplot2::aes(x = x, y = y),
                        pch = 16,
                        size = 4, colour = palette["0"],
                        alpha = 1) +
    ggplot2::geom_point(data = shelterLocations,
                        ggplot2::aes(x = x, y = y),
                        pch = "S",
                        size = 3, colour = "grey90",
                        alpha = 1) +
    ## avoidance
    ggplot2::geom_point(data = avoidance,
                        ggplot2::aes(x = x, y = y),
                        pch = 16,
                        size = 4, colour = "grey90",
                        alpha = 1) +
    ggplot2::geom_point(data = avoidance,
                        ggplot2::aes(x = x, y = y),
                        pch = "A",
                        size = 3, colour = "grey10",
                        alpha = 1) +
    scico::scale_colour_scico(palette = "buda") +
    ggplot2::coord_fixed() +
    # coord_cartesian(xlim = range(realisedData$x), ylim = range(realisedData$y)) +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1,
                   legend.position = "left",
                   legend.justification = "top",
                   legend.title = ggplot2::element_text(face = 2),
                   legend.key.width = ggplot2::unit(5, "mm"),
                   legend.key.height = ggplot2::unit(10, "mm"),
                   axis.title = ggplot2::element_text(angle = 0,
                                                      face = 2,
                                                      hjust = 1),
                   axis.title.y = ggplot2::element_text(angle = 0,
                                                        face = 2,
                                                        hjust = 1),
                   panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(size = 0.5)) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 4,
                                                                      alpha = 1),
                                                  direction = "vertical",
                                                  title.position = "top",
                                                  title.hjust = 0,
                                                  label.vjust = 0.5,
                                                  label.hjust = 0,
                                                  label.position = "right")) +
    ggplot2::labs(x = "X", y = "Y", shape = "Behaviour", fill = "Environmental\nquality")

  if(plotOptions){
    outPlot <- outPlot +
      ggplot2::geom_point(data = optionData,
                          ggplot2::aes(x = x, y = y),
                          size = 0.35,
                          alpha = 0.05, colour = palette["purp1"])
  }

  # if(!is.null(avoidance)){
  #   outPlot <- outPlot +
  # ## avoidance
  # ggplot2::geom_point(data = avoidance,
  #                     ggplot2::aes(x = x, y = y),
  #                     pch = 16,
  #                     size = 4, colour = "grey90",
  #                     alpha = 1) +
  # ggplot2::geom_point(data = avoidance,
  #                     ggplot2::aes(x = x, y = y),
  #                     pch = "A",
  #                     size = 3, colour = "grey10",
  #                     alpha = 1)
  # }

  return(outPlot)
}

#' @rdname abm_plot
#' @export
abmCyclePlot <- function(simResults,
                         timeLimits = NA, drop_x = FALSE){

  palette <- c("#AD6DED", "#7D26D4", "#E87D13", "#965A1D", "#302010")
  names(palette) <- c("purp1", "purp2", "2", "1", "0")

  # number of steps (mins)
  simSteps <- simResults$inputs$in_timesteps
  # timeLimits needs converting to hours
  if(!is.na(timeLimits[1]) & !is.na(timeLimits[2])){
    timeLimits <- timeLimits/60
  }
  ### CYCLING ###
  behaveProb <- sapply(1:simSteps/60, function(x){
    cycle_draw(x,
               simResults$inputs$in_rest_Cycle_A,
               simResults$inputs$in_rest_Cycle_M,
               simResults$inputs$in_rest_Cycle_PHI,
               simResults$inputs$in_rest_Cycle_TAU)
  })
  behaviourPlotData <- data.frame(
    "i" = 1:simSteps/60,
    "behaveObs" = simResults$locations$behave,
    "behaveRest" = behaveProb
  )

  behAddCycleVec <- vector("list",
                           length = length(simResults$inputs$in_add_Cycle_A))
  for(i in 1:length(simResults$inputs$in_add_Cycle_A)){
    behaveProb <- sapply(1:simSteps/60, function(x){
      cycle_draw(x,
                 simResults$inputs$in_add_Cycle_A[i],
                 simResults$inputs$in_add_Cycle_M[i],
                 simResults$inputs$in_add_Cycle_PHI[i],
                 simResults$inputs$in_add_Cycle_TAU[i])
    })

    behAddCycleVec[[i]] <- data.frame(
      "i" = 1:simSteps/60,
      "behaveObs" = simResults$locations$behave,
      "behaveRest" = behaveProb,
      "cycle" = paste0("cycle_", i)
    )
  }

  plotList <- lapply(behAddCycleVec, function(x){
    plotExpRest <- ggplot2::ggplot(x) +
      ggplot2::geom_path(ggplot2::aes(x = i, y = behaveRest), colour = palette["0"]) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     axis.title = ggplot2::element_text(angle = 0,
                                                        face = 2,
                                                        hjust = 1),
                     axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(angle = 0,
                                                          face = 2,
                                                          hjust = 1),
                     plot.background = ggplot2::element_blank(),
                     # panel.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(size = 0.5),

                     panel.grid.major.y = ggplot2::element_line(linetype = 2,
                                                                size = 0.5,
                                                                colour = "grey75")
      ) +
      ggplot2::labs(y = "Rest prob.\nmodifier", x = "Hour")

    if(!is.na(timeLimits[1]) & !is.na(timeLimits[2])){
      plotExpRest <- plotExpRest +
        ggplot2::scale_x_continuous(limits = c(timeLimits[1], timeLimits[2]))
    }
  })

  # depends on the offest and starting scenario
  daylight <- data.frame(
    "sDay" = seq(6,simSteps/60-18,24),
    "eDay" = seq(18,simSteps/60,24)
  )

  (plotObsBehave <- ggplot2::ggplot(behaviourPlotData) +
      ggplot2::geom_rect(data = daylight,
                         ggplot2::aes(xmin = sDay, xmax = eDay,
                                      ymin = 0.95, ymax = 1.05),
                         fill = "yellow", alpha = 0.25) +
      ggplot2::geom_path(ggplot2::aes(x = i, y = behaveObs), size = 0.5, alpha = 0.5) +
      ggplot2::geom_point(ggplot2::aes(x = i, y = behaveObs, colour = as.factor(behaveObs)), size = 0.5) +
      ggplot2::scale_x_continuous(breaks = seq(0, simSteps/60, 24)) +
      ggplot2::scale_y_continuous(breaks = c(0, 1, 2),
                                  labels = c("0 - Rest", "1 - Explore", "2 - Forage")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     axis.title = ggplot2::element_text(angle = 0,
                                                        face = 2,
                                                        hjust = 1),
                     axis.title.y = ggplot2::element_text(angle = 0,
                                                          face = 2,
                                                          hjust = 1),
                     axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     plot.background = ggplot2::element_blank(),
                     # panel.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(size = 0.5),

                     panel.grid.major.y = ggplot2::element_line(linetype = 2,
                                                                size = 0.5,
                                                                colour = "grey75")
      ) +
      ggplot2::scale_colour_manual(values = palette[3:5]) +
      ggplot2::labs(colour = "Behaviour", y = "Observed\nbehaviour"))

  (plotExpRest <- ggplot2::ggplot(behaviourPlotData) +
      ggplot2::geom_path(ggplot2::aes(x = i, y = behaveRest), colour = palette["0"]) +
      ggplot2::scale_x_continuous(breaks = seq(0, simSteps/60, 24)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none",
                     axis.title = ggplot2::element_text(angle = 0,
                                                        face = 2,
                                                        hjust = 1),
                     axis.text.x = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(angle = 0,
                                                          face = 2,
                                                          hjust = 1),
                     plot.background = ggplot2::element_blank(),
                     # panel.background = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(size = 0.5),

                     panel.grid.major.y = ggplot2::element_line(linetype = 2,
                                                                size = 0.5,
                                                                colour = "grey75")
      ) +
      ggplot2::labs(y = "Rest prob.\nmodifier", x = "Hour"))

  if(!is.na(timeLimits[1]) & !is.na(timeLimits[2])){
    plotObsBehave <- plotObsBehave +
      ggplot2::scale_x_continuous(limits = c(timeLimits[1], timeLimits[2]))
    plotExpRest <- plotExpRest +
      ggplot2::scale_x_continuous(limits = c(timeLimits[1], timeLimits[2]))
  }

  if(!drop_x){
    plotList[[length(plotList)]] <- plotList[[length(plotList)]] +
      ggplot2::theme(axis.text.x = ggplot2::element_text(),
                     axis.title.x = ggplot2::element_text(angle = 0,
                                                          face = 2,
                                                          hjust = 1))
  }

  plotListComp <- vector("list", length = length(plotList)+1)
  plotListComp[[1]] <- plotExpRest
  for(i in 2:(length(plotList)+1)){
    plotListComp[[i]] <- plotList[[i-1]]
  }

  patchwork::wrap_plots(
    plotObsBehave,
    patchwork::wrap_plots(plotListComp, ncol = 1),
    ncol = 1,
    heights = c(0.4, 0.6))
  # +
  #   patchwork::plot_layout(heights = c(1,
  #                                      1/(length(simResults$inputs$in_add_Cycle_A)+1),
  #                                      1-(1/(length(simResults$inputs$in_add_Cycle_A)+1))
  #   ))

  # plotObsBehave / plotExpRest /
  #   do.call(patchwork::wrap_plots, c(plotList, ncol=1)) +
  #   patchwork::plot_layout(heights = c(1,
  #                                      1/(length(simResults$inputs$in_add_Cycle_A)+1),
  #                                      1-(1/(length(simResults$inputs$in_add_Cycle_A)+1))
  #   ))

}

