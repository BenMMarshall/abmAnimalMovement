#' Plot simulation outputs
#'
#' @param simResults The resulting object created by [abm_simuate()].
#' @param shelterLocations The 2 column shelter location data.frame fed into the
#'   [abm_simuate()].
#' @param avoidance The 2 column avoidance location data.frame fed into the
#'   [abm_simuate()] (optional).
#' @param timeLimits The start timestep of data to be plotted, can be helpful for
#'   plotting a more manageable section of large simulation (optional).
#' @name abmMapPlot
#' @return a plot
#'
#' @seealso [abm_simuate()] for the required input.
#'
#' @useDynLib abmAnimalMovement

#' @rdname abm_plot
#' @export
abmMapPlot <- function(simResults,
                       shelterLocations,
                       avoidance = NULL,
                       timeLimits = NA
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


  outPlot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = optionData,
                        ggplot2::aes(x = x, y = y),
                        size = 0.35,
                        alpha = 0.05, colour = palette["purp1"]) +
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
                        size = 2, colour = palette["2"],
                        alpha = 0.85) +
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

  if(!is.null(avoidance)){
    outPlot <- outPlot +
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
                          alpha = 1)
  }

  return(outPlot)
}
