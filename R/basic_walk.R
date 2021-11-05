#' R wrap for basic walk
#'
#' A function to run Cpp basic random walk where each step is chosen from a number of possible new locations.
#' @param start The x and y coords of the starting location
#' @param steps The number of steps to be simulated
#' @param options The number of options to be considered at each step
#' @param normmean Parameter describing step length
#' @param normsd Parameter describing step angle
#' @param meanang Parameter describing angle
#' @param sdang Parameter describing angle variation
#' @param envMat1 Environmental matrix 1
#' @return Matrix of locations chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
basic_walk <- function(start, steps, options, normmean, normsd, meanang,
                           sdang, envMat1){
  # split the vector of start location x and y
  startxIN <- start[1]
  startyIN <- start[2]

  # input all into the Cpp function
  res <- cpp_run_basic_walk(
    startx = startxIN,
    starty = startyIN,
    steps = steps,
    options = options,
    normmean = normmean,
    normsd = normsd,
    meanang = meanang,
    sdang = sdang,
    envMat1 = envMat1
  )

  ## TO DO ##

  # add in a translating function to tidy up all objects parse via the
  # list into dataframes with properly labelled columns


  return(res)
}

cpp_run_basic_walk <- function(startx, starty, steps, options, normmean, normsd, meanang, sdang,
                               envMat1){
  .Call("_abmAnimalMovement_walk_options_xy",
        startx, starty, steps, options, normmean, normsd, meanang, sdang,
        envMat1)
}

