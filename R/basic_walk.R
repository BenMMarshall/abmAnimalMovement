#' R wrap for basic walk
#'
#' A function to run Cpp basic random walk where each step is chosen from a number of possible new locations.
#' @param start The x and y coords of the starting location
#' @param steps The number of steps to be simulated
#' @param options The number of options to be considered at each step
#' @param k_step Parameter describing step length for each behavioural state
#' @param s_step Parameter describing step length for each behavioural state
#' @param meanang Parameter describing angle for each behavioural state
#' @param sdang Parameter describing angle variation for each behavioural state
#' @param behave_Tmat Base transisition matrix for 3 behavioural states
#' @param envMat1 Environmental matrix 1
#' @return Matrix of locations chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
basic_walk <- function(start, steps, options, k_step, s_step, mu_angle,
                       k_angle, behave_Tmat, envMat1){
  # split the vector of start location x and y
  startxIN <- start[1]
  startyIN <- start[2]

  # https://www.r-bloggers.com/2018/09/using-rs-set-seed-to-set-seeds-for-use-in-c-c-including-rcpp/
  # a function that gets a seed so the sampling function is fed something fresh each turn
  get_seed <- function() {
    sample.int(.Machine$integer.max, 1)
  }

  # input all into the Cpp function
  res <- cpp_run_basic_walk(
    startx = startxIN,
    starty = startyIN,
    steps = steps,
    options = options,
    k_step = k_step,
    s_step = s_step,
    mu_angle = mu_angle,
    k_angle = k_angle,
    b0_Options = behave_Tmat[1,],
    b1_Options = behave_Tmat[2,],
    b2_Options = behave_Tmat[3,],
    envMat1 = envMat1,
    seeds = sapply(1:steps, function(x){
      get_seed()
    }) # make sure we have enough seeds for each time sample_options is used
  )

  ## TO DO ##

  # add in a translating function to tidy up all objects parse via the
  # list into dataframes with properly labelled columns


  return(res)
}

cpp_run_basic_walk <- function(startx, starty, steps, options, k_step, s_step, mu_angle, k_angle,
                               b0_Options,
                               b1_Options,
                               b2_Options,
                               envMat1, seeds){
  .Call("_abmAnimalMovement_walk_options_xy",
        startx, starty, steps, options, k_step, s_step, mu_angle, k_angle,
        b0_Options,
        b1_Options,
        b2_Options,
        envMat1, seeds)
}

