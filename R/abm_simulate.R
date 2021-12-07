#' Agent based model: Simulate animal movement
#'
#' @description An R function to arrange simulation parameters ready to be fed
#'   to *cpp_abm_simulate*, and also convert the list output of
#'   *cpp_abm_simulate* into a series of objects more easily used by downstream
#'   functions.
#' @param start A numeric vector of length 2, including the x and y coordinates
#'   of the start location.
#' @param steps The number of time steps to be simulated, where each step is
#'   equal to ------ ----.
#' @param options The number of options the animal considers at each step.
#' @param k_step The shape parameters (k) for the gamma distribution describing
#'   step length for each behavioural state. A vector of length 3.
#' @param s_step The scale parameters (\eqn{\theta}) for the gamma distribution
#'   describing step length for each behavioural state. A vector of length 3.
#' @param mu_angle The means (\eqn{\mu}) for the von Mises distribution used to
#'   draw turn angles for each behavioural state. A vector of length 3.
#' @param k_angle The concentrations (\eqn{\kappa}) for the von Mises
#'   distribution used to draw turn angles for each behavioural state. A vector
#'   of length 3.
#' @param envMat1 TESTING ENVIRONMENTAL LAYER, A matrix.
#'
#' @return A list with the following components:
#' 1. The location dataframe describing all locations the animal occupied, where each row is equal to a
#'   timestep. Columns include:
#'    - step, the step as a integer;
#'    - x, the x coordinate of the animal;
#'    - y, the y coordinate of the animal;
#' 2. The options dataframe describing All the options available to the animal over the entire
#'   simulation duration. Columns include:
#'    - a
#'    - b
#'    - c
#' 3. ONWARDS... TESTING OUTPUTS Columns include:
#'    - a
#'    - b
#'
#' @details PROVIDE DETAILS THAT WOULD GUIDE INPUTS. The function automatically
#'   generates a list of seeds required for the *sampling_options* based upon
#'   any previous set R seeds set (*e.g.*, using set.seed).
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
abm_simulate <- function(start, steps, options, k_step, s_step, mu_angle,
                       k_angle, envMat1){
  # split the vector of start location x and y
  startxIN <- start[1]
  startyIN <- start[2]

  # A function that gets a seed so the sampling function is fed something fresh
  # each turn. More details in sample_options documentation
  get_seed <- function() {
    sample.int(.Machine$integer.max, 1)
  }

  # input all into the Cpp function
  res <- run_abm_simulate(
    startx = startxIN,
    starty = startyIN,
    steps = steps,
    options = options,
    k_step = k_step,
    s_step = s_step,
    mu_angle = mu_angle,
    k_angle = k_angle,
    envMat1 = envMat1,
    seeds = rep(get_seed(), steps) # make sure we have enough seeds for each time sample_options is used
  )

  ## TO DO ##

  # add in a translating function to tidy up all objects parse via the
  # list into dataframes with properly labelled columns


  return(res)
}

run_abm_simulate <- function(startx, starty, steps, options, k_step, s_step, mu_angle, k_angle,
                               envMat1, seeds){
  .Call("_abmAnimalMovement_cpp_abm_simulate",
        startx, starty, steps, options, k_step, s_step, mu_angle, k_angle,
        envMat1, seeds)
}

