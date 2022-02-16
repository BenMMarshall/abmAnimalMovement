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
#' @param des_options
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
#' @param behave_Tmat Base transition matrix for 3 behavioural states
#' @param rest_Cycle A vector length 4 for A, M, \eqn{\phi} and \eqn{\tau} to
#'   define the resting/active cycle. Ideal for defining circadian rhythm.
#' @param memShelterMatrix
#' @param forageMatrix
#' @param move_Options
#'
#' @return A list with the following components: 1. The location dataframe
#'   describing all locations the animal occupied, where each row is equal to a
#'   timestep. Columns include: - step, the step as a integer; - x, the x
#'   coordinate of the animal; - y, the y coordinate of the animal; 2. The
#'   options dataframe describing All the options available to the animal over
#'   the entire simulation duration. Columns include: - a - b - c 3. ONWARDS...
#'   TESTING OUTPUTS Columns include: - a - b
#'
#' @details PROVIDE DETAILS THAT WOULD GUIDE INPUTS. The function automatically
#'   generates a list of seeds required for the *sampling_options* based upon
#'   any previous set R seeds set (*e.g.*, using set.seed).
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
abm_simulate <- function(start, steps,
                         des_options,
                         options,

                         shelterLocations,

                         k_step, s_step, mu_angle,
                       k_angle, behave_Tmat, rest_Cycle,
                       memShelterMatrix,
                       forageMatrix,
                       move_Options){
  # split the vector of start location x and y
  startxIN <- start[1]
  startyIN <- start[2]

  # split the dataframe of shelter and forage locations
  shelter_locs_xIN <- shelterLocations[,1]
  shelter_locs_yIN <- shelterLocations[,2]

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
    des_options = des_options,
    options = options,

    shelter_locs_x = shelter_locs_xIN,
    shelter_locs_y = shelter_locs_yIN,

    k_step = k_step,
    s_step = s_step,
    mu_angle = mu_angle,
    k_angle = k_angle,
    b0_Options = behave_Tmat[1,],
    b1_Options = behave_Tmat[2,],
    b2_Options = behave_Tmat[3,],
    rest_Cycle_A = rest_Cycle[1],
    rest_Cycle_M = rest_Cycle[2],
    rest_Cycle_PHI = rest_Cycle[3],
    rest_Cycle_TAU = rest_Cycle[4],
    memShelterMatrix = memShelterMatrix,
    forageMatrix = forageMatrix,
    move_Options = move_Options,
    seeds = sapply(1:steps, function(x){
      get_seed()
    }) # make sure we have enough seeds for each time sample_options is used
  )

  # tidy up all objects parse via the
  # list into dataframes with properly labelled columns

  OUTPUTS <- vector(mode = "list", length = 3)
  names(OUTPUTS)[1] <- "locations"
  names(OUTPUTS)[2] <- "options"
  names(OUTPUTS)[3] <- "others"

  locations <- data.frame(
    step = res$loc_step,
    x = res$loc_x,
    y = res$loc_y,
    behave = res$loc_behave,
    chosen = res$chosen,
    destination_x = res$loc_x_destinations,
    destination_y = res$loc_y_destinations,
    destination_chosen = res$loc_chosen_destinations
  )

  options <- data.frame(
    step = res$oall_step,
    x = res$oall_x,
    y = res$oall_y,
    sl = res$oall_stepLengths)

  OUTPUTS[["locations"]] <- locations
  OUTPUTS[["options"]] <- options
  OUTPUTS[["others"]] <-
    res[!names(res) %in% c("loc_step", "loc_x", "loc_y", "loc_behave", "chosen",
                           "loc_x_destinations", "loc_y_destinations", "loc_chosen_destinations",
                           "oall_step", "oall_x", "oall_y", "oall_stepLengths")]

  return(OUTPUTS)
}

run_abm_simulate <- function(startx, starty, steps,
                             des_options,
                             options,

                             shelter_locs_x,
                             shelter_locs_y,

                             k_step, s_step, mu_angle, k_angle,
                             b0_Options,
                             b1_Options,
                             b2_Options,
                             rest_Cycle_A,
                             rest_Cycle_M,
                             rest_Cycle_PHI,
                             rest_Cycle_TAU,
                             memShelterMatrix,
                             forageMatrix,
                             move_Options,
                             seeds){
  .Call("_abmAnimalMovement_cpp_abm_simulate",
        startx, starty, steps,
        des_options,
        options,

        shelter_locs_x,
        shelter_locs_y,

        k_step, s_step, mu_angle, k_angle,
        b0_Options,
        b1_Options,
        b2_Options,
        rest_Cycle_A,
        rest_Cycle_M,
        rest_Cycle_PHI,
        rest_Cycle_TAU,
        memShelterMatrix,
        forageMatrix,
        move_Options,
        seeds)
}

