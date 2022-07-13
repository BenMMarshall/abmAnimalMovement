#' Agent based model: Simulate animal movement
#'
#' @description An R function to arrange simulation parameters ready to be fed
#'   to *cpp_abm_simulate*, and also convert the list output of
#'   *cpp_abm_simulate* into a series of objects more easily used by downstream
#'   functions.
#' @param start A numeric vector of length 2, including the x and y coordinates
#'   of the start location.
#' @param timesteps The number of time steps to be simulated.
#' @param des_options The number of dynamically chosen destinations presented to
#'   the animal during the foraging behaviour state.
#' @param options The number of options the animal considers at each step.
#' @param shelterLocations A dataframe including the x and y coordinates for all
#'   shelter sites, that act as points of attraction during the resting
#'   behavioural state.
#' @param shelterSize A value describing the shelter site size. This value
#'   dictates at what point the animal's movements will dramatically drop
#'   simulating (near-)stationary behaviour.
#' @param avoidPoints A dataframe including the x and y coordinates that the
#'   animal will avoid.
#' @param destinationRange A numeric vector of length two where the first value
#'   provides the shape (k) parameter and the second value provides the scale
#'   parameter (\eqn{\theta}) of the gamma distribution guiding the distance of
#'   foraging destinations.
#' @param destinationDirection A numeric vector of length two where the first
#'   value provides the mean (\eqn{\mu}) and the second value provides the
#'   concentration (\eqn{\kappa}) of the Von Mises distribution guiding the
#'   direction of foraging destinations.
#' @param destinationTransformation This parameter and the following three all
#'   apply to the strength/pull/push the animal feels from a destination or
#'   avoidance point. 0 - no transformation applied to the distance to
#'   destination weighting, 1 - distance to destination weighing is
#'   square-rooted, 2 - distance to destination weighting is squared
#' @param destinationModifier A coefficient to be applied to the distance to
#'   destination weighting.
#' @param avoidTransformation Must be 0, 1, or 2: 0 = no transformation applied
#'   to the distance to avoidance points weighting, 1 = distance to avoidance
#'   points weighing is square-rooted, 2 = distance to avoidance points
#'   weighting is squared.
#' @param avoidModifier A coefficient to be applied to the avoidance points
#'   weighting.
#' @param k_step The shape parameters (k) for the gamma distribution describing
#'   step length for each behavioural state. A vector of length 3.
#' @param s_step The scale parameters (\eqn{\theta}) for the gamma distribution
#'   describing step length for each behavioural state. A vector of length 3.
#' @param mu_angle The means (\eqn{\mu}) for the Von Mises distribution used to
#'   draw turn angles for each behavioural state. A vector of length 3.
#' @param k_angle The concentrations (\eqn{\kappa}) for the Von Mises
#'   distribution used to draw turn angles for each behavioural state. A vector
#'   of length 3.
#' @param rescale_step2cell A value that describes the cell size of the
#'   environmental matrices relative to the units of the step lengths. Must be
#'   greater than zero. E.g., step lengths described in metres, and using a
#'   matrix where a cell is 1mx1m the rescale value would be 1. Whereas step
#'   lengths in metres with a cell size of 10mx10m the rescale value would be
#'   10. The step lengths returned are on the scale of the matrix and need to be
#'   back transformed to match the input step length units for comparison.
#'   Default is 1, step and matrix unit are the same.
#' @param behave_Tmat Base transition matrix for 3 behavioural states
#' @param rest_Cycle A vector length 4 for A, M, \eqn{\phi} and \eqn{\tau} to
#'   define the resting/active cycle. Ideal for defining circadian rhythm.
#' @param additional_Cycles A (optional) data.frame 4 columns wide for A, M,
#'   \eqn{\phi} and \eqn{\tau} to define any additional activity cycles. Ideal
#'   for defining patterns that operate alongside circadian rhythm, e.g.,
#'   seasonal shifts.
#' @param shelteringMatrix A matrix describing the sheltering quality of the
#'   landscape. Higher values are the best shelter quality.
#' @param foragingMatrix A matrix describing the foraging quality of the
#'   landscape. Higher values are the best foraging areas.
#' @param movementMatrix A matrix describing the movement ease of the landscape.
#'   Higher values are the easiest to move through.
#'
#' @return A list with the following components: 1. "locations" The dataframe
#'   describing all realised locations the animal occupied, where each row is
#'   equal to a timestep. Columns include: timestep, the timestep as a integer;
#'   x, the x coordinate of the animal; y, the y coordinate of the animal; sl,
#'   the step length between locations used during the simulation; sl_rescale
#'   the rescale factor required to return step lengths back to the input scale;
#'   ta, turning angle between locations in degrees; behave, the behaviroual
#'   mode the animal was in at a given timestep; chosen, the location chosen out
#'   of the number of options available; destination_x and destination_y the
#'   point the animal was attracted to at that time (note exploratory behaviour
#'   is not subject attraction). 2. The "options" dataframe describing All the
#'   options available to the animal over the entire simulation duration, where
#'   each row is equal to an option repeated for each timestep. Columns include:
#'   timestep, the timestep as a integer; oall_x, and oall_y show the x and y
#'   coordinates of all the options available to an animal at a timestep;
#'   oall_steplengths are the step lengths from the current location compared to
#'   all the options. The "inputs" return the arguments used to simulate the
#'   movement, split into a list covering inputs_basic, inputs_destination,
#'   inputs_movement, inputs_cycle, inputs_layerSeed. "others" captures all
#'   other outputs, mainly used internally for debugging and checking.
#'
#' @details The function automatically generates a list of seeds required for
#'   the *sampling_options* based upon any previous set R seeds set (*e.g.*,
#'   using set.seed).
#'
#' @seealso [vonmises()] for further guidance on turn angle distribution
#'   definition, [cycle_draw()] for further guidance on activity cycle
#'   definition. [cpp_abm_simulate()] is the C++ function that is called.
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
abm_simulate <- function(start, timesteps,
                         des_options,
                         options,

                         shelterLocations,
                         shelterSize,
                         avoidPoints,

                         destinationRange,
                         destinationDirection,
                         destinationTransformation,
                         destinationModifier,
                         avoidTransformation,
                         avoidModifier,

                         k_step, s_step, mu_angle, k_angle,
                         rescale_step2cell = 1,

                         behave_Tmat,

                         rest_Cycle,
                         additional_Cycles,

                         shelteringMatrix,
                         foragingMatrix,
                         movementMatrix){


# Verify/check inputs -----------------------------------------------------

  ## start
  if(!is.vector(start) | !length(start) == 2 | !is.numeric(start)){
    stop("Start location (start) is not a numeric vector of length 2")
  }
  ## steps
  if(!timesteps%%1==0 | !length(timesteps)==1){
    stop("Number of time steps (timesteps) should be a single integer")
  }
  ## des_options
  if(!des_options%%1==0 | !length(des_options)==1){
    stop("Number of destination options (des_options) should be a single integer")
  }
  ## options
  if(!options%%1==0 | !length(options)==1){
    stop("Number of movement options (options) should be a single integer")
  }
  ## shelterLocations
  if(!is.data.frame(shelterLocations) | !ncol(shelterLocations) == 2){
    stop("Shelter location input (shelterLocations) is not a data.frame with two columns")
  }
  if(!is.numeric(shelterLocations[,1]) | !is.numeric(shelterLocations[,2])){
    stop("Non-numeric elements in the shelter locations input (shelterLocations)")
  }
  ## shelterSize
  if(!is.numeric(shelterSize) | shelterSize <= 0){
    stop("Shelter site size (shelterSize) should be should be a single positive numeric value")
  }
  ## avoidPoints
  if(!is.data.frame(avoidPoints) | !ncol(avoidPoints) == 2){
    stop("Avoidance location input (avoidPoints) is not a data.frame with two columns")
  }
  if(!is.numeric(avoidPoints[,1]) | !is.numeric(avoidPoints[,2])){
    stop("Non-numeric elements in the avoidance locations input (avoidPoints)")
  }
  ## destinationRange, destinationDirection
  if(!is.vector(destinationRange) | !length(destinationRange) == 2 | !is.numeric(destinationRange)){
    stop("Destination range (destinationRange) is not a numeric vector of length 2")
  }
  if(!is.vector(destinationDirection) | !length(destinationDirection) == 2 | !is.numeric(destinationDirection)){
    stop("Destination direction (destinationDirection) is not a numeric vector of length 2")
  }

  ## destinationTransformation
  ## avoidTransformation
  if(!destinationTransformation %in% c(0,1,2) | !avoidTransformation %in% c(0,1,2)){
    stop("destinationTransformation and avoidTransformation must be 0, 1 or 2 to
         chose the transformation type")
  }
  ## destinationModifier
  ## avoidModifier
  if(!is.numeric(destinationModifier) | !is.numeric(avoidModifier)){
    stop("destinationModifier and avoidModifier should be should be a single numeric value")
  }
  ## k_step
  ## s_step
  ## mu_angle
  ## k_angle
  if(
    !is.vector(k_step) | !length(k_step) == 3 | !is.numeric(k_step)|
    !is.vector(s_step) | !length(s_step) == 3 | !is.numeric(s_step)|
    !is.vector(mu_angle) | !length(mu_angle) == 3 | !is.numeric(mu_angle)|
    !is.vector(k_angle) | !length(k_angle) == 3 | !is.numeric(k_angle)
     ){
    stop("All step and angle distribution variables (k_step, s_step, mu_angle, k_angle)
         need to be numeric vector of length 3")
  }
  ## rescale_step2cell
  if(rescale_step2cell <=0){
    stop("Rescale factor (rescale_step2cell) must be greater than 0")
  }
  ## behave_Tmat
  if(
    !is.matrix(behave_Tmat) | !is.numeric(behave_Tmat) |
    # !all(behave_Tmat <= 1) | !all(behave_Tmat >= 0) |
    !dim(behave_Tmat)[1] == 3 | !dim(behave_Tmat)[2] == 3
  ){
    stop("The behavioural transition matrix (behave_Tmat) should be a 3x3
         numeric matrix")
  }
  ## rest_Cycle
  if(!is.vector(rest_Cycle) | !length(rest_Cycle) == 4 | !is.numeric(rest_Cycle)){
    stop("The rest cycle parameters should be a numeric vector of length 4")
  }
  ## additional_Cycles
  if(!is.null(additional_Cycles)){
    if(!is.matrix(additional_Cycles) | !ncol(additional_Cycles) == 4 |
       !is.numeric(additional_Cycles[,1])| !is.numeric(additional_Cycles[,2]) |
       !is.numeric(additional_Cycles[,3])| !is.numeric(additional_Cycles[,4])
       ){
      stop("The additional cycles data.frame (cycleMat), if needed, requires four numeric columns")
    }}
  ## shelteringMatrix
  ## foragingMatrix
  ## movementMatrix
  if(!is.matrix(shelteringMatrix) |
     !is.matrix(foragingMatrix) |
     !is.matrix(movementMatrix) |
     # !all(shelteringMatrix <= 1) |
     # !all(foragingMatrix <= 1) |
     # !all(movementMatrix <= 1) |
     !all(shelteringMatrix >= -99.9) |
     !all(foragingMatrix >= -99.9) |
     !all(movementMatrix >= -99.9)
  ){
    stop("All the landscape layers (shelterMatrix, forageMatrix, moveMatrix)
       should be numeric matricies, with values between -99.9 and 1")
  }
  if(any(shelterLocations[,1] > nrow(shelteringMatrix)) |
     any(shelterLocations[,2] > ncol(shelteringMatrix)) |
     any(shelterLocations[,1] < 0) |
     any(shelterLocations[,2] < 0)){
    stop("Shelter locations (shelterLocations) must be contained within environmental rasters
         (shelteringMatrix, foragingMatrix, movementMatrix)")
  }
  if(any(start[1] > nrow(shelteringMatrix)) |
     any(start[2] > ncol(shelteringMatrix)) |
     any(start[1] < 0) |
     any(start[2] < 0)){
    stop("Start location (start) must be contained within environmental rasters
         (shelteringMatrix, foragingMatrix, movementMatrix)")
  }
  if(
    !all(all(dim(shelteringMatrix) == dim(movementMatrix)),
        all(dim(foragingMatrix) == dim(shelteringMatrix)),
        all(dim(movementMatrix) == dim(foragingMatrix)))
  ){
    stop("All environmental layers require the same dimensions (shelteringMatrix,
         foragingMatrix, movementMatrix)")
  }


# Rearrange inputs for C++ ------------------------------------------------

  # split the vector of start location x and y
  startxIN <- start[1]
  startyIN <- start[2]

  # split the data.frame of shelter locations
  shelter_locs_xIN <- shelterLocations[,1]
  shelter_locs_yIN <- shelterLocations[,2]

  # split the data.frame of avoidance points
  avoidPoints_xIN <- avoidPoints[,1]
  avoidPoints_yIN <- avoidPoints[,2]

  # how many additional cycles have been provided, and get that value ready for
  # C++
  if(is.null(additional_Cycles)){
    nAdditionalCycles <- 0
  } else {
    nAdditionalCycles <- nrow(additional_Cycles)
  }

  # input all into the Cpp function
  res <- run_abm_simulate(
    startx = startxIN,
    starty = startyIN,
    timesteps = timesteps,
    ndes = des_options,
    nopt = options,

    shelter_locs_x = shelter_locs_xIN,
    shelter_locs_y = shelter_locs_yIN,
    sSiteSize = shelterSize,
    avoidPoints_x = avoidPoints_xIN,
    avoidPoints_y = avoidPoints_yIN,

    k_desRange = destinationRange[1],
    s_desRange = destinationRange[2],
    mu_desDir = destinationDirection[1],
    k_desDir = destinationDirection[2],
    destinationTrans = destinationTransformation,
    destinationMod = destinationModifier,
    avoidTrans = avoidTransformation,
    avoidMod = avoidModifier,

    k_step = k_step,
    s_step = s_step,
    mu_angle = mu_angle,
    k_angle = k_angle,
    rescale = rescale_step2cell,
    b0_Options = behave_Tmat[1,],
    b1_Options = behave_Tmat[2,],
    b2_Options = behave_Tmat[3,],

    rest_Cycle_A = rest_Cycle[1],
    rest_Cycle_M = rest_Cycle[2],
    rest_Cycle_PHI = rest_Cycle[3],
    rest_Cycle_TAU = rest_Cycle[4],
    addCycles = nAdditionalCycles,
    add_Cycle_A = additional_Cycles[,1],
    add_Cycle_M = additional_Cycles[,2],
    add_Cycle_PHI = additional_Cycles[,3],
    add_Cycle_TAU = additional_Cycles[,4],

    shelterMatrix = shelteringMatrix,
    forageMatrix = foragingMatrix,
    moveMatrix = movementMatrix
  )

  # tidy up all objects parse via the
  # list into dataframes with properly labelled columns

  OUTPUTS <- vector(mode = "list", length = 4)
  names(OUTPUTS)[1] <- "locations"
  names(OUTPUTS)[2] <- "options"
  names(OUTPUTS)[3] <- "others"
  names(OUTPUTS)[4] <- "inputs"

  locations <- data.frame(
    timestep = res$loc_step,
    x = res$loc_x,
    y = res$loc_y,
    sl = res$loc_sl,
    sl_rescale = res$inputs_list$inputs_movement$in_rescale,
    ta = res$loc_ta,
    behave = res$loc_behave,
    chosen = res$loc_chosen,
    destination_x = res$loc_x_destinations,
    destination_y = res$loc_y_destinations,
    destination_chosen = res$loc_chosen_destinations
  )

  options <- data.frame(
    timestep = res$oall_step,
    x = res$oall_x,
    y = res$oall_y,
    sl = res$oall_stepLengths)

  # inputs <- list(
  #   basic = res$inputs_list$inputs_basic,
  #   destination = res$inputs_list$inputs_destination,
  #   movement = res$inputs_list$inputs_movement,
  #   cycle = res$inputs_list$inputs_cycle,
  #   layerSeed = res$inputs_list$inputs_layerSeed
  # )
  inputs <- c(
    res$inputs_list$inputs_basic,
    res$inputs_list$inputs_destination,
    res$inputs_list$inputs_movement,
    res$inputs_list$inputs_cycle,
    res$inputs_list$inputs_layerSeed
  )

  OUTPUTS[["locations"]] <- locations
  OUTPUTS[["options"]] <- options
  OUTPUTS[["others"]] <-
    res[!grepl("loc|oall|input", names(res))]
  OUTPUTS[["inputs"]] <- inputs

  return(OUTPUTS)
}

run_abm_simulate <- function(startx, starty,
                             timesteps,
                             ndes,
                             nopt,

                             shelter_locs_x,
                             shelter_locs_y,
                             sSiteSize,
                             avoidPoints_x,
                             avoidPoints_y,

                             k_desRange,
                             s_desRange,
                             mu_desDir,
                             k_desDir,
                             destinationTrans,
                             destinationMod,
                             avoidTrans,
                             avoidMod,

                             k_step, s_step, mu_angle, k_angle,
                             rescale,
                             b0_Options,
                             b1_Options,
                             b2_Options,

                             rest_Cycle_A,
                             rest_Cycle_M,
                             rest_Cycle_PHI,
                             rest_Cycle_TAU,
                             addCycles,
                             add_Cycle_A,
                             add_Cycle_M,
                             add_Cycle_PHI,
                             add_Cycle_TAU,

                             shelterMatrix,
                             forageMatrix,
                             moveMatrix){
  .Call("_abmAnimalMovement_cpp_abm_simulate",
        startx, starty,
        timesteps,
        ndes,
        nopt,

        shelter_locs_x,
        shelter_locs_y,
        sSiteSize,
        avoidPoints_x,
        avoidPoints_y,

        k_desRange,
        s_desRange,
        mu_desDir,
        k_desDir,
        destinationTrans,
        destinationMod,
        avoidTrans,
        avoidMod,

        k_step, s_step, mu_angle, k_angle,
        rescale,
        b0_Options,
        b1_Options,
        b2_Options,

        rest_Cycle_A,
        rest_Cycle_M,
        rest_Cycle_PHI,
        rest_Cycle_TAU,
        addCycles,
        add_Cycle_A,
        add_Cycle_M,
        add_Cycle_PHI,
        add_Cycle_TAU,

        shelterMatrix,
        forageMatrix,
        moveMatrix)
}

