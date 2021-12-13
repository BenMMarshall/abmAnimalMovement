#' Draw from cycle
#'
#' @name cycle_draw
#' @description A function to define and draw from a cyclical distribution used
#'   for daily of seasonal shifts.
#' @param TIME A vector of time intervals to draw using.
#' @param A A value defining the amplitude of the cycle, calculated as the
#'   difference from M.
#' @param M A value defining the cycle offset from 0.
#' @param THETA A values defining the offset of the cycle. Must be kept
#'   proportional to TAU to avoid cycle drift.
#' @param TAU A value defining the cycle frequency, where TAU is the difference
#'   between peaks.
#' @return Vector of drawn values matching the length and order of the TIME
#'   intervals supplied.
#' @details The time intervals will match the scale the cycle was defined as.
#'   For example, a TAU of 24 defines a cycle repeating every 24 instances, so a
#'   draw using TIME == 1 will extract a value at hour 1. For a draw 2 and half
#'   hours in TIME == 2.5. Theta must be proportional to tau to prevent the
#'   cycle from drifting. If kept proportional the cycle will remain constrained
#'   to the Tau frequency defined.
#'
#' @examples
#'
#' # an example of a cycle approximating nocturnal activity
#' cycle_draw(TIME = t_seq,
#' A = 1,
#' M = 0,
#' THETA = 4 / 24,
#' TAU = 24)
#'
#' # an example of a cycle approximating crepuscular activity
#' cycle_draw(TIME = t_seq,
#' A = 1,
#' M = 0,
#' THETA = 24 / 12,
#' TAU = 12)
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
cycle_draw <- function(TIME, A,  M, THETA, TAU){
  .Call("_abmAnimalMovement_cpp_cycle_draw",
        TIME, A,  M, THETA, TAU)
}
