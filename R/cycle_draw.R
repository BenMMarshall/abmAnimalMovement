#' Draw from cycle
#'
#' @name cycle_draw
#' @description A function to define and draw from a cyclical distribution used
#'   for daily of seasonal shifts.
#' @param TIME A point in time during the cycle to draw a value from.
#' @param A A value defining the amplitude of the cycle, calculated as the
#'   difference from M (i.e., half of the overall variation in the cycle from
#'   top to bottom).
#' @param M A value defining the cycle offset from 0 (Midline Statistic Of
#'   Rhythm, a rhythm-adjusted mean).
#' @param PHI A values defining the offset of the cycle (\eqn{\phi}; i.e.,
#'   acrophase). Must be kept proportional to \eqn{\tau} to avoid cycle drift.
#' @param TAU A value defining the cycle frequency (\eqn{\tau}; i.e., period),
#'   where \eqn{\tau} is the difference between peaks.
#' @return A value between M+A and M-A draw from the defined cycle.
#' @details The time intervals will match the scale the cycle was defined as.
#'   For example, a \eqn{\tau} of 24 defines a cycle repeating every 24
#'   instances, so a draw using TIME == 1 will extract a value at hour 1. For a
#'   draw two and half hours in, TIME == 2.5. \eqn{\phi} must be proportional to tau to
#'   prevent the cycle from drifting. If kept proportional the cycle will remain
#'   constrained to the \eqn{\tau} frequency defined. Implementation following
#'   Cornelissen (2014).
#' @references Cornelissen, G. (2014). Cosinor-based rhythmometry. Theoretical
#'   Biology and Medical Modelling. 11(16)
#'   https://doi.org/10.1186/1742-4682-11-16
#'
#' @examples
#'
#' # an example of a cycle approximating nocturnal activity with a draw at hour 12.
#' cycle_draw(TIME = 12,
#' A = 1,
#' M = 0,
#' PHI = 4 / 24,
#' TAU = 24)
#'
#' # an example of a cycle approximating crepuscular activity with a draw at hour 12.
#' cycle_draw(TIME = 12,
#' A = 1,
#' M = 0,
#' PHI = 24 / 12,
#' TAU = 12)
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
cycle_draw <- function(TIME, A,  M, PHI, TAU){
  .Call("_abmAnimalMovement_cpp_cycle_draw",
        TIME, A,  M, PHI, TAU)
}
