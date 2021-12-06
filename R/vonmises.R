#' Draw from a von Mises distribution
#'
#' @name vonmises
#' @description This is the R function counterpart to the C++ function
#'   cpp_vonmises. The C++ function is based on the rvm function from the
#'   CircStats package.
#' @param N The number of values to draw.
#' @param MU The mean direction of the distribution to be draw from.
#' @param KAPPA The concentration or shape parameter. High value will provide a
#'   tighter distribution around MU, whereas a lower value will widen the
#'   distribution.
#' @return Will return a vector the size of N draw from a Von Mises distribution
#'   defined by MU and KAPPA. When MU is zero the limits of the overall
#'   distribution will be between -pi and pi. When considering KAPPA, a higher
#'   value will approximate a greater likely to draw values linked to straight
#'   line movement.
#'
#' @references S-plus original by Ulric Lund and R port by Claudio Agostinelli
#' (2018). CircStats: Circular Statistics, from Topics in Circular Statistics
#' (2001). R package version 0.2-6. https://CRAN.R-project.org/package=CircStats
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
#' @examples
#' vonOut <- vonmises(N = 1000, MU = 0, KAPPA = 0.1)
#' hist(vonOut)
#'
vonmises <- function(N, MU, KAPPA){
  .Call("_abmAnimalMovement_cpp_vonmises",
        N, MU, KAPPA)
}
