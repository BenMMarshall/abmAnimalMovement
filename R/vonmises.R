#' Draw from a von Mises distribution
#'
#' @name vonmises
#' @description The R function counterpart to the C++ function *cpp_vonmises*.
#'   The C++ function is based on the *rvm* function from the [CircStats
#'   package](https://cran.r-project.org/web/packages/CircStats/index.html).
#' @param N The number of values to draw.
#' @param MU The mean (\eqn{\mu}) direction of the distribution to be draw from.
#' @param KAPPA The concentration or shape parameter (\eqn{\kappa}). High value
#'   will provide a tighter distribution around MU, whereas a lower value will
#'   widen the distribution.
#' @return Will return a vector the size of N draw from a Von Mises distribution
#'   defined by \eqn{\mu} and \eqn{\kappa}.
#' @details When \eqn{\mu} is zero the limits of the overall distribution will
#'   be between -\eqn{\pi} and \eqn{\pi}. When considering \eqn{\kappa}, a
#'   higher value will approximate a greater likely to draw values linked to
#'   straight line movement.
#'
#' @references S-plus original by Ulric Lund and R port by Claudio Agostinelli
#'   (2018). CircStats: Circular Statistics, from Topics in Circular Statistics
#'   (2001). R package version 0.2-6.
#'   https://CRAN.R-project.org/package=CircStats
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
#' @examples
#' vonOut <- vonmises(N = 1000, MU = 0, KAPPA = 0.1)
#' hist(vonOut)
#'
vonmises <- function(N, MU, KAPPA){

  if(!N%%1==0 | !length(N)==1){
    stop("Number of draws (N) should be a single integer")
  }

  if(!is.numeric(MU) | !is.numeric(KAPPA)
  ){
    stop("MU and KAPPA inputs must be numeric")
  }

  .Call("_abmAnimalMovement_cpp_vonmises",
        N, MU, KAPPA)
}
