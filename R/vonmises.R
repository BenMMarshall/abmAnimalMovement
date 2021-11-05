#' Vonmises
#'
#' @name vonmises
#' @param N number to draw
#' @param kappa
#' @param mu
#' @return Vector pulled from vonmises
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
vonmises <- function(N, kappa, mu, x){
  .Call("_abmAnimalMovement_vonmises",
        N, kappa, mu, x)
}
