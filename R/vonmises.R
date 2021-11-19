#' Vonmises
#'
#' @name vonmises
#' @param N number to draw
#' @param MU
#' @param KAPPA
#' @return Vector pulled from vonmises
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
vonmises <- function(N, MU, KAPPA){
  .Call("_abmAnimalMovement_vonmises",
        N, MU, KAPPA)
}
