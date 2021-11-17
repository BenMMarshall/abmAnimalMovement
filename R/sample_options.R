#' Sample options
#'
#' @param vect weights
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_options <- function(weights){
  .Call("_abmAnimalMovement_sample_options",
        weights)
}
