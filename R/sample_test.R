#' Sample test
#'
#' @param vect weights
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_test <- function(weights){
  .Call("_abmAnimalMovement_sample_test",
        weights)
}
