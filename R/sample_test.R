#' Sample test
#'
#' @param vect vector
#' @param probs probs
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_test <- function(vect, probs){
  .Call("_abmAnimalMovement_sample_test",
        vect, probs)
}
