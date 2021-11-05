#' Sample test
#'
#' @param vect vector
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_test <- function(vect){
  .Call("_abmAnimalMovement_sample_test",
        vect)
}
