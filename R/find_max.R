#' Find max
#'
#' @param vect
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
find_max <- function(vect){
  .Call("_abmAnimalMovement_find_max",
        vect)
}
