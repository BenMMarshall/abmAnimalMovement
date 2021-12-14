#' Find max
#'
#' @name find_max
#' @param vect a
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
find_max <- function(vect){
  .Call("_abmAnimalMovement_find_max",
        vect)
}
