#' Sample options
#'
#' @param W W
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_options <- function(W){
  .Call("_abmAnimalMovement_sample_options",
        W)
}
