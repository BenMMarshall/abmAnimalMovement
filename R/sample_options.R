#' Sample options
#'
#' @param W W
#' @param SEED
#' @return chosen
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_options <- function(W, SEED){
  .Call("_abmAnimalMovement_sample_options",
        W, SEED)
}
