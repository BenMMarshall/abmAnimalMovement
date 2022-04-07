#' Sample from weighted options
#'
#' @name sample_options
#' @description The R function to call the C++ *cpp_sample_options* function.
#' @param W A vector of weights.
#' @param SEED The seed to be used for the sampling.
#' @details If a weight of less than zero is provided the weight is set to zero.
#'   A new seed must be passed each time the function is ran (for different
#'   answers). The C++ function internally set a random seed with the random C++
#'   library (std::mt19937). Example code below provides a shorthand call to
#'   retrieve consistent seeds based upon an initial use of *set.seed()*.
#' @return An integer corresponding to the chosen weight in the initially
#'   provided vector. **NOTE: indexing begins at 0 matching C++ convention**.
#'
#' @examples
#' set.seed(2021)
#' get_seed <- function() {
#' sample.int(.Machine$integer.max, 1)
#' }
#'
#' sampleOut <- NULL
#' for(i in 1:10000){
#'   sampleOut[i] <- sample_options(c(0.25, 0.15, 0.5, 0.05, 0.05), get_seed())
#' }
#' hist(sampleOut)
#' table(sampleOut) / 10000
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_options <- function(W, SEED){

  if(!length(W)>0){
    stop("Weights (W) must be a vector of length > 0")
  }

  if(!is.numeric(SEED)
  ){
    stop("SEED must be numeric")
  }

  .Call("_abmAnimalMovement_cpp_sample_options",
        W, SEED)
}
