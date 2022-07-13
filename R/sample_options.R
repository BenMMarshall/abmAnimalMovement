#' Sample from weighted options
#'
#' @name sample_options
#' @description The R function to call the C++ *cpp_sample_options* function.
#' @param W A vector of weights.
#' @details The function will pre-normalise provided values before passing them
#'   to the Rcpp sample function and returning the index randomly selected.
#' @return An integer corresponding to the chosen weight in the initially
#'   provided vector. **NOTE: indexing begins at 0 matching C++ convention**.
#'
#' @examples
#' set.seed(2022)
#'
#' sampleOut <- NULL
#' for(i in 1:10000){
#'   sampleOut[i] <- sample_options(c(0.25, 0.15, 0.5, 0.05, 0.05))
#' }
#' hist(sampleOut)
#' table(sampleOut) / 10000
#'
#' @useDynLib abmAnimalMovement
#' @export
#'
sample_options <- function(W){

  if(!length(W)>0){
    stop("Weights (W) must be a vector of length > 0")
  }

  .Call("_abmAnimalMovement_cpp_sample_options",
        W)
}
