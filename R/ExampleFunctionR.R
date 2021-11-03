#' Title
#'
#' @param colour The colour to make shades of
#' @param n The number of shades to make
#' @param lighter Whether to make lighter (TRUE) or darker (FALSE) shades
#'
#' @return A vector of n colour hex codes
#' @export
#'
#' @examples
#' # Five lighter shades
#' make_shades("goldenrod", 5)
#' # Five darker shades
#' make_shades("goldenrod", 5, lighter = FALSE)
#'
make_shades <- function(colour, n, lighter = TRUE) {
  # Convert the colour to RGB
  colour_rgb <- grDevices::col2rgb(colour)[, 1]

  # Decide if we are heading towards white or black
  if (lighter) {
    end <- 255
  } else {
    end <- 0
  }

  # Calculate the red, green and blue for the shades
  # we calculate one extra point to avoid pure white/black
  red <- seq(colour_rgb[1], end, length.out = n + 1)[1:n]
  green <- seq(colour_rgb[2], end, length.out = n + 1)[1:n]
  blue <- seq(colour_rgb[3], end, length.out = n + 1)[1:n]

  # Convert the RGB values to hex codes
  shades <- grDevices::rgb(red, green, blue, maxColorValue = 255)

  return(shades)
}
