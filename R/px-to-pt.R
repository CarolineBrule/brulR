#' Pixel to Point Conversion
#'
#' Convert a size in pixels to points, using the conversion factor where 1 pt = 0.75 px.
#' This function is useful for adjusting graphical parameters in ggplot2 or other graphics packages.
#'
#' @param px The size in pixels (numeric).
#' @return The equivalent size in points (numeric).
#' @examples 
#' pt_size <- px_to_pt(48)
#' pt_sizes <- px_to_pt(c(32, 48, 72))
#' @export
px_to_pt <- function(px) {
  pt <- px / 0.75
  return(pt)
}
