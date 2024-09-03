#' Ggnormfluodbf Box Plot
#' @param data data
#' @param mapping mapping
#' @return ggplot object
#' @export
#'
#' @examples
ggnormfluodbf_box <- function(data,
                              mapping){
  mapping$x <- mapping$x
  mapping$y <- mapping$y
  mapping$fill <- mapping$fill

  p <- ggplot2::ggplot(data, mapping) +
    ggplot2::geom_boxplot()
  p
}
