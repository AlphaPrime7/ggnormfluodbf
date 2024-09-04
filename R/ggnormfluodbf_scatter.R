#' Single Well Scatter Plot
#' @param data data
#' @param mapping aesthetic mapping container
#' @param colour colour
#' @param title title
#' @param xlab x label
#' @param ylab y label
#' @return ggplot object
#' @export
#' @examples \dontrun{
#' ggnormfluodbf_scatter(dat_1,mapping = aes(x=Cycle_Number, y=A1), colour = 'green')
#' }
ggnormfluodbf_scatter <- function(data,
                                  mapping,
                                  title = 'Normfluodbf scatter plot',
                                  xlab = "Cycle Number",
                                  ylab = "Fluorescence"){

  if (inherits(mapping, 'uneval')){
    rlang::warn(message = 'mapping must be an aesthetic mapping',
                use_cli_format = T)
  }

  if (is.null(mapping$x)) {
    rlang::warn(message = 'The x variable must be provided',
                use_cli_format = T)
  }

  if (is.null(mapping$y)) {
    rlang::warn(message = 'The y variable must be provided',
                use_cli_format = T)
  }

  p <- ggplot2::ggplot(data,
                       mapping) +
    ggplot2::geom_point() +
    ggplot2::labs(title = title,
         x = xlab,
         y = ylab) +
    ggplot2::theme_minimal()
  p
}
