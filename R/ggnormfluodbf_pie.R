#' Ggnormfluodbf Pie Charts
#' @param data data
#' @param mapping mapping
#' @param mapping mapping
#' @param label label
#' @param fill_colors fill_colors
#' @param fill_brewer_palette fill_brewer_palette
#' @param ... dots
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' ggnormfluodbf_pie(df1, ggplot2::aes(x=smoker))
#' ggnormfluodbf_pie(df1, ggplot2::aes(x=sex))
#' ggnormfluodbf_pie(df1, ggplot2::aes(x=sex), fill_colors = c('red','green','blue'))
#' ggnormfluodbf_pie(df1, ggplot2::aes(x=sex), fill_brewer_palette = 'Greens')
#' }
ggnormfluodbf_pie <- function(data,
                              mapping,
                              label = TRUE,
                              fill_colors = NULL,
                              fill_brewer_palette = NULL,
                              ...){

  x <- rlang::as_name(mapping$x)

  data <- data %>%
    dplyr::group_by(!!rlang::sym(x)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    dplyr::mutate(
      prop = round(((count / sum(count)) * 100), 2)
    )

  mapping$y <- ""
  mapping$x <- mapping$x
  mapping$fill <- mapping$x

  if (is.null(label) || isTRUE(label)){
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_bar(stat = 'identity', width = 1) +
      ggplot2::coord_polar('x', start = 0) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(prop,"%")),
        position = ggplot2::position_stack(vjust=0.5, reverse = T)
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL)
  }
  else {
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_bar(stat = 'identity', width = 1) +
      ggplot2::coord_polar('x', start = 0) +
      ggplot2::theme_void()
  }

  if (!is.null(fill_colors) && is.null(fill_brewer_palette)){
    p <- p + ggplot2::scale_fill_manual(values = fill_colors)
  }

  if (!is.null(fill_brewer_palette) && is.null(fill_colors)){
    p <- p + ggplot2::scale_fill_brewer(palette = fill_brewer_palette)
  }
  p
}
