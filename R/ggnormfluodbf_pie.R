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
#'
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
      prop = round(((count / sum(count)) * 100), 2),
      ypos = cumsum(prop) - 0.5 * prop,
      angle = 360 * ypos / sum(count) - 90
    ) %>%
    dplyr::mutate(!!rlang::sym(x) := forcats::fct_rev(factor(!!rlang::sym(x))))

  mapping <- ggplot2::aes( x = "", y = prop, fill = !!rlang::sym(x))
  set_invisible_variable('title',"") #assign("title", "", envir = parent.frame())

  if (is.null(label) || isTRUE(label)){
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_bar(stat = 'identity', width = 1) +
      ggplot2::coord_polar(theta = 'y', start = 0) +
      ggplot2::theme_void() +
      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggrepel::geom_label_repel(
        ggplot2::aes(y = ypos, label = paste0(prop,"%")),
        size = 4,  # Increase text size for clarity
        nudge_x = 1.5,  # Push labels further out
        nudge_y = 0.3,  # Slight nudge in y direction
        box.padding = 0.5,  # Increase padding around text box
        point.padding = 0.5,  # Adjust point padding to prevent overlap
        direction = "both",
        segment.size = 0.7,  # Fine-tune the segment line size
        segment.color = "grey50",  # Change segment color to grey
        segment.linetype = "dashed",  # Dashed line for segments
        max.overlaps = Inf,
        show.legend = FALSE,
        min.segment.length = 0,
        force = 2,  # Increase force to prevent overlap
        label.size = 0.25,  # Thin border for the label
        label.padding = ggplot2::unit(0.25, "lines"),  # Padding inside the label
        label.r = ggplot2::unit(0.15, "lines"),  # Rounded corners for the label
        fill = "green",  # Background color of the label
        color = "black"  # Text color
      )
  }
  else {
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_bar(stat = 'identity', width = 1) +
      ggplot2::coord_polar('y', start = 0) +
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
