## normfluodbf - R package for plotting normfluodbf data and other generic functions for visualization immersion
## Copyright (C) 2024 Tingwei Adeck

#' Bar plots generic
#' @name barplots
#' @param data data
#' @param mapping aesthetic vector
#' @param stat stat (prop or count)
#' @param x x
#' @param y y
#' @param size text size
#' @param v_adj vertical adjustment
#' @param h_adj horizontal adjustment
#' @param fill_colors fill colors
#' @param ... dots
#' @return ggplot object
#' @export
#' @examples \dontrun{
#' ggnormfluodbf_vbar(tips, mapping = ggplot2::aes(x = smoker, y = sex), size = 8, fill_colors = c('red','blue'))
#' ggnormfluodbf_vbar(tips, mapping = ggplot2::aes(x = smoker, y = sex), size = 8, stat = 'prop', fill_colors = c('red','blue'))
#' ggnormfluodbf_vbar(tips, mapping = ggplot2::aes(x = smoker, y = sex), size = 8, stat = 'count', fill_colors = c('red','blue'))
#' ggnormfluodbf_vbar(tips, mapping = ggplot2::aes(x = smoker, y = sex), size = 8, stat = 'prop_alt', fill_colors = c('red','blue'))
#' ggnormfluodbf_hbar(tips, mapping = ggplot2::aes(x = sex, y = smoker), size = 8, fill_colors = c('maroon','gray'))
#' ggnormfluodbf_hbar(df1, mapping = ggplot2::aes(x = sex, y = smoker), size = 8, fill_colors = c('maroon','blue','green'))
#' ggnormfluodbf_hbar(df2, mapping = ggplot2::aes(x = sex, y = smoker), size = 5, fill_colors = c('red','blue','maroon','green'),h_adj = 0.4))
#' ggnormfluodbf_hbar(df2, mapping = ggplot2::aes(x = smoker, y = sex), size = 5, fill_colors = c('red','blue','maroon','green'),h_adj = 0.4)}

#' @rdname barplots
#' @return ggplot object
#' @export
ggnormfluodbf_vbar <- function(
    data,
    mapping,
    stat = c('prop', 'prop_alt', 'count'),
    size = NULL,
    v_adj = 0.5,
    fill_colors = NULL,
    bar_args =NULL,
    count_bar_args = NULL,
    ...) {

  if (is.null(mapping$x) || is.null(mapping$y))
    rlang::warn("'x' and 'y' arguments are required", use_cli_format = TRUE)

  x_var <- rlang::as_name(mapping$x)
  y_var <- rlang::as_name(mapping$y)

  if('prop' %in% stat || 'prop_alt' %in% stat){
    stat <- ggnormfluodbf_aes(stat = prop)
    stat <- rlang::as_name(stat$stat)
  } else {
    stat <- ggnormfluodbf_aes(stat = count)
    stat <- rlang::as_name(stat$stat)
  }

  data_summary <- ybarplot_data_summary(
    data,map = ggnormfluodbf_aes(
    x=!!rlang::sym(x_var),
    y=!!rlang::sym(y_var)),
    v_adj = v_adj)

  mapping_summ <- ggplot2::aes(
    x = !!rlang::sym(x_var),
    y = !!rlang::sym(stat),
    fill = !!rlang::sym(y_var))

  mapping_prop <- ggplot2::aes()
  mapping_prop$fill <- mapping$y
  mapping_prop$x <- mapping$x
  mapping_prop$by <- mapping$x
  data[[rlang::as_name(mapping$y)]] <- factor(data[[rlang::as_name(mapping$y)]])
  if (is.null(bar_args)) bar_args <- list()
  bar_args$position <- ggplot2::position_fill(.5, reverse = F)
  bar_args$stat <- 'count'

  mapping_count <- ggplot2::aes()
  mapping_count$fill <- mapping$y
  mapping_count$x <- mapping$x
  mapping_count$by <- mapping$x
  if (is.null(count_bar_args)) count_bar_args <- list()
  data_count <- data
  data_count[[rlang::as_name(mapping$y)]] <- forcats::fct_rev(data[[rlang::as_name(mapping$y)]])
  count_bar_args$stat <- 'count'
  count_bar_args$position <- 'stack'

  print(stat)
  if (stat == 'prop') {
    p <- ggplot2::ggplot(data_summary, mapping_summ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::geom_text(
        ggplot2::aes(y = ypos, label = scales::percent(prop, accuracy = 0.2)),
        size = size,
        ...)
  } else if (stat == 'prop_alt'){
    p <- ggplot2::ggplot(data, mapping_prop) +
      do.call(ggplot2::geom_bar, bar_args) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = scales::percent(ggplot2::after_stat(count / tapply(count, x, sum)[x]))
        ),
        stat = 'count',
        position = ggplot2::position_fill(vjust = 0.5, reverse = F),
        size = size,
        ...)
  } else {
    p <- ggplot2::ggplot(data_count, mapping_count) +
      do.call(ggplot2::geom_bar, count_bar_args) +
      ggplot2::scale_y_continuous() +
      ggplot2::geom_text(
        stat = 'count',
        ggplot2::aes(label = after_stat(count)),
        position = ggplot2::position_stack(vjust = 0.5, reverse = F),
        ...
      )
  }

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = rev(fill_colors))
  }
  plotly::ggplotly(p)
}

#' @rdname barplots
#' @return ggplot object
#' @export
ggnormfluodbf_vbar_alt <- function(
    data,
    x,
    y,
    size,
    fill_colors = NULL,
    ...
) {
  if (is.null(x) || is.null(y))
    rlang::warn("'x' and 'y' arguments are required", use_cli_format = TRUE)

  x_var <- if (is.symbol(x)) rlang::as_string(x) else x
  y_var <- if (is.symbol(y)) rlang::as_string(y) else y

  if (!x_var %in% names(data))
    rlang::abort(paste("Variable", x_var, "not found in data"), use_cli_format = TRUE)
  if (!y_var %in% names(data))
    rlang::abort(paste("Variable", y_var, "not found in data"), use_cli_format = TRUE)

  data_summary <- data %>%
    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(y_var)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(!!rlang::sym(x_var)) %>%
    dplyr::mutate(
      prop = count / sum(count),
      ypos = cumsum(prop) - (0.5 * prop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::sym(y_var) := forcats::fct_rev(factor(!!rlang::sym(y_var))))

  mapping_summ <- ggplot2::aes(x = !!rlang::sym(x_var), y = prop, fill = !!rlang::sym(y_var))

  p <- ggplot2::ggplot(data_summary, mapping_summ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_text(
      ggplot2::aes(y = ypos, label = scales::percent(prop, accuracy = 0.2)),
      size = size,
      ...)

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = rev(fill_colors))
  }
  plotly::ggplotly(p)
}


#' @rdname barplots
#' @return ggplot object
#' @export
ggnormfluodbf_vbar_alt_two <- function(
    data,
    mapping,
    bar_args =NULL,
    stat = c('prop_alt','prop'),
    size = NULL,
    v_adj = 0.5,
    fill_colors = NULL,
    ...) {
  if (is.null(mapping$x) || is.null(mapping$y))
    rlang::warn("'x' and 'y' arguments are required", use_cli_format = TRUE)


  #handling the count component
  mapping_prop <- ggplot2::aes()
  mapping_prop$fill <- mapping$y
  mapping_prop$x <- mapping$x
  mapping_prop$by <- mapping$x
  data[[rlang::as_name(mapping$y)]] <- factor(data[[rlang::as_name(mapping$y)]]) #ensure data is treated right because count relies on the data
  if (is.null(bar_args)) bar_args <- list()
  bar_args$position <- ggplot2::position_fill(.5, reverse = TRUE)
  bar_args$stat <- 'count'

  #cannot convert a <quosure> into a string so rlang::as_string fails
  x_var <- rlang::as_name(mapping$x)
  y_var <- rlang::as_name(mapping$y)

  #managing the stat param
  if('count' %in% stat){
    stat = ggnormfluodbf_aes(stat = count)
    stat <- rlang::as_name(stat$stat)
  } else {
    stat = ggnormfluodbf_aes(stat = prop)
    stat <- rlang::as_name(stat$stat)
  }

  #add the prop and ypos attributes to the summary of the data
  data_summary <- data %>%
    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(y_var)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(!!rlang::sym(x_var)) %>%
    dplyr::mutate(
      prop = count / sum(count),
      ypos = cumsum(prop) - (v_adj * prop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::sym(y_var) := forcats::fct_rev(factor(!!rlang::sym(y_var))))
  print(data_summary)

  #make an aes map of the needed variables for the prop plot
  mapping_summ <- ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(stat), fill = !!rlang::sym(y_var))

  #create the base ggplot then add components
  #Recipe is:
  #base plot >  stack with geom_col > make percentage axis with scale_y_continuous > add the text (coolest part)
  if(stat == 'prop') {
    p <- ggplot2::ggplot(data_summary, mapping_summ) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::geom_text(
        ggplot2::aes(y = ypos, label = scales::percent(prop, accuracy = 0.2)),
        size = size,
        ...)
  } else {
    p <- ggplot2::ggplot(data, mapping_prop) +
      do.call(ggplot2::geom_bar, bar_args) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = scales::percent(ggplot2::after_stat(count / tapply(count, x, sum)[x])),
        ),
        stat = 'count',
        position = ggplot2::position_fill(vjust = 0.5, reverse=TRUE),
        ...
      )
  }


  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = rev(fill_colors))
  }
  plotly::ggplotly(p)
}

#' @rdname barplots
#' @return ggplot object
#' @export
ggnormfluodbf_hbar <- function(data,
                               mapping,
                               size,
                               h_adj = 0.5,
                               fill_colors = NULL,
                               ...){
  if (is.null(mapping$x) || is.null(mapping$y))
    rlang::warn("'x' and 'y' arguments are required", use_cli_format = TRUE)

  x_var <- rlang::as_name(mapping$x)
  y_var <- rlang::as_name(mapping$y)

  data_summary <- data %>%
    dplyr::group_by(!!rlang::sym(x_var), !!rlang::sym(y_var)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(!!rlang::sym(y_var)) %>%
    dplyr::mutate(
      prop = count / sum(count),
      xpos = cumsum(prop) - (h_adj * prop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::sym(x_var) := forcats::fct_rev(factor(!!rlang::sym(x_var))))
  print(data_summary)

  mapping_summ <- ggplot2::aes(y = !!rlang::sym(y_var), x = prop, fill = !!rlang::sym(x_var))

  p <- ggplot2::ggplot(data_summary, mapping_summ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::geom_text(
      ggplot2::aes(x = xpos, label = scales::percent(prop, accuracy = 0.2)),
      size = size,
      ...)

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = rev(fill_colors))
  }
  plotly::ggplotly(p)
}
