#' Ggnormfluodbf Box Plot
#' @importFrom stats median
#' @param data data
#' @param mapping mapping
#' @param alpha alpha
#' @param dodge_pos dodge_pos
#' @param vadj vertical adjustment
#' @param hadj horizontal adjustment
#' @param mean_point_size size
#' @param mean_point_shape shape
#' @param mean_point_color color
#' @param include_mean mean boolean
#' @param include_labels labels boolean
#' @param label_text_size label size
#' @param label_box_outline label box outline size
#' @param label_color label text color
#' @param label_alpha color intensity
#' @param label_family font family
#' @param label_fontface font face
#' @param label_box_padding padding
#' @param label_box_alpha label box color intensity
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' ggnormfluodbf_box(tips,ggplot2::aes(x = smoker, y = tip, fill = sex))
#' ggnormfluodbf_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2)
#' ggnormfluodbf_box(tips,ggplot2::aes(x=smoker, y = tip, fill = smoker), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' ggnormfluodbf_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7)
#' ggnormfluodbf_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F)
#' ggnormfluodbf_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' }
ggnormfluodbf_box <- function(data,
                              mapping,
                              alpha = 1.0,
                              dodge_pos = 0.75,
                              vadj = 0,
                              hadj = 0,
                              mean_point_size = 6,
                              mean_point_shape = 18, #10,11,12,15*,16=20,17*,18**,21
                              mean_point_color = "red",
                              include_mean = TRUE,
                              include_labels = TRUE,
                              label_text_size = 4.5,
                              label_box_outline = 0.5,
                              label_color = "blue",
                              label_alpha = 1.0,
                              label_family = "serif",
                              label_fontface = "bold",
                              label_box_padding = ggplot2::unit(0.5, "lines"),
                              label_box_alpha = 0.8){
  mapping$x <- mapping$x
  mapping$y <- mapping$y
  mapping$fill <- mapping$fill

  if (is.null(include_mean) || include_mean){
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_boxplot(alpha = alpha) +
      ggplot2::stat_summary(fun = mean,
                            mapping = ggplot2::aes(color=mapping$fill),
                            geom = "point",
                            position = ggplot2::position_dodge2(width = dodge_pos, preserve = "single"),
                            shape = mean_point_shape,
                            size = mean_point_size,
                            color = mean_point_color
      )
  } else {
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_boxplot(alpha = alpha)
  }

  if (is.null(include_labels) || include_labels) {
    calc_stats <- function(x) {
      q75 <- stats::quantile(x, 0.75)
      iqr <- IQR(x)
      upper_whisker <- max(max(x), q75 + 1.5 * iqr)
      data.frame(
        y = upper_whisker * 1.01,
        label = sprintf("Median: %.2f\nMean: %.2f", stats::median(x), mean(x))
      )
    }

    p <- p +
      ggplot2::theme(legend.position='none') +
      ggplot2::stat_summary(
        fun.data = calc_stats,
        geom = "label",
        position = ggplot2::position_dodge(width = dodge_pos),
        vjust = 0,
        size = label_text_size,
        color = label_color,
        alpha = label_alpha,
        family = label_family,
        fontface = label_fontface,
        label.padding = label_box_padding,
        label.size = label_box_outline
      )
    p
  }
  else {
    p
  }
  p
}

#' Ggnormfluodbf Box Plot Deprecated
#' @importFrom stats median
#' @param data data
#' @param mapping mapping
#' @param alpha alpha
#' @param dodge_pos dodge_pos
#' @param vadj vertical adjustment
#' @param hadj horizontal adjustment
#' @param include_mean mean boolean
#' @param include_labels labels boolean
#' @return ggplot object
#' @examples
#' \dontrun{
#' ggnormfluodbf_box_textgeom_deprecated(tips,ggplot2::aes(x = smoker, y = tip, fill = sex))
#' ggnormfluodbf_box_textgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2)
#' ggnormfluodbf_box_textgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = smoker), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' ggnormfluodbf_box_textgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7)
#' ggnormfluodbf_box_textgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F)
#' ggnormfluodbf_box_textgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' }
ggnormfluodbf_box_textgeom_deprecated <- function(data,
                              mapping,
                              alpha = 0.5,
                              dodge_pos = 0.75,
                              vadj = 1.5,
                              hadj = 0,
                              include_mean = TRUE,
                              include_labels = TRUE){
  mapping$x <- mapping$x
  mapping$y <- mapping$y
  mapping$fill <- mapping$fill

  if (is.null(include_mean) || include_mean){
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_boxplot(alpha = alpha) +
      ggplot2::stat_summary(fun=mean,
                            mapping = ggplot2::aes(color=mapping$fill),
                            geom="point",
                            position = ggplot2::position_dodge2(width = dodge_pos, preserve = "single"),
                            shape=20,
                            size=3,
                            color="blue"
      )
  } else {
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_boxplot(alpha = alpha)
  }

  if (is.null(include_labels) || include_labels) {
    p <- p +
      ggplot2::theme(legend.position='none') +
      ggplot2::geom_text(
        stat = "summary",
        fun = median,
        mapping = ggplot2::aes(label = sprintf("median = %.2f", ggplot2::after_stat(y) )),
        position = ggplot2::position_dodge(width = dodge_pos),
        vjust = vadj,
        hjust = hadj) +
      ggplot2::geom_text(
        stat = "summary",
        fun = mean,
        mapping = ggplot2::aes(label = sprintf("mean = %.2f", ggplot2::after_stat(y) )),
        position = ggplot2::position_dodge(width = dodge_pos),
        vjust = vadj + 3.0,
        hjust = hadj)
    p
  }
  else {
    p
  }
  p
}

#' Ggnormfluodbf Box Plot
#' @importFrom stats median
#' @param data data
#' @param mapping mapping
#' @param alpha alpha
#' @param dodge_pos dodge_pos
#' @param vadj vertical adjustment
#' @param hadj horizontal adjustment
#' @param include_mean mean boolean
#' @param include_labels labels boolean
#' @param label_size label size
#' @param label_color label text color
#' @param label_alpha color intensity
#' @param label_family font family
#' @param label_fontface font face
#' @param label_box_padding padding
#' @param label_box_alpha label box color intensity
#' @return ggplot object
#' @examples
#' \dontrun{
#' ggnormfluodbf_box_labelgeom_deprecated(tips,ggplot2::aes(x = smoker, y = tip, fill = sex))
#' ggnormfluodbf_box_labelgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2)
#' ggnormfluodbf_box_labelgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = smoker), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' ggnormfluodbf_box_labelgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7)
#' ggnormfluodbf_box_labelgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F)
#' ggnormfluodbf_box_labelgeom_deprecated(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' }
ggnormfluodbf_box_labelgeom_deprecated <- function(data,
                              mapping,
                              alpha = 0.5,
                              dodge_pos = 0.75,
                              vadj = 1.5,
                              hadj = 0,
                              include_mean = TRUE,
                              include_labels = TRUE,
                              label_size = 3.5,
                              label_color = "red",
                              label_alpha = 0.8,
                              label_family = "serif",
                              label_fontface = "bold",
                              label_box_padding = ggplot2::unit(0.5, "lines"),
                              label_box_alpha = 0.8){
  mapping$x <- mapping$x
  mapping$y <- mapping$y
  mapping$fill <- mapping$fill

  if (is.null(include_mean) || include_mean){
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_boxplot(alpha = alpha) +
      ggplot2::stat_summary(fun=mean,
                            mapping = ggplot2::aes(color=mapping$fill),
                            geom="point",
                            position = ggplot2::position_dodge2(width = dodge_pos, preserve = "single"),
                            shape=20,
                            size=3,
                            color="blue"
      )
  } else {
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_boxplot(alpha = alpha)
  }

  if (is.null(include_labels) || include_labels) {
    p <- p +
      ggplot2::theme(legend.position='none') +
      ggplot2::geom_label(
        stat = "summary",
        fun = median,
        mapping = ggplot2::aes(label = sprintf("median = %.2f", ggplot2::after_stat(y) )),
        position = ggplot2::position_dodge(width = dodge_pos),
        vjust = vadj,
        hjust = hadj,
        size = label_size,
        color = label_color,
        alpha = label_alpha,
        family = label_family,
        fontface = label_fontface,
        label.padding = label_box_padding,
        label.size = 1) +
      ggplot2::geom_label(
        stat = "summary",
        fun = mean,
        mapping = ggplot2::aes(label = sprintf("mean = %.2f", ggplot2::after_stat(y) )),
        position = ggplot2::position_dodge(width = dodge_pos),
        vjust = vadj + 3.0,
        hjust = hadj,
        size = label_size,
        color = label_color,
        alpha = label_alpha,
        family = label_family,
        fontface = label_fontface,
        label.padding = label_box_padding,
        label.size = 1)
    p
  }
  else {
    p
  }
  p
}
