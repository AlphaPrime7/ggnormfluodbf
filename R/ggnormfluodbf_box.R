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
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' ggnormfluodbf_grouped_box(tips,ggplot2::aes(x = smoker, y = tip, fill = sex))
#' ggnormfluodbf_grouped_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2)
#' ggnormfluodbf_grouped_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2)
#' ggnormfluodbf_grouped_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7)
#' ggnormfluodbf_grouped_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F)
#' ggnormfluodbf_grouped_box(tips,ggplot2::aes(x=smoker, y = tip, fill = sex), alpha = 2, vadj = -5, hadj = 0.7, include_labels = F, include_mean = F)
#' }
ggnormfluodbf_grouped_box <- function(data,
                              mapping,
                              alpha = 0.5,
                              dodge_pos = 0.75,
                              vadj = -0.5,
                              hadj = 0.5,
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
                            position = ggplot2::position_dodge2(width = 0.75, preserve = "single"),
                            shape=20,
                            size=3,
                            color="red"
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
        hjust = hadj
      ) +
      ggplot2::geom_text(
        stat = "summary",
        fun = mean,
        mapping = ggplot2::aes(label = sprintf("mean = %.2f", ggplot2::after_stat(y) )),
        position = ggplot2::position_dodge(width = dodge_pos),
        vjust = vadj + 2.5,
        hjust = hadj
      )
    p
  }
  else {
    p
  }
  p
}
