## normfluodbf - R package for plotting normfluodbf data and other generic functions for visualization immersion
## Copyright (C) 2024 Tingwei Adeck

# --------------------- General ---------------------------------

#' Interactive Print
#' @param p function
#' @return NULL
#' @export
#' @examples \dontrun{
#' print_if_interactive(p)
#' }
print_if_interactive <- function(p) {
  if (interactive()) {
    print(p)
  }
}

#' String Concatenate
#' @param ... dots
#' @param sep separator
#' @param collapse collapse
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' str_c(...)
#' }
str_c <- function(..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

#' Require Namespace
#' @param pkgs requisite pkgs
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' require_namespaces(pkgs)
#' }
require_namespaces <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(str_c("please install the package '", pkg, "'.  install.packages('", pkg, "') "))
    }
  }
}

#' Read a function from the functions dictionary
#' @param key key
#' @param file file with dictionary
#' @return NULL
#' @export
#' @examples \dontrun{
#' read_functions_dictionary(test_package)
#' }
read_functions_dictionary <- function(key, file) {
  eval(parse(text = read.dcf(file, fields = key)))
}

# --------------------- Aesthetics ggnormfluodbf ---------------------------------
#' ggnormfluodbf aesthetic mapper
#' @param ... dots
#' @return aes list
#' @export
#' @examples \dontrun{
#' ggnormfluodbf_aes(x, y)
#' }
ggnormfluodbf_aes <- function(...) {
  aes_list <- rlang::enquos(...)
  aes_list <- lapply(aes_list, function(x) {
    expr <- rlang::quo_get_expr(x)
    if (rlang::is_string(expr)) {
      rlang::new_quosure(rlang::sym(expr), rlang::quo_get_env(x))
    } else {
      x
    }
  })
  class(aes_list) <- "ggnormfluodbf_aes"
  return(aes_list)
}

#' Print
#' @family printer
#' @param x print requirement
#' @param ... placeholder
#' @return NULL
#' @examples
#' \dontrun{
#' print.ggnormfluodbf_aes(map)
#' }
print.ggnormfluodbf_aes <- function(x, ...) {
  aes_map <- x
  cat("Normfluodbf Aesthetic mappings:\n")
  for (name in names(aes_map)) {
    expr <- aes_map[[name]]
    if (rlang::is_symbol(expr)) {
      value <- rlang::as_string(expr)
    } else if (rlang::is_quosure(expr)) {
      value <- rlang::quo_text(expr)
    } else {
      value <- rlang::expr_text(expr)
    }
    cat(name, " = ", value, "\n", sep = "")
  }
  invisible(aes_map)
}

# --------------------- Data Summary ---------------------------------
#' Y Bar Data Summary
#' @param data data
#' @param map ggnormfluodbf aesthetic map
#' @param v_adj vertical adjustment
#' @return data summary
#' @export
#' @examples \dontrun{
#' weird <- ggplot2::aes(x=smoker,y=sex)
#' ybarplot_data_summary(data, map = ggnormfluodbf_aes(x=sym(as_name(weird$x)),
#' y=sym(as_name(weird$y))))
#' }
ybarplot_data_summary <- function(data, map, v_adj){

  x <- rlang::as_name(map$x)
  y <- rlang::as_name(map$y)

  data <- data %>%
    dplyr::group_by(!!rlang::sym(x),!!rlang::sym(y)) %>%
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
    dplyr::group_by(!!rlang::sym(x)) %>%
    dplyr::mutate(
      prop = count / sum(count),
      cum = cumsum(prop),
      ypos = cum - (v_adj * prop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::sym(y) := forcats::fct_rev(factor(!!rlang::sym(y))))
  data
}

#' X Bar Data Summary
#' @param data data
#' @param map ggnormfluodbf aesthetic map
#' @param h_adj horizontal adjustment
#' @return data summary
#' @export
#' @examples \dontrun{
#' weird <- aes(x=smoker,y=sex)
#' xbarplot_data_summary(data,map = ggnormfluodbf_aes(x=sym(as_name(weird$x)),
#' y=sym(as_name(weird$y))))
#' }
xbarplot_data_summary <- function(data, map, h_adj) {

  x <- rlang::as_name(map$x)
  y <- rlang::as_name(map$y)

  data <- data %>%
    dplyr::group_by(!!rlang::sym(x), !!rlang::sym(y)) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::group_by(!!rlang::sym(y)) %>%
    dplyr::mutate(
      prop = count / sum(count),
      xpos = cumsum(prop) - (h_adj * prop)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!rlang::sym(x) := forcats::fct_rev(factor(!!rlang::sym(x))))
  data
}

#' Active Binding in R
#' @param name name
#' @param value value
#' @return NULL
#' @export
#' @examples
#' \dontrun{
#' set_invisible_variable('title',"")}
set_invisible_variable <- function(name, value) {
  parent_env <- parent.frame()

  makeActiveBinding(name, function() value, parent_env)

  # Hide the binding
  bindingIsActive(name, parent_env)
  environmentIsLocked(parent_env)
  lockBinding(name, parent_env)
}
