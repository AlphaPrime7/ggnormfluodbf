## normfluodbf - R package for plotting normfluodbf data and other generic functions for visualization immersion
## Copyright (C) 2024 Tingwei Adeck

# --------------------- General ---------------------------------

#' Interactive Print
#' @param p function
#' @return NULL
#' @export
#' @examples \dontrun{print_if_interactive(p)}
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
#' \dontrun{str_c("please install the package '", pkg, "'.  install.packages('", pkg, "') ")}
str_c <- function(..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

#' Require Namespace
#' @param pkgs requisite pkgs
#' @return NULL
#' @export
#' @examples
#' \dontrun{require_namespaces(pkgs)}
require_namespaces <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(str_c("please install the package '", pkg, "'.  install.packages('", pkg, "') "))
    }
  }
}

#' Create dictionary in .dcf format
#' @param key key
#' @param func value
#' @param filename_with_dcf_ext file name with dcf extension
#' @param temp_loc temp boolean
#' @return NULL
#' @export
#' @examples
#' \dontrun {
#' tp <- function(package = "./", ...){
#' devtools::document(package)
#' devtools::test(package, ...)}
#' key <- "test_package"
#' dict <- create_functions_dictionary()
#' file_path <- dict$add_field(field_name = "bar_data", mbd, "hf.dcf")}
create_functions_dictionary <- function() {
  dcf_content <- list()
  dcf_file <- NULL

  format_function <- function(func, field_name) {
    func_string <- deparse(func)

    formatted_func <- c(
      paste0(field_name, " <- function (", paste(names(formals(func)), collapse = ", "), ") "),
      paste0("\t", trimws(func_string[-c(1, length(func_string))])),
      "}",
      paste0(field_name, "()")
    )

    paste(formatted_func, collapse = "\n")
  }

  add_field <- function(field_name, field_value, filename_with_dcf_ext = NULL, temp_loc = FALSE) {
    if (is.function(field_value)) {
      field_value <- format_function(field_value, field_name)
    }

    dcf_content[[field_name]] <<- field_value

    if (!is.null(filename_with_dcf_ext)) {
      dcf_text <- sapply(names(dcf_content), function(name) {
        value <- dcf_content[[name]]
        value_lines <- strsplit(value, "\n")[[1]]
        indented_value <- paste0("\t", value_lines, collapse = "\n")
        paste0(name, ":\n", indented_value)
      })
      dcf_text <- paste(dcf_text, collapse = "\n\n")

      if (temp_loc) {
        dcf_file <<- tempfile(fileext = paste0(".", filename_with_dcf_ext))
      } else {
        dcf_file <<- filename_with_dcf_ext
      }

      writeLines(dcf_text, dcf_file)
    }

    invisible(dcf_file)
  }

  get_file <- function() {
    dcf_file
  }

  list(add_field = add_field, get_file = get_file)
}

#' Read a function from the functions dictionary
#' @param key key
#' @return NULL
#' @export
#' @examples \dontrun{read_functions_dictionary(test_package)}
read_functions_dictionary <- function(key, file) {
  eval(parse(text = read.dcf(file, fields = key)))
}

# --------------------- Aesthetics ggnormfluodbf ---------------------------------
#' ggnormfluodbf aesthetic mapper
#' @param ... dots
#' @return aes list
#' @export
#' @examples \dontrun{ggnormfluodbf_aes(x=smokeyou, y=yourmamasofat)}
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
#' \dontrun{aes}
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

#' Y Bar Data Summary
#' @param data data
#' @param map ggnormfluodbf aesthetic map
#' @param v_adj vertical adjustment
#' @return data summary
#' @export
#' @examples \dontrun{
#' weird <- ggplot2::aes(x=smoker,y=sex)
#' ybarplot_data_summary(data,
#'                      map = ggnormfluodbf_aes(x=!!rlang::sym(rlang::as_name(weird$x)),
#'                                              y=!!rlang::sym(rlang::as_name(weird$y))))}
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

