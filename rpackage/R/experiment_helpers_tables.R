compute_latex_dataframe <- function(x, row_vars, col_var, value, se = TRUE){
  checkmate::assert_class(x, "data.frame")
  checkmate::assert_character(row_vars)
  checkmate::assert_subset(row_vars, names(x))
  checkmate::assert_string(col_var)
  checkmate::assert_subset(col_var, names(x))
  checkmate::assert_string(value)
  checkmate::assert_subset(value, names(x))
  checkmate::assert_flag(se)

  tab <- eval(parse(text = paste0("dplyr::group_by(x, ", paste(row_vars, collapse = ", "), ", ", col_var, ")")))
  var_nms <- paste0("mean_", value)
  sum_text <- paste0(var_nms," = mean(",value,")")
  if(se) {
    sum_text <- paste0(sum_text, ", se_", value," = sd(",value,")/dplyr::n()")
    var_nms <- c(var_nms, paste0("se_", value))
  }
  tab <- eval(parse(text = paste0(" dplyr::summarise(tab, ", sum_text, ", n = dplyr::n())")))
  tab <- eval(parse(text = paste0(" tidyr::pivot_wider(tab, names_from = ", col_var, ", values_from = c(", paste0(var_nms, collapse = ", "),", n))")))
  tab
}

write_latex_table <- function(x, file_path, ...){
  checkmate::assert_class(x, "data.frame")
  xtab <- xtable::xtable(x, ...)
  latex_table <- utils::capture.output(xtable::print.xtable(xtab, booktabs = TRUE, include.rownames = FALSE))
  writeLines(latex_table, con = file_path)
}

