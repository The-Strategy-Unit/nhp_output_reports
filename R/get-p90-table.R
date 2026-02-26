
get_p90_table <- function(soc_obc){
# pulls 90% CI from OBC and compares with SOC Principal Projection

tbl_soc_obc_p90 <- soc_obc |>
  dplyr::arrange(sort) |>
  dplyr::select(c(heading, principal.soc, principal.obc, obc_pp_var, p90, obc_p90_var)) |>
  dplyr::relocate(heading) |>
  gt::gt(rowname_col = "heading") |>
  gt::fmt_number(
    decimals = 0
  ) |>

  gt::sub_missing(
    missing_text = "-"
  ) |>

  gt::cols_label(
    principal.soc = "SOC Principal",
    principal.obc = "OBC Principal",
    obc_pp_var = "OBC PP variation from SOC PP",
    p90 = "OBC P90",
    obc_p90_var = "OBC P90 variation from SOC PP"

  ) |>

  # footnote not working
  gt::tab_footnote(
    footnote = "Not available in SOC scenario",
    locations = gt::cells_stub(
      c("Delivery admissions", "Delivery beddays", "SDEC attendances (type 5)")
    )
  ) |>
  gt::opt_footnote_marks(marks = "standard") |>
  gt_theme()

tbl_soc_obc_p90
}
