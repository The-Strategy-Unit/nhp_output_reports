

get_soc_obc_table <- function(soc_obc_data)
{
#### build table comparing SOC with OBC ---
tbl_soc_obc <- soc_obc_data |>
  dplyr::arrange(sort) |>
  dplyr::select(-c(measure, pod, sort, p90, obc_pp_var, obc_p90_var)) |>
  dplyr::relocate(heading) |>
  gt::gt(rowname_col = "heading") |>
  gt::fmt_number(
    columns = c(baseline.soc, principal.soc, baseline.obc, principal.obc),
    decimals = 0
  ) |>
  gt::fmt_percent(
    columns = c(cagr.soc, cagr.obc),
    decimals = 2
  ) |>
  gt::sub_missing(
    missing_text = "-"
  ) |>
  gt::tab_spanner(
    label = "SOC",
    columns = c(baseline.soc, principal.soc, cagr.soc)
  ) |>
  gt::tab_spanner(
    label = "OBC",
    columns = c(baseline.obc, principal.obc, cagr.obc)
  ) |>
  gt::cols_label(
    baseline.soc = "Baseline",
    principal.soc = "Principal",
    cagr.soc = "CAGR (%)",
    baseline.obc = "Baseline",
    principal.obc = "Principal",
    cagr.obc = "CAGR (%)"
  ) |>

  # footnote not working
  gt::tab_footnote(
    footnote = "Not available in SOC scenario",
    locations = gt::cells_stub(
      c("Delivery admissions", "Delivery beddays", "SDEC attendances (type 5)")
    )
  ) |>
  gt::opt_footnote_marks(marks = "standard")

tbl_soc_obc
}
