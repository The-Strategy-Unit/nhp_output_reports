

get_soc_obc_table <- function(soc_obc_data,soc_major_version,scenario_name_1,scenario_name_2)
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
      label = scenario_name_1,
    # Code for further change upon confirmation from schemes
    #  label = glue::glue("Scenario ",scenario_name_1),
      columns = c(baseline.soc, principal.soc, cagr.soc)
    ) |>
    gt::tab_spanner(
      label = scenario_name_2,
   # Code for further change upon confirmation from schemes
    #  label = glue::glue("Scenario ",scenario_name_2),
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
    gt::tab_style_body(
      style = gt::cell_text(style = "italic"),
      targets = "row",
      values = c("Delivery admissions", "Delivery beddays"),
      extents = c("body", "stub")
    ) |>
    gt::tab_footnote(
      footnote = "Not available in earlier scenario",
      locations = gt::cells_stub(
        c("Regular Day Attender admissions", "Delivery admissions", "Delivery beddays", "SDEC attendances (type 5)")
      )
    ) |>
    gt::tab_footnote(
      footnote = "Deliveries are a subset of maternity.",
      locations = gt::cells_stub(
        c("Delivery admissions", "Delivery beddays")
      )
    ) |>
    gt::tab_footnote(
      footnote = "Activity assumed to be SDEC due to TPMAs. Care should be taken in interpretation as recording of activity is in flux.",
      locations = gt::cells_stub(
        c("SDEC attendances (type 5)")
      )
    ) |>
    gt::opt_footnote_marks(marks = "numbers") |>
    gt_theme()

  # Code awaiting detail on the footnote when received
  #if(soc_major_version==1){
  #  tbl_soc_obc <-
  #  tbl_soc_obc |>
  #  gt::tab_footnote(
  #    footnote = "Note: In v1 Maternity Growth...",

  #  )}

  tbl_soc_obc
}
