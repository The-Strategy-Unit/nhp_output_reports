get_tpma_impact_table <- function(soc_scenario,obc_scenario, site_codes){

tpma_soc <- get_tpma_data(soc_scenario) |>
  dplyr::rename(range_soc = range)
tpma_obc <- get_tpma_data(obc_scenario) |>
  dplyr::rename(range_obc = range)

tpma <- dplyr::full_join(tpma_soc, tpma_obc)

mits <- read_mitigators()

impact <- get_impact_data(soc_scenario,obc_scenario, site_codes)

tbl_tpma <- dplyr::left_join(impact, tpma, by = dplyr::join_by(activity_type, strategy)) |>
  dplyr::select(c(strategy, measure, range_soc, impact_soc, range_obc, impact_obc)) |>
  dplyr::left_join(mits) |>
  dplyr::select(-strategy) |>
  dplyr::relocate(mitigator_name) |>
  dplyr::filter(!is.na(mitigator_name)) |>

  gt::gt(rowname_col = "mitigator_name") |>
  gt::tab_stubhead(label = "TPMA") |>
  gt::fmt_number(
    decimals = 0
  ) |>

  gt::sub_missing(
    missing_text = "-"
  ) |>

  gt::cols_label(
    measure = "Measure",
    range_soc = "SOC output model assumption",
    impact_soc = "SOC estimated impact PP (beddays, attendances or appointments)",
    range_obc = "Current model (OBC)",
    impact_obc = "OBC estimated impact PP (beddays, attendances or appointments)"
  )

tbl_tpma

}

