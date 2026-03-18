get_tpma_impact_table <- function(soc_scenario,obc_scenario, site_codes,scenario_name_1,scenario_name_2){

  tpma_soc <- get_tpma_data(soc_scenario) |>
    dplyr::rename(range_soc = range)
  tpma_obc <- get_tpma_data(obc_scenario) |>
    dplyr::rename(range_obc = range)

  tpma <- dplyr::full_join(tpma_soc, tpma_obc)

  mits <- read_mitigators()

  impact <- get_impact_data(soc_scenario,obc_scenario, site_codes)

  tpma <- dplyr::left_join(impact, tpma, by = dplyr::join_by(activity_type, strategy)) |>
    dplyr::select(c(strategy, measure, range_soc, impact_soc, range_obc, impact_obc)) |>
    dplyr::left_join(mits) |>
    dplyr::select(-strategy) |>
    dplyr::relocate(mitigator_name) |>
    dplyr::filter(!is.na(mitigator_name))  |>
    dplyr::arrange(measure, mitigator_name)    # new line


  # This section is to force the range onto the arrivals impact for these mitigators
  sdec_veryhigh <-tpma |>
    dplyr::filter(mitigator_name=="Same Day Emergency Care (Very High Potential)" & measure == "beddays") |>
    dplyr::pull(range_obc)
  if (length(sdec_veryhigh)!=0){
    tpma <- tpma |>
      dplyr::mutate(range_obc = dplyr::case_when(mitigator_name=="Same Day Emergency Care (Very High Potential)" & measure != "beddays" ~ sdec_veryhigh,
                                                 .default = range_obc))
  }

  sdec_high <- tpma |>
    dplyr::filter(mitigator_name=="Same Day Emergency Care (High Potential)" & measure == "beddays") |>
    dplyr::pull(range_obc)
  if (length(sdec_high)!=0){
    tpma <- tpma |>
      dplyr::mutate(range_obc = dplyr::case_when(mitigator_name=="Same Day Emergency Care (High Potential)" & measure != "beddays" ~ sdec_high,
                                                 .default = range_obc))
  }

  sdec_moderate <- tpma |>
    dplyr::filter(mitigator_name=="Same Day Emergency Care (Moderate Potential)" & measure == "beddays") |>
    dplyr::pull(range_obc)
  if (length(sdec_moderate)!=0){
    tpma <- tpma |>
      dplyr::mutate(range_obc = dplyr::case_when(mitigator_name=="Same Day Emergency Care (Moderate Potential)" & measure != "beddays" ~ sdec_moderate,
                                                 .default = range_obc))
  }

  sdec_low <- tpma |>
    dplyr::filter(mitigator_name=="Same Day Emergency Care (Low Potential)" & measure == "beddays") |>
    dplyr::pull(range_obc)
  if (length(sdec_low)!=0){
    tpma <- tpma |>
      dplyr::mutate(range_obc = dplyr::case_when(mitigator_name=="Same Day Emergency Care (Low Potential)" & measure != "beddays" ~ sdec_low,
                                                 .default = range_obc))
  }


  #create table titles
  range_soc_label = glue::glue(scenario_name_1, " output model assumption")
  impact_soc_label = glue::glue(scenario_name_1, " estimated impact PP (beddays, attendances or appointments)")
  range_obc_label = glue::glue("Current model (",scenario_name_2,")")
  impact_obc_label = glue::glue(scenario_name_2, " estimated impact PP (beddays, attendances or appointments)")


  # build table, rows by A&E, IP, OP.
  # order of sub-rows not as per the Word doc, can manually enter row-names in order if necessary


  tbl_tpma <- tpma |>
    gt::gt(rowname_col = "mitigator_name") |>
    gt::tab_stubhead(label = "TPMA") |>
    gt::fmt_number(
      decimals = 0
    ) |>

    gt::sub_missing(
      missing_text = "-"
    ) |>
    gt::tab_row_group(
      label = "Outpatients",
      rows = measure %in% c("attendances", "tele_attendances")
    ) |>

    gt::tab_row_group(
      label = "Inpatients",
      rows = measure == "beddays"
    ) |>

    gt::tab_row_group(
      label = "A&E",
      rows = measure == "arrivals"
    ) |>

    gt::cols_label(
      measure = "Measure",
      range_soc = range_soc_label,
      impact_soc = impact_soc_label,
      range_obc = range_obc_label,
      impact_obc = impact_obc_label
    )

  tbl_tpma

}

