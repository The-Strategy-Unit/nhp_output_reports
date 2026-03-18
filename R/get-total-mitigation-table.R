
get_total_mitigation_table <- function(soc_scenario, obc_scenario, site_codes,scenario_name_1,scenario_name_2){
  # SOC & OBC mitigated activity

  mit_soc_ip <- get_stepcounts(soc_scenario)|>
    dplyr::filter(measure == "beddays",
                  change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(mit_activity_soc = sum(value)) |>
    dplyr::mutate(pod = "Inpatient beddays")

  mit_soc_op <- get_stepcounts(soc_scenario) |>
    dplyr::filter(measure == "attendances" | measure == "tele_attendances",
                  change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(mit_activity_soc = sum(value))  |>
    dplyr::mutate(pod = "Outpatient Attendances")

  mit_soc_ae <- get_stepcounts(soc_scenario) |>
    dplyr::filter(measure == "arrivals",
                  change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(mit_activity_soc = sum(value))  |>
    dplyr::mutate(pod = "A&E attendances (types 1, 3, 5)")

  mit_obc_ip <- get_stepcounts(obc_scenario) |>
    dplyr::filter(measure == "beddays",
                  change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(mit_activity_obc = sum(value))  |>
    dplyr::mutate(pod = "Inpatient beddays")

  mit_obc_op <- get_stepcounts(obc_scenario) |>
    dplyr::filter(measure == "attendances" | measure == "tele_attendances",
                  change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(mit_activity_obc = sum(value))  |>
    dplyr::mutate(pod = "Outpatient Attendances")

  mit_obc_ae <- get_stepcounts(obc_scenario) |>
    dplyr::filter(measure == "arrivals",
                  change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::summarise(mit_activity_obc = sum(value))  |>
    dplyr::mutate(pod = "A&E attendances (types 1, 3, 5)")

  mit_soc <- dplyr::bind_rows(mit_soc_ip, mit_soc_op, mit_soc_ae)
  mit_obc <- dplyr::bind_rows(mit_obc_ip, mit_obc_op, mit_obc_ae)

  mit_soc_obc <- dplyr::full_join(mit_soc, mit_obc) |>
    dplyr::relocate(pod)


  ## write to gt table
  tbl_mit_activity <- mit_soc_obc |>
    gt::gt(rowname_col = "pod") |>
    gt::tab_stubhead(label = "POD") |>
    gt::fmt_number(
      decimals = 0
    ) |>
    gt::tab_spanner(
      label = scenario_name_1,
      columns = c(mit_activity_soc)
    ) |>
    gt::tab_spanner(
      label = scenario_name_2,
      columns = c(mit_activity_obc)
    ) |>
    gt::cols_label(
      mit_activity_soc = "Total estimated levels of mitigated activity",
      mit_activity_obc = "Total estimated levels of mitigated activity"
    ) |>
    gt::tab_source_note(
      source_note = "sum of TPMA level estimates for activity avoidance and efficiencies"
    ) |>
    gt_theme()

  tbl_mit_activity
}
