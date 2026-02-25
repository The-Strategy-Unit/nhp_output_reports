
get_total_mitigation_table <- function(r_final_report_ndg2, r_validation_report_ndg2, site_codes){
# SOC & OBC mitigated activity

mit_soc_ip <- get_stepcounts(r_final_report_ndg2)|>
  dplyr::filter(measure == "beddays",
                change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(mit_activity_soc = sum(value)) |>
  dplyr::mutate(pod = "Inpatient beddays")

mit_soc_op <- get_stepcounts(r_final_report_ndg2) |>
  dplyr::filter(measure == "attendances" | measure == "tele_attendances",
                change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
  filter_sites_conditionally(site_codes$op) |>
  dplyr::summarise(mit_activity_soc = sum(value))  |>
  dplyr::mutate(pod = "Outpatient Attendances")

mit_soc_ae <- get_stepcounts(r_final_report_ndg2) |>
  dplyr::filter(measure == "arrivals",
                change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
  filter_sites_conditionally(site_codes$aae) |>
  dplyr::summarise(mit_activity_soc = sum(value))  |>
  dplyr::mutate(pod = "A&E attendances (types 1, 3, 5)")

mit_obc_ip <- get_stepcounts(r_addendum_report_ndg2) |>
  dplyr::filter(measure == "beddays",
                change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::summarise(mit_activity_obc = sum(value))  |>
  dplyr::mutate(pod = "Inpatient beddays")

mit_obc_op <- get_stepcounts(r_addendum_report_ndg2) |>
  dplyr::filter(measure == "attendances" | measure == "tele_attendances",
                change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
  filter_sites_conditionally(site_codes$op) |>
  dplyr::summarise(mit_activity_obc = sum(value))  |>
  dplyr::mutate(pod = "Outpatient Attendances")

mit_obc_ae <- get_stepcounts(r_addendum_report_ndg2) |>
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
    label = "SOC",
    columns = c(mit_activity_soc)
  ) |>
  gt::tab_spanner(
    label = "OBC",
    columns = c(mit_activity_obc)
  ) |>
  gt::cols_label(
    mit_activity_soc = "Total estimated levels of mitigated activity",
    mit_activity_obc = "Total estimated levels of mitigated activity"
  ) |>
  gt::tab_source_note(
    source_note = "sum of TPMA level estimates for activity avoidance and efficiencies"
  )

tbl_mit_activity
}
