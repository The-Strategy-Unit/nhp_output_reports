get_impact_data <- function(soc_scenario, obc_scenario, soc_site_codes, obc_site_codes){

  #THIS IS FINAL SCENARIO
  # estimated impact PP
  impact_soc <- get_stepcounts(soc_scenario) |>
    dplyr::filter(change_factor %in% c("activity_avoidance", "efficiencies")) |>
    dplyr::group_split(activity_type) |>
    purrr::set_names(c("aae", "ip", "op")) |>
    purrr::map2(
      list(soc_site_codes$aae, soc_site_codes$ip, soc_site_codes$op),
      \(df, sites) filter_sites_conditionally(df, sites)
    )|>
    dplyr::bind_rows() |>
    dplyr::summarise(
      impact_soc = sum(value),
      .by = c(strategy, activity_type, measure)
    )

  #THIS IS VALIDATION
  impact_obc <- get_stepcounts(obc_scenario) |>
    dplyr::filter(change_factor %in% c("activity_avoidance", "efficiencies")) |>
    dplyr::group_split(activity_type) |>
    purrr::set_names(c("aae", "ip", "op")) |>
    purrr::map2(
      list(obc_site_codes$aae, obc_site_codes$ip, obc_site_codes$op),
      \(df, sites) filter_sites_conditionally(df, sites)
    )|>
    dplyr::bind_rows() |>
    dplyr::summarise(
      impact_obc = sum(value),
      .by = c(strategy, activity_type, measure)
    )

  impact <- dplyr::full_join(impact_soc, impact_obc) |>
    # remove entries with no mitigation
    dplyr::filter_out(impact_soc == 0 & impact_obc == 0) |>
    # new line added to remove admissions rows since already represented as beddays for ip
    dplyr::filter(measure != "admissions")

  impact

}

