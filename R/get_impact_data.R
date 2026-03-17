get_impact_data <- function(soc_scenario, obc_scenario, site_codes){

  # estimated impact PP
  impact_soc <- get_stepcounts(soc_scenario) |>
    dplyr::filter(change_factor %in% c("activity_avoidance", "efficiencies")) |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::summarise(
      impact_soc = sum(value),
      .by = c(strategy, activity_type, measure)
    ) |>

  impact_obc <- get_stepcounts(obc_scenario) |>
    dplyr::filter(change_factor == "activity_avoidance" | change_factor == "efficiencies") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::group_by(strategy, activity_type, measure) |>
    dplyr::summarise(impact_obc = sum(value)) |>
    dplyr::ungroup()

  impact <- dplyr::full_join(impact_soc, impact_obc) |>
    # remove entries with no mitigation
    dplyr::filter(impact_soc != 0 | impact_obc != 0) |>
    # new line added to remove admissions rows since already represented as beddays for ip
    dplyr::filter(measure != "admissions")

  impact

}

