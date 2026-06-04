get_covid_soc_cagr <- function (soc_scenario, site_codes){

### COVID CAGR adjustment for baseline = 19/20

# create values for each POD, compute CAGR with Covid adjustment

### soc
# IP
baseline <- get_stepcounts(soc_scenario) |>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  dplyr::filter(change_factor == "baseline") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(factor = "baseline")

baseline_adjustment <- get_stepcounts(soc_scenario) |>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  dplyr::filter(change_factor == "baseline_adjustment") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(factor = "baseline_adjustment")

covid_adjustment <- get_stepcounts(soc_scenario) |>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  dplyr::filter(change_factor == "covid_adjustment") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(factor = "covid")

all <- get_stepcounts(soc_scenario)   |>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(value = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(factor = "all")

cov_cagr_ip <- dplyr::bind_rows(baseline, baseline_adjustment, covid_adjustment, all) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(cagr = ((sum(value[factor == "all"]) /
                              (sum(value[factor == "baseline"]) +
                                 sum(value[factor == "baseline_adjustment"]) +
                                 sum(value[factor == "covid_adjustment"])))^(1 / fc_period_soc) - 1)
  ) |>
  dplyr::ungroup()


# OP
  baseline <- get_stepcounts(soc_scenario) |>
    dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  baseline_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
    dplyr::filter(change_factor == "baseline_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  covid_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  all <- get_stepcounts(soc_scenario)  |>
    dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::pull()

  net_gr <- ((all / (baseline + baseline_adjustment + covid_adjustment))^(1 / fc_period_soc) - 1)

  cov_cagr_op <- data.frame(pod = "op_outpatients", measure = "attendances", cagr = net_gr)


  # DG & NDG for A&E
  baseline <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                         pod == "aae_type-05" ~ "sdec"),
                  measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                             pod == "sdec" ~ "attendances (type 5)")) |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(factor = "baseline")

  baseline_adjustment <- get_stepcounts(soc_scenario) |>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "baseline_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                         pod == "aae_type-05" ~ "sdec"),
                  measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                             pod == "sdec" ~ "attendances (type 5)")) |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(factor = "baseline_adjustment")

  covid_adjustment <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    dplyr::filter(change_factor == "covid_adjustment") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                         pod == "aae_type-05" ~ "sdec"),
                  measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                             pod == "sdec" ~ "attendances (type 5)")) |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::mutate(factor = "covid")

  all <- get_stepcounts(soc_scenario)|>
    dplyr::filter(activity_type == "aae") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                         pod == "aae_type-05" ~ "sdec"),
                  measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                             pod == "sdec" ~ "attendances (type 5)")) |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::ungroup()|>
    dplyr::mutate(factor = "all")

  cov_cagr_ae <- dplyr::bind_rows(baseline, baseline_adjustment, covid_adjustment, all) |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(cagr = ((sum(value[factor == "all"]) /
                                (sum(value[factor == "baseline"]) +
                                   sum(value[factor == "baseline_adjustment"]) +
                                   sum(value[factor == "covid_adjustment"])))^(1 / fc_period_soc) - 1)
    ) |>
    dplyr::ungroup()

  cov_cagr_soc <- dplyr::bind_rows(cov_cagr_ip, cov_cagr_ae, cov_cagr_op)

  cov_cagr_soc

}
