get_soc_obc <- function (soc_scenario, obc_scenario, site_codes){

  # get baseline year for CAGR calculation
  soc_base_yr <- soc_scenario[["params"]][["start_year"]]

#### SOC figures from final report ---
# Sample code to get the outputs from the default tab of the download spreadsheet
soc_ip <- get_baseline_and_projections(soc_scenario)|>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod,measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal)) |>
  dplyr::ungroup()


# Code to get A&E and SDEC from default tab
soc_ae <- get_baseline_and_projections(soc_scenario)|>
  dplyr::filter(measure=="ambulance" | measure == "walk-in") |>
  filter_sites_conditionally(site_codes$aae) |>
  dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                       pod == "aae_type-05" ~ "sdec"),
                measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                           pod == "sdec" ~ "attendances (type 5)")) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal)) |>
  dplyr::ungroup()

# code to get OP activity from defaults tab
soc_op <- get_baseline_and_projections(soc_scenario)|>
  dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
  filter_sites_conditionally(site_codes$op) |>
  dplyr::mutate(pod = "op_outpatients", measure = "attendances") |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal)) |>
  dplyr::ungroup()


# need baseline_adjustment from step_counts
baseline_adjustment <- get_stepcounts(soc_scenario) |>
  dplyr::filter(change_factor == "baseline_adjustment") |>
  dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                       pod == "aae_type-05" ~ "sdec",
                                       stringr::str_starts(pod, "op_") ~ "op_outpatients",
                                       TRUE ~ pod),
                measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                           pod == "sdec" ~ "attendances (type 5)",
                                           pod == "op_outpatients" ~ "attendances",
                                           TRUE ~ measure)
  ) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline_adjustment = sum(value)) |>
  dplyr::ungroup()

# need covid_adjustment from step_counts
covid_adjustment <- get_stepcounts(soc_scenario) |>
  dplyr::filter(change_factor == "covid_adjustment") |>
  dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                       pod == "aae_type-05" ~ "sdec",
                                       stringr::str_starts(pod, "op_") ~ "op_outpatients",
                                       TRUE ~ pod),
                measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                           pod == "sdec" ~ "attendances (type 5)",
                                           pod == "op_outpatients" ~ "attendances",
                                           TRUE ~ measure)
                ) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(covid_adjustment = sum(value)) |>
  dplyr::ungroup()

# combine all soc
soc <- dplyr::bind_rows(soc_ip, soc_ae, soc_op) |>
  dplyr::left_join(baseline_adjustment) |>
  dplyr::left_join(covid_adjustment) |>
  dplyr::mutate(adj_baseline = rowSums(pick(baseline, baseline_adjustment,covid_adjustment), na.rm = TRUE)) |>
  dplyr::mutate_if(is.numeric, dplyr::coalesce,0)

# CAGR calculation
  soc <- soc |>
  dplyr::mutate(cagr = (principal / (adj_baseline)) ^ (1/fc_period_soc) - 1,
                cagr = dplyr::na_if(cagr, Inf))
# }


#### OBC figures from validation report ---
# Sample code to get the outputs from the default tab of the download spreadsheet
obc_ip <- get_baseline_and_projections(obc_scenario)|>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod,measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal),
                   p90 = sum(upr_ci)) |>
  dplyr::ungroup()


# Code to get A&E and SDEC from default tab
obc_ae <- get_baseline_and_projections(obc_scenario)|>
  dplyr::filter(measure=="ambulance" | measure == "walk-in") |>
  filter_sites_conditionally(site_codes$aae) |>
  dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                       pod == "aae_type-05" ~ "sdec"),
                measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                           pod == "sdec" ~ "attendances (type 5)")) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal),
                   p90 = sum(upr_ci)) |>
  dplyr::ungroup()

# code to get OP activity from defaults tab
obc_op <- get_baseline_and_projections(obc_scenario)|>
  dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
  filter_sites_conditionally(site_codes$op) |>
  dplyr::mutate(pod = "op_outpatients", measure = "attendances") |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal),
                   p90 = sum(upr_ci)) |>
  dplyr::ungroup()


# code to get delivery activity from delivery_episode_in_spell tab
obc_deliv <- obc_scenario[["results"]][["delivery_episode_in_spell"]]|>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::mutate(pod = "delivery") |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal),
                   p90 = sum(upr_ci)) |>
  dplyr::ungroup()

# need baseline_adjustment from step_counts
baseline_adjustment <- get_stepcounts(obc_scenario) |>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  dplyr::filter(change_factor == "baseline_adjustment") |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(baseline_adjustment = sum(value)) |>
  dplyr::ungroup()

# need covid_adjustment from step_counts
covid_adjustment <- get_stepcounts(obc_scenario) |>
  dplyr::filter(change_factor == "covid_adjustment") |>
  dplyr::mutate(measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                           pod == "sdec" ~ "attendances (type 5)",
                                           TRUE ~ measure),
                pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                       pod == "aae_type-05" ~ "sdec",
                                       stringr::str_starts(pod, "op_") ~ "op_outpatients",
                                       TRUE ~ pod)
  ) |>
  dplyr::group_by(pod, measure) |>
  dplyr::summarise(covid_adjustment = sum(value)) |>
  dplyr::ungroup()


# combine obc
obc <- dplyr::bind_rows(obc_ip, obc_deliv, obc_ae, obc_op) |>
  dplyr::left_join(baseline_adjustment) |>
  dplyr::left_join(covid_adjustment) |>
  dplyr::mutate(adj_baseline = rowSums(pick(baseline, baseline_adjustment,covid_adjustment), na.rm = TRUE)) |>
  dplyr::mutate_if(is.numeric, dplyr::coalesce,0) |>
  dplyr::mutate(cagr =(principal / (adj_baseline)) ^ (1/fc_period_obc) - 1,
                cagr = dplyr::na_if(cagr, Inf))

soc_obc_data <- dplyr::full_join(soc, obc,
                            by = dplyr::join_by(pod, measure),
                            suffix = c(".soc", ".obc")) |>

  dplyr::mutate(obc_pp_var = principal.obc - principal.soc,
                obc_p90_var = p90 - principal.soc) |>

  dplyr::mutate(heading = dplyr::case_when(
    startsWith(pod, "ip_elective_admission") ~ paste("Elective inpatient", measure),
    startsWith(pod, "ip_elective_daycase") ~ paste("Daycase", measure),
    startsWith(pod, "ip_regular_day_attender") ~ paste("Regular Day Attender", measure),
    startsWith(pod, "ip_non-elective") ~ paste("Non-elective", measure),
    startsWith(pod, "ip_maternity") ~ paste("Maternity", measure),
    startsWith(pod, "delivery") ~ paste("Delivery", measure),
    startsWith(pod, "op_") ~ paste("Outpatient", measure),
    startsWith(pod, "ae") ~ paste("A&E", measure),
    startsWith(pod, "sdec") ~ paste("SDEC", measure)),

    sort = dplyr::case_match(
      heading,
      "Non-elective admissions" ~ 1,
      "Non-elective beddays" ~ 2,
      "Daycase admissions" ~ 3,
      "Regular Day Attender admissions" ~ 4,
      "Elective inpatient admissions" ~ 5,
      "Elective inpatient beddays" ~ 6,
      "Maternity admissions" ~ 7,
      "Maternity beddays" ~ 8,
      "Delivery admissions" ~ 9,
      "Delivery beddays" ~ 10,
      "A&E arrivals (type 1 & 3)" ~ 11,
      "SDEC attendances (type 5)" ~ 12,
      "Outpatient attendances" ~ 13)
  ) |>
  dplyr::filter(!is.na(sort))

soc_obc_data

}
