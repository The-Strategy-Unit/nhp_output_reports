get_soc_obc <- function (soc_scenario, obc_scenario, site_codes){
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


# combine all soc

# need to add in Covid adjustment to CAGR calc but only if baseline is 2019/20
soc <- dplyr::bind_rows(soc_ip, soc_ae, soc_op) |>
  dplyr::mutate(cagr = (principal / baseline) ^ (1/fc_period_soc) - 1,
                cagr = dplyr::na_if(cagr, Inf))


#### OBC figures from validation report ---
# Sample code to get the outputs from the default tab of the download spreadsheet
obc_ip <- get_baseline_and_projections(obc_scenario)|>
  dplyr::filter(measure=="admissions" | measure == "beddays") |>
  filter_sites_conditionally(site_codes$ip) |>
  dplyr::group_by(pod,measure) |>
  dplyr::summarise(baseline = sum(baseline),
                   principal = sum(principal),
                   p90 = sum(upr_ci)) |>
  dplyr::filter(pod != "ip_regular_day_attender") |>
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




# combine obc
obc <- dplyr::bind_rows(obc_ip, obc_deliv, obc_ae, obc_op) |>
  dplyr::mutate(cagr =(principal / baseline) ^ (1/fc_period_obc) - 1,
                cagr = dplyr::na_if(cagr, Inf))

soc_obc <- dplyr::full_join(soc, obc,
                            by = dplyr::join_by(pod, measure),
                            suffix = c(".soc", ".obc")) |>

  dplyr::mutate(obc_pp_var = principal.obc - principal.soc,
                obc_p90_var = p90 - principal.soc) |>

  dplyr::mutate(heading = dplyr::case_when(
    startsWith(pod, "ip_elective_admission") ~ paste("Elective inpatient", measure),
    startsWith(pod, "ip_elective_daycase") ~ paste("Daycase", measure),
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
      "Elective inpatient admissions" ~ 4,
      "Elective inpatient beddays" ~ 5,
      "Maternity admissions" ~ 6,
      "Maternity beddays" ~ 7,
      "Delivery admissions" ~ 8,
      "Delivery beddays" ~ 9,
      "A&E arrivals (type 1 & 3)" ~ 10,
      "SDEC attendances (type 5)" ~ 11,
      "Outpatient attendances" ~ 12)
  ) |>
  dplyr::filter(!is.na(sort))


#### build table comparing SOC with OBC ---
tbl_soc_obc <- soc_obc |>
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
