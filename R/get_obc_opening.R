get_soc_obc_open <- function (soc_scenario, obc_scenario, obc_open_scenario, site_codes){

      #### SOC figures from final report ---

    soc_ip <- get_baseline_and_projections(soc_scenario)|>
      dplyr::filter(measure=="admissions" | measure == "beddays") |>
      filter_sites_conditionally(site_codes$ip) |>
      dplyr::group_by(pod,measure) |>
      dplyr::summarise(principal = sum(principal)) |>
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
      dplyr::summarise(principal = sum(principal)) |>
      dplyr::ungroup()

    # code to get OP activity from defaults tab
    soc_op <- get_baseline_and_projections(soc_scenario)|>
      dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
      filter_sites_conditionally(site_codes$op) |>
      dplyr::mutate(pod = "op_outpatients") |>
      dplyr::group_by(pod, measure) |>
      dplyr::summarise(principal = sum(principal)) |>
      dplyr::ungroup()


    # combine all soc

    # need to add in Covid adjustment to CAGR calc but only if baseline is 2019/20
    soc <- dplyr::bind_rows(soc_ip, soc_ae, soc_op)


    #### OBC figures from validation report ---
    # Sample code to get the outputs from the default tab of the download spreadsheet
    obc_ip <- get_baseline_and_projections(obc_scenario)|>
      dplyr::filter(measure=="admissions" | measure == "beddays") |>
      filter_sites_conditionally(site_codes$ip) |>
      dplyr::group_by(pod,measure) |>
      dplyr::summarise(principal = sum(principal),
                       p90.obc = sum(upr_ci)) |>
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
      dplyr::summarise(principal = sum(principal),
                       p90.obc = sum(upr_ci)) |>
      dplyr::ungroup()

    # code to get OP activity from defaults tab
    obc_op <- get_baseline_and_projections(obc_scenario)|>
      dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
      filter_sites_conditionally(site_codes$op) |>
      dplyr::mutate(pod = "op_outpatients") |>
      dplyr::group_by(pod, measure) |>
      dplyr::summarise(principal = sum(principal),
                       p90.obc = sum(upr_ci)) |>
      dplyr::ungroup()


    # code to get delivery activity from delivery_episode_in_spell tab
    obc_deliv <- obc_scenario[["results"]][["delivery_episode_in_spell"]]|>
      dplyr::filter(measure=="admissions" | measure == "beddays") |>
      filter_sites_conditionally(site_codes$ip) |>
      dplyr::mutate(pod = "delivery") |>
      dplyr::group_by(pod, measure) |>
      dplyr::summarise(principal = sum(principal),
                       p90.obc = sum(upr_ci)) |>
      dplyr::ungroup()


    # combine soc obc
    obc <- dplyr::bind_rows(obc_ip, obc_deliv, obc_ae, obc_op)

    soc_obc_data <- dplyr::full_join(soc, obc,
                                     by = dplyr::join_by(pod, measure),
                                     suffix = c(".soc", ".obc"))


  #### OBC opening year figures from validation report ---

  obc_ip <- get_baseline_and_projections(obc_open_scenario)|>
    dplyr::filter(measure=="admissions" | measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::group_by(pod,measure) |>
    dplyr::summarise(principal.open = sum(principal),
                     p90.open = sum(upr_ci)) |>
    dplyr::ungroup()


  # Code to get A&E and SDEC from default tab
  obc_ae <- get_baseline_and_projections(obc_open_scenario)|>
    dplyr::filter(measure=="ambulance" | measure == "walk-in") |>
    filter_sites_conditionally(site_codes$aae) |>
    dplyr::mutate(pod = dplyr::case_when(pod %in% c("aae_type-01", "aae_type-03") ~ "ae",
                                         pod == "aae_type-05" ~ "sdec"),
                  measure = dplyr::case_when(pod == "ae" ~ "arrivals (type 1 & 3)",
                                             pod == "sdec" ~ "attendances (type 5)")) |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(principal.open = sum(principal)) |>
    dplyr::ungroup()

  # code to get OP activity from defaults tab
  obc_op <- get_baseline_and_projections(obc_open_scenario)|>
    dplyr::filter(measure=="attendances" | measure == "tele_attendances") |>
    filter_sites_conditionally(site_codes$op) |>
    dplyr::mutate(pod = "op_outpatients") |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(principal.open = sum(principal),
                     p90.open = sum(upr_ci)) |>
    dplyr::ungroup()

  # code to get delivery activity from delivery_episode_in_spell tab
  obc_deliv <- obc_open_scenario[["results"]][["delivery_episode_in_spell"]]|>
    dplyr::filter(measure=="admissions" | measure == "beddays") |>
    filter_sites_conditionally(site_codes$ip) |>
    dplyr::mutate(pod = "delivery") |>
    dplyr::group_by(pod, measure) |>
    dplyr::summarise(principal.open = sum(principal),
                     p90.open = sum(upr_ci)) |>
    dplyr::ungroup()



  # tidy obc_open
  obc_open <- dplyr::bind_rows(obc_ip, obc_deliv, obc_ae, obc_op)

  soc_obc_open <-
    dplyr::full_join(soc_obc_data, obc_open,
                     by = dplyr::join_by(pod, measure))

  soc_obc_open_data <- soc_obc_open |>

    ### for OP, change description of attendances to F2F attendances,
    # and create new var of OP attendances which is sum of F2F and tele

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
        "Outpatient attendances" ~ 14,
        "Outpatient tele_attendances" ~ 15)
    ) |>

    dplyr::mutate(heading = dplyr::case_when(
      heading == "Outpatient attendances" ~ "Outpatient attendances",
      heading == "Outpatient tele_attendances" ~ "Outpatient tele-attendances",
      TRUE ~ heading
    )) |>
    dplyr::filter(!is.na(sort))

  op_att <- soc_obc_open_data |>
    dplyr::filter(startsWith(pod, "op_")) |>
    dplyr::summarise(
      dplyr::across(where(is.numeric), sum),
      dplyr::across(where(~ !is.numeric(.x)), ~ NA_character_)
    ) |>
    dplyr::select(all_of(names(soc_obc_open_data))) |>
    dplyr::mutate(
      heading = "Outpatient attendances (all)",
      sort = 13
    )

  soc_obc_open_data <- dplyr::bind_rows(soc_obc_open_data, op_att) |>
    dplyr::select(-c(pod, measure)) |>
    dplyr::relocate(heading) |>
    dplyr::arrange(sort) |>
    dplyr::select(-sort)

  soc_obc_open_data

  writexl::write_xlsx(soc_obc_open_data, path = paste0(scheme_code, "-projection comparison.xlsx"))

}
