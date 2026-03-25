
get_bespoke_ecdf_values <- function(soc_scenario, obc_scenario, site_codes){

  # Generate bespoke table of values derived from ECDF curves that show
  # 'validation' results with a point to
  # indicate the principal from the 'final' data.
  #
  # See https://github.com/The-Strategy-Unit/nhp_output_reports/issues/37
  # See https://github.com/The-Strategy-Unit/nhp_output_reports/issues/40

  # Set up ----

  r_primary <- obc_scenario
  r_secondary <- soc_scenario

  # Bespoke functions ----

  # Bespoke version of mod_model_results_distribution_ecdf_plot() which just gets 2 important values
  mod_model_results_distribution_ecdf_plot_perc_diff <- function(
    data_primary,
    principal_secondary# NEW: to add NDG2 principal point to plot

  ) {


    b <- data_primary$baseline[[1]]
    p <- data_primary$principal[[1]]

    ecdf_fn <- stats::ecdf(data_primary[["value"]])

    # Calculate y value for principal x value (helps find nearest y-axis % value)
    x_vals <- sort(data_primary[["value"]])
    y_vals <- sort(ecdf_fn(data_primary[["value"]]))

    # Percentage (y-axis value) for primary principal value (x-axis value)
    principal_diffs <- abs(p - x_vals) # nearest x in ECDF to the principal
    min_principal_diff_i <- which(principal_diffs == min(principal_diffs))[1]
    p_pcnt <- y_vals[min_principal_diff_i]

    # NEW: percentage for secondary principal value
    principal_secondary_diffs <- abs(principal_secondary - x_vals)
    min_principal_secondary_diff_i <- which(
      principal_secondary_diffs == min(principal_secondary_diffs)
    )[1]
    p_secondary_pcnt <- y_vals[min_principal_secondary_diff_i]

    output_val = (p_pcnt - p_secondary_pcnt)*100

    return(output_val)

  }



  # Get data needed for a single example

  # activity_type = "inpatients"
  # pod = "ip_non-elective_admission"
  # measure = "admissions"

  get_perc_diffs <- function(data_primary, data_secondary, site_codes, activity_type, pod, measure)
  {
    activity_type_short <-
      switch(
        activity_type,
        "inpatients" = "ip",
        "outpatients" = "op",
        "aae" = "aae"
      )
    site_codes <- site_codes[[activity_type_short]]

    aggregated_data_primary <- data_primary |>
      get_model_run_distribution(pod, measure, site_codes) |>
      require_rows()

    aggregated_data_secondary <- data_secondary |>
      get_model_run_distribution(pod, measure, site_codes) |>
      require_rows()

    principal_secondary <- aggregated_data_secondary |>
      dplyr::distinct(principal) |>
      dplyr::pull()

    value <- mod_model_results_distribution_ecdf_plot_perc_diff(
      data_primary=aggregated_data_primary,
      principal_secondary # NEW: to add NDG2 principal point to plot
    )

    return(value)

  }



  # All combinations needed for task in this issue:
  #   https://github.com/The-Strategy-Unit/nhp_output_reports/issues/40
  # (but not deliveries becuase that's not available in the outputs app, and not
  # aae_type-05 becuase SDEC didn't exist in the model when the 'final' scenario
  # was run.
  atmpo_combos <- tibble::tribble(
    ~activity_type , ~pod                                          , ~measure                  ,
    "inpatients"   , "ip_non-elective_admission"                   , "admissions"              ,
    "inpatients"   , "ip_non-elective_admission"                   , "beddays"                 ,
    "inpatients"   , "ip_elective_daycase"                         , "admissions"              ,
    "inpatients"   , "ip_elective_admission"                       , "admissions"              ,
    "inpatients"   , "ip_elective_admission"                       , "beddays"                 ,
    "inpatients"   , "ip_maternity_admission"                      , "admissions"              ,
    "inpatients"   , "ip_maternity_admission"                      , "beddays"                 ,
    "aae"          , c("aae_type-01", "aae_type-03")               , c("walk-in", "ambulance") ,
    "outpatients"  , c("op_first", "op_follow-up", "op_procedure") , "attendances"
  )

  ecdf_values_names <- atmpo_combos |>
    dplyr::mutate(
      pod = purrr::map_chr(pod, \(x) paste(x, collapse = "-")),
      measure = purrr::map_chr(measure, \(x) paste(x, collapse = "-")),
      dplyr::across(c(pod, measure), \(x) stringr::str_replace_all(x, "_", "-")),
      value_name = paste(activity_type, pod, measure, sep = "_")
    ) |>
    dplyr::pull(value_name)

  ecdf_values <- purrr::pmap(
    list(
      atmpo_combos$activity_type,
      atmpo_combos$pod,
      atmpo_combos$measure
    ),
    \(at, p, m) {
      get_perc_diffs(
        data_primary = r_primary,
        data_secondary = r_secondary,
        site_codes,
        activity_type = at,
        pod = p,
        measure = m
      )
    }
  ) |>
    purrr::set_names(ecdf_values_names)

  table_of_names <- tibble::enframe(ecdf_values) |> tidyr::unnest_longer("value")

}
