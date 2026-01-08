# Goal: produce nhp_output_reports charts for:
#   1. Gynae (treatment specialties 502 and 503)
#   2. Paeds (ages 0 to 17)
#   3. Maternity (the POD)
#
# See https://github.com/The-Strategy-Unit/nhp_output_reports/issues/54
#
# General approach for gynae/paeds: load all functions, then overwrite certain
# data wrangling functions so that they read datasets needed for gynae/paeds
# plots/tables. This is controlled by setting a 'results_type' global option()
# to "gynae" or "paeds".

# Set up ----

# Load all existing functions, overwrite later with bespoke versions
purrr::walk(list.files("R", ".R$", , TRUE, TRUE), source)

# Read results/sites as per insert_into_template()
scheme_code <- "XYZ" # replace with scheme of interest
result_sets <- get_nhp_result_sets()
run_stages <- list(
  primary = "final_report_ndg2",
  secondary = "final_report_ndg3"
)
meta <- get_run_metadata(scheme_code, result_sets, run_stages)
site_codes <- get_sites(scheme_code)
primary_file <- dplyr::pull(meta$metadata_primary, file)
secondary_file <- dplyr::pull(meta$metadata_secondary, file)
r_primary <- get_nhp_results(file = primary_file)
r_secondary <- get_nhp_results(file = secondary_file)

# Helper functions ----

# Selects a dataset depending on the value of the "results_type" option
switch_results_data <- function(r) {
  switch(
    getOption("results_type", default = NA_character_),
    "gynae" = r[["results"]][["tretspef"]] |>
      dplyr::filter(tretspef %in% c("502", "503")),
    "paeds" = r[["results"]][["age"]] |>
      dplyr::filter(dplyr::between(age, 0, 17)),
    r[["results"]][["default"]] # original default behaviour
  )
}

# Generate beeswarms and S-curves ----

# Bespoke version of existing function
get_model_run_distribution <- function(r, pod, measure, site_codes) {
  filtered_results <- switch_results_data(r) |> # bespoke data switch
    dplyr::filter(
      .data$pod %in% .env$pod,
      .data$measure %in% .env$measure
    ) |>
    dplyr::select("sitetret", "baseline", "principal", "model_runs")

  if (nrow(filtered_results) == 0) {
    return(NULL)
  }

  filtered_results |>
    dplyr::mutate(
      dplyr::across(
        "model_runs",
        \(.x) purrr::map(.x, tibble::enframe, name = "model_run")
      )
    ) |>
    tidyr::unnest("model_runs") |>
    dplyr::inner_join(get_variants(r), by = "model_run") |>
    trust_site_aggregation(site_codes)
}

# Convenience function to generate plots and (un)set options
plot_activity_dist <- function(
  results = r_primary,
  sites = site_codes,
  results_type = NULL
) {
  options(results_type = results_type)
  on.exit(options(results_type = NULL))
  prepare_all_activity_distribution_plots(results, sites)
}

# Generate primary-scenario beeswarms (9.1, 9.6, 9.8), S-curves (9.2, 9.7, 9.9)
plots_activity_distribution <- plot_activity_dist() # overall
plots_activity_distribution_paeds <- plot_activity_dist(results_type = "paeds")
plots_activity_distribution_gynae <- plot_activity_dist(results_type = "gynae")

# Generate secondary-scenario beeswarm (9.10) and S-curve (9.11)
plots_activity_distribution <- plot_activity_dist(results = r_secondary) # overall
plots_activity_distribution_paeds <- plot_activity_dist(
  results = r_secondary,
  results_type = "paeds"
)
plots_activity_distribution_gynae <- plot_activity_dist(
  results = r_secondary,
  results_type = "gynae"
)

# Generate summary tables ----

# Bespoke version of existing function
get_principal_high_level <- function(r, measures, sites) {
  switch_results_data(r) |> # bespoke data switch
    dplyr::filter(.data$measure %in% measures) |>
    dplyr::select("pod", "sitetret", "baseline", "principal") |>
    dplyr::mutate(dplyr::across(
      "pod",
      ~ ifelse(
        stringr::str_starts(.x, "aae"),
        "aae",
        .x
      )
    )) |>
    dplyr::group_by(.data$pod, .data$sitetret) |>
    dplyr::summarise(dplyr::across(where(is.numeric), sum), .groups = "drop") |>
    trust_site_aggregation(sites)
}

# Convenience function to generate plots and (un)set options
make_summary_table <- function(
  results = r_primary,
  sites = site_codes[["ip"]],
  results_type = NULL
) {
  options(results_type = results_type)
  on.exit(options(results_type = NULL))
  mod_principal_summary_data(results, sites) |>
    dplyr::filter(activity_type == "Inpatient") |>
    mod_principal_summary_table()
}

# Generate summary table (9.3)
summary_table <- make_summary_table() # overall
summary_table_gynae <- make_summary_table(results_type = "gynae")
summary_table_paeds <- make_summary_table(results_type = "paeds")

# Ganerate LoS tables ----

# Bespoke version of existing function
mod_principal_summary_los_data <- function(r, sites, measure) {
  pods <- mod_principal_los_pods()

  has_tretspef_los <- !is.null(r$results[["tretspef+los_group"]])
  is_gynae <- getOption("results_type", default = "none") == "gynae"

  if (!has_tretspef_los) {
    if (is_gynae) {
      stop("Can't produce an output for gynae, tretspef+los_group is missing.")
    }
    los_data <- r$results[["los_group"]]
  }

  if (has_tretspef_los) {
    los_data <- r$results[["tretspef+los_group"]]
    if (is_gynae) {
      los_data <- los_data |> dplyr::filter(tretspef %in% c("502", "503"))
    }
    los_data <- los_data |> dplyr::select(-"tretspef")
  }

  summary_los <- los_data |>
    dplyr::filter(.data$measure == .env$measure) |>
    trust_site_aggregation(sites) |>
    dplyr::inner_join(pods, by = "pod") |>
    dplyr::mutate(
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::select(
      "pod_name",
      "los_group",
      "baseline",
      "principal",
      "change",
      "change_pcnt"
    ) |>
    dplyr::arrange("pod_name", "los_group")

  summary_los[order(summary_los$pod_name, summary_los$los_group), ]
}

# Convenience function to generate plots and (un)set options
make_summary_los_table <- function(
  results = r_primary,
  sites = site_codes[["ip"]],
  results_type = NULL
) {
  options(results_type = results_type)
  on.exit(options(results_type = NULL))
  mod_principal_summary_los_data(results, sites, "beddays") |>
    dplyr::mutate(
      pod_name = stringr::str_replace(pod_name, "Admission", "Bed Days")
    ) |>
    dplyr::arrange(pod_name, los_group) |>
    mod_principal_summary_los_table() |>
    gt::tab_options(table.align = "left")
}

# Generate LoS table (9.5)
make_summary_los_table() # overall
make_summary_los_table(results_type = "gynae")
