## A miscellany of functions to allow working with results from version 6 and below.
# BUT get_nhp_results() does not work with parquet files from v4 or below.
# These functions are required for the new generate_validation_figures script
# (still being developed)


# get path for results. Defaults to aggregated_results_path, if NA
# then pulls json filepath.

# use this for named run stage
get_run_stage_path <- function(stage_name){

  result_sets |>
    dplyr::filter(dataset == scheme_code) |>
    dplyr::filter(run_stage == stage_name) |>
    dplyr::mutate(path = dplyr::case_when(
      is.na(aggregated_results_path) ~ file,
      TRUE ~ aggregated_results_path
    )) |>
    dplyr::pull(path)

}

# use this for named scenario
get_scenario_path <- function(scenario_name){

  result_sets |>
    dplyr::filter(dataset == scheme_code) |>
    dplyr::filter(scenario == scenario_name) |>
    dplyr::mutate(path = dplyr::case_when(
      is.na(aggregated_results_path) ~ file,
      TRUE ~ aggregated_results_path
    )) |>
    dplyr::pull(path)

}

# pull sites included in a specific scenario/stage run
# Returns a list(aae, ip, op) where NULL means "all sites".

# assumes the aggregated file path has already been identified (e.g. get_run_stage_path())

get_sites <- function(agg_results_path) {

  run_row <- result_sets |>
    dplyr::filter(aggregated_results_path == agg_results_path|file==agg_results_path) # added this to allow for parquet and json scenario data

  sites_list <- run_row |>
    dplyr::select("sites_aae", "sites_ip", "sites_op") |>
    unlist() |>
    as.list()

  # Convert returned values into what's expected by this codebase
  sites_list |>
    purrr::set_names(\(x) stringr::str_remove(x, "sites_")) |> # 'ip' not 'sites_ip'
    purrr::map(\(x) stringr::str_split_1(x, ",")) |> # "X,Y" to c("X", "Y")
    purrr::map(\(x) if (identical(x, "ALL")) NULL else x) # NULL means all sites

 }

# new function to parse the results when in parquet, replicating the output of parse_results
# from get_nhp_results() for json files.
parse_az_results <- function(results, name) {
  if (name == "step_counts") {
    return(calculate_step_counts(results))
  }

  group_cols <- list(
    acuity                      = c("pod", "sitetret", "measure", "acuity", "model_run"),
    age                         = c("pod", "sitetret", "measure", "age", "model_run"),
    attendance_category         = c("pod", "sitetret", "measure", "attendance_category", "model_run"),
    avoided_activity            = c("pod", "sitetret", "measure", "sex", "age_group", "model_run"),
    default                     = c("pod", "sitetret", "measure", "model_run"),
    delivery_episode_in_spell   = c("pod", "sitetret", "measure", "model_run"),
    functional_areas            = c("measure", "functional_area", "sitetret", "model_run"),
    `sex+age_group`             = c("pod", "sitetret", "measure",  "sex", "age_group", "model_run"),
    `sex+tretspef_grouped`      = c("pod", "sitetret", "measure",  "sex", "tretspef_grouped", "model_run"),
    `tretspef+los_group`        = c("pod", "sitetret", "measure", "tretspef", "los_group", "model_run"),
    tretspef                    = c("pod", "sitetret", "measure", "tretspef", "model_run")
  )

  cols <- group_cols[[name]]
  if (is.null(cols)) stop("No column definition for analysis: ", name)

  return(calculate_wide_principal_stats(results, cols))
}


## following functions are copied/adapted from reskit functions

# this makes data wide, as opposed to reskit::calculate_principal_stats() which is long
calculate_wide_principal_stats <- function(results, cols) {

  id_cols <- setdiff(cols, "model_run")

  stat_cols <- c("mean", "median", "p10", "p90")

  results |>
    reskit:::check_single_row_groups(cols) |>
    dplyr::mutate(
      stage = dplyr::if_else(.data[["model_run"]] == 0, "baseline", "principal")
    ) |>
    dplyr::summarise(
      mean = mean(.data[["value"]]),
      median = stats::quantile(.data[["value"]], 0.5),
      p10 = stats::quantile(.data[["value"]], 0.1),
      p90 = stats::quantile(.data[["value"]], 0.9),
      .by = tidyselect::all_of(reskit:::swap_modelrun_for_stage(cols))
    )   |>

    tidyr::pivot_wider(
      names_from = "stage",
      values_from = c("mean", "median", "p10", "p90")
    ) |>
    dplyr::select(
      dplyr::all_of(id_cols),
      baseline  = mean_baseline,
      principal = mean_principal,
      median    = median_principal,
      lwr_ci    = p10_principal,
      upr_ci    = p90_principal
    )

}

calculate_step_counts <- function(results) {

  cols = c("pod", "sitetret", "change_factor", "strategy", "measure", "model_run")
  id_cols <- setdiff(cols, "model_run")

  results |>
    dplyr::filter_out(model_run == 0) |>
    reskit:::check_single_row_groups(cols) |>
    dplyr::mutate(
      stage = "principal" # helper to allow swap_modelrun_for_stage() function
    ) |>
    dplyr::summarise(
      value = mean(.data[["value"]]),
      .by = tidyselect::all_of(reskit:::swap_modelrun_for_stage(cols))
    )   |>

    dplyr::select(
      dplyr::all_of(id_cols),
      value
    )

}


