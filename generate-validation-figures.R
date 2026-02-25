### code to create tables for validation report:
## comparison of projections
## comparison of upper CI with principal projection
## comparison of estimated mitigated activity
purrr::walk(list.files("R", ".R$", , TRUE, TRUE), source)

scheme_code="XYZ" # add scheme_code for the scenario here to replace XYZ
# If the scheme has site codes already recorded or if all sites are required then set site_codes=NULL, otherwise set sites manually
site_codes = NULL
if (is.null(site_codes)) site_codes <- get_sites(scheme_code)
# site_codes = list( # change each element (each can be NULL to mean 'all')
#   ip  = "R0A66",
#   op  = "R0A66",
#   aae = "R0A66"
# )

result_sets = get_nhp_result_sets()

final_report_ndg1 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="final_report_ndg1")

final_report_ndg2 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="final_report_ndg2")

# we dont have any validation scenarios yet so just temporarily using addendum report scenarios so the code runs
validation_report_ndg2 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="validation_report_ndg2")
validation_report_ndg3 <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="validation_report_ndg3")
opening_date_scenario <- result_sets |>
  dplyr::filter(dataset==scheme_code) |>
  dplyr::filter(run_stage=="validation_report_ndg2_opening")

selected_results_list <- list(final_report_ndg1,
                              final_report_ndg2,
                              validation_report_ndg2,
                              validation_report_ndg3,
                              opening_date_scenario)


# fudge the secondary scenario as the primary scenario . Since secondary
# scenario is neither available for the special runs, nor needed, but we want to
# re-use code that assumes it exists then just ignore the secondary scenario


get_final_run_metadata_special <- function(scheme_code, selected_result_set) {

  scheme_results <- selected_result_set |> dplyr::filter(dataset == scheme_code)

  metadata_secondary <- dplyr::filter(scheme_results)
  metadata_primary <- dplyr::filter(scheme_results)

  dplyr::lst(metadata_secondary, metadata_primary)

}

meta <- purrr::map(
  selected_results_list,
  \(result) get_final_run_metadata_special(scheme_code, result)
)

r_final_report_ndg1 <- meta[[1]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _)

r_final_report_ndg2 <- meta[[2]]$metadata_primary |>
  dplyr::pull(file) |>
  get_nhp_results(file = _) #SOC

r_validation_report_ndg2 <- meta[[3]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _) #OBC

r_validation_report_ndg3 <- meta[[4]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _)

r_opening_date_scenario <- meta[[5]]$metadata_primary |>
  dplyr::pull(file) |> get_nhp_results(file = _)

# in CAGR calc, assumes this raises to power of forecast period? Need to account for difference if using opening scenario
# initially use 18 as in sample data calcs.
# time in years from baseline (23/24) to horizon (41/42 for usual)

get_years <- function(scenario){
  years_to_forecast <- as.numeric(
    scenario[["params"]][["end_year"]] - scenario[["params"]][["start_year"]]
  )
}

fc_period_soc <- get_years(r_final_report_ndg2)
fc_period_obc <- get_years(r_validation_report_ndg2)

# get the soc obc data
soc_obc <- get_soc_obc(r_final_report_ndg2, r_validation_report_ndg2, site_codes)
