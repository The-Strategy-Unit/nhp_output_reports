

# pull sites included in a specific scenario/stage run
# Returns a list(aae, ip, op) where NULL means "all sites".

# assumes the aggregated file path has already been identified

get_sites <- function(agg_results_path) {

  run_row <- scheme_runs |>
    dplyr::filter(aggregated_results_path == agg_results_path)

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


