
get_tpma_data <- function(r_trust){

  df_act <- r_trust[["params"]][["activity_avoidance"]] |>
    tibble::enframe(name = "activity_type", value = "int") |>
    tidyr::unnest_longer(int, indices_to = "strategy") |>
    # Unnest the measures level
    tidyr::unnest_longer(int, indices_to = "measure")  |>
    # Unnest the intervals and create low/high columns
    tidyr::unnest_wider(int, names_sep = "_")  |>
    dplyr::rename(high = int_1, low = int_2)  |>
    dplyr::mutate(high = round(100 * (1 - high), 0),
                  low = round(100 * (1 - low), 0))  |>
    # remove entries with no mitigation
    dplyr::filter(low != 0 & high != 0) |>
    dplyr::mutate(range = paste0(low, "-", high,"%"),
                  type = "activity_avoidance") |>
    dplyr::select(type, activity_type, strategy, range)

  df_eff <- r_trust[["params"]][["efficiencies"]] |>
    tibble::enframe(name = "activity_type", value = "int") |>
    tidyr::unnest_longer(int, indices_to = "strategy") |>
    # Unnest the measures level
    tidyr::unnest_longer(int, indices_to = "measure")  |>
    dplyr::filter(measure == "interval") |>
    # Unnest the intervals and create low/high columns
    tidyr::unnest_wider(int, names_sep = "_")  |>
    dplyr::rename(high = int_1, low = int_2)  |>
    dplyr::mutate(high = round(100 * (1 - high), 0),
                  low = round(100 * (1 - low), 0)) |>
    # remove entries with no mitigation
    dplyr::filter(low != 0 & high != 0) |>
    dplyr::mutate(range = paste0(low, "-", high,"%"),
                  type = "efficiencies") |>
    dplyr::select(type, activity_type, strategy, range)

  df <- dplyr::bind_rows(df_act, df_eff)
}
