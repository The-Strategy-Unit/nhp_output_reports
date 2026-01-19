# Generate bespoke ECDF curves that show 'validation' results with a point to
# indicate the principal from the 'final' data.
#
# See https://github.com/The-Strategy-Unit/nhp_output_reports/issues/37
# See https://github.com/The-Strategy-Unit/nhp_output_reports/issues/40

# Set up ----

purrr::walk(list.files("R", ".R$", , TRUE, TRUE), source)
scheme_code <- "XYZ" # change to scheme of interest

result_sets <- get_nhp_result_sets()

run_stages <- list(
  primary = "addendum_report_ndg2",
  secondary = "final_report_ndg2"
)

meta <- get_run_metadata(scheme_code, result_sets, run_stages)
site_codes <- get_sites(scheme_code)

primary_file <- dplyr::pull(meta$metadata_primary, file)
secondary_file <- dplyr::pull(meta$metadata_secondary, file)

r_primary <- get_nhp_results(file = primary_file)
r_secondary <- get_nhp_results(file = secondary_file)

# Bespoke functions ----

# Bespoke version of mod_model_results_distribution_ecdf_plot()
mod_model_results_distribution_ecdf_plot <- function(
  data_primary,
  principal_secondary, # NEW: to add NDG2 principal point to plot
  show_origin
) {
  b <- data_primary$baseline[[1]]
  p <- data_primary$principal[[1]]

  ecdf_fn <- stats::ecdf(data_primary[["value"]])

  # Calculate x values for y-axis quantiles
  probs_pcnts <- c(0.1, 0.9)
  x_quantiles <- stats::quantile(ecdf_fn, probs = probs_pcnts)

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

  min_x <- min(b, min(data_primary[["value"]]))
  min_x <- dplyr::if_else(show_origin, 0, min_x)

  line_guides <- tibble::tibble(
    x_start = c(rep(min_x, 3), x_quantiles, p),
    x_end = rep(c(x_quantiles, p), 2),
    y_start = c(probs_pcnts, p_pcnt, rep(0, 3)),
    y_end = rep(c(probs_pcnts, p_pcnt), 2),
    colour = "cornflowerblue"
  )

  lines_n <- nrow(line_guides)
  line_guides[c(lines_n, lines_n / 2), "colour"] <- "red"

  primary_plot <- data_primary |>
    require_rows() |>
    ggplot2::ggplot() +
    suppressWarnings(
      ggplot2::geom_point(
        ggplot2::aes(
          x_vals,
          y_vals,
          text = glue::glue(
            "Percentage: {scales::percent(y_vals, accuracy = 1)}\n",
            "Value: {scales::comma(x_vals, accuracy = 1)}"
          )
        ),
        alpha = 0.01,
        size = 0.01
      )
    ) +
    ggplot2::geom_step(ggplot2::aes(x_vals, y_vals)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$x_start,
        y = .data$y_start,
        xend = .data$x_end,
        yend = .data$y_end
      ),
      data = line_guides,
      linetype = "dashed",
      colour = line_guides[["colour"]]
    ) +
    ggplot2::geom_vline(xintercept = b, colour = "dimgrey") +
    ggplot2::ylab("Percentage of model runs") +
    ggplot2::expand_limits(x = ifelse(show_origin, 0, b)) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_extended(9),
      labels = scales::comma,
      expand = c(0.002, 0),
      limits = c(min_x, NA)
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(seq(0, 1, 0.1)),
      labels = scales::percent,
      expand = c(0, 0)
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())

  # NEW: add point showing principal for secondary scenario
  primary_plot +
    ggplot2::annotate(
      "point",
      x = principal_secondary,
      y = p_secondary_pcnt,
      shape = 1,
      size = 5,
      colour = "red"
    )
}

# Bespoke version of plot_activity_distributions(). Plots only the ECDF, not the
# beeswarm. Shows point on the chart relating to placement of NDG2 ('secondary')
# principal value.
plot_ecdf <- function(
  data_primary,
  data_secondary, # NEW: pass in secondary to add NDG2 principal point to plot
  site_codes,
  activity_type,
  pod,
  measure
) {
  # Convert activity type to abbreviated form and filter for the set of site
  # codes specific to this activity type
  activity_type_short <-
    switch(
      activity_type,
      "inpatients" = "ip",
      "outpatients" = "op",
      "aae" = "aae"
    )
  site_codes <- site_codes[[activity_type_short]]

  # selected_measure <- c(activity_type, pod, measure)

  aggregated_data_primary <- data_primary |>
    # mod_model_results_distribution_get_data(selected_measure, site_codes) |>
    get_model_run_distribution(pod, measure, site_codes) |>
    require_rows()

  principal_secondary <- data_secondary |>
    # mod_model_results_distribution_get_data(selected_measure, site_codes) |>
    get_model_run_distribution(pod, measure, site_codes) |>
    dplyr::distinct(principal) |>
    dplyr::pull()

  mod_model_results_distribution_ecdf_plot(
    aggregated_data_primary,
    principal_secondary,
    show_origin = FALSE
  )
}

# Produce plots ----

# Single example
plot_ecdf(
  data_primary = r_primary,
  data_secondary = r_secondary,
  site_codes,
  activity_type = "inpatients",
  pod = "ip_non-elective_admission",
  measure = "admissions"
)

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

ecdf_plot_names <- atmpo_combos |>
  dplyr::mutate(
    pod = purrr::map_chr(pod, \(x) paste(x, collapse = "-")),
    measure = purrr::map_chr(measure, \(x) paste(x, collapse = "-")),
    dplyr::across(c(pod, measure), \(x) stringr::str_replace_all(x, "_", "-")),
    plot_name = paste(activity_type, pod, measure, sep = "_")
  ) |>
  dplyr::pull(plot_name)

ecdf_plots <- purrr::pmap(
  list(
    atmpo_combos$activity_type,
    atmpo_combos$pod,
    atmpo_combos$measure
  ),
  \(at, p, m) {
    plot_ecdf(
      data_primary = r_primary,
      data_secondary = r_secondary,
      site_codes,
      activity_type = at,
      pod = p,
      measure = m
    )
  }
) |>
  purrr::set_names(ecdf_plot_names)
