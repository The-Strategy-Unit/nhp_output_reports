#' Concatenate Scheme Name and Code
#' @param scheme_code Character. A focus scheme's three-character ODS code.
#' @param lookup_path Character. The file path to the CSV lookup of scheme names
#'     and codes.
#' @param as_filestring Logical. Express as a string with punctuation removed,
#'    hyphen-delimited and in lowercase? Used to build filepath.
#' @return Character string.
#' @noRd
make_scheme_name <- function(
  scheme_code,
  lookup_path = "data/scheme-lookup.csv",
  as_filestring = FALSE
) {
  scheme_string <- readr::read_csv(lookup_path, col_types = "c") |>
    dplyr::filter(scheme == scheme_code) |>
    dplyr::mutate(
      hosp_site_scheme = glue::glue("{hosp_site} ({scheme})"),
      .keep = "none"
    ) |>
    dplyr::pull()

  if (as_filestring) {
    scheme_string <- scheme_string |>
      stringr::str_remove_all("[:punct:]") |>
      stringr::str_to_lower() |>
      stringr::str_replace_all(" ", "-")
  }

  scheme_string
}

#' Check Run-Stage and Scenario-File Inputs
#' @param scheme_code Character.
#' @param run_stages List.
#' @param scenario_files List.
#' @return Nothing. Possibly prints an error.
#' @noRd
check_scenario_inputs <- function(scheme_code, run_stages, scenario_files) {
  # Make sure one of run_stages or scenario_files is provided
  if (
    is.null(run_stages) &
      is.null(scenario_files) |
      !is.null(run_stages) & !is.null(scenario_files)
  ) {
    cli::cli_abort(c(
      "!" = "Provide one of {.arg run_stages} or {.arg scenario_files}.",
      "i" = "Set the other to NULL."
    ))
  }

  # Check that scenario_files are for the provided scheme_code by checking for
  # the scheme code in the filepath (excluding the scenario name)
  if (!is.null(scenario_files)) {
    code_matches_path <- scenario_files |>
      purrr::map(
        \(path) {
          path |>
            stringr::str_remove(basename(path)) |>
            stringr::str_detect(scheme_code)
        }
      ) |>
      unlist() |>
      all()

    if (!code_matches_path) {
      cli::cli_abort(c(
        "!" = "{.arg scenario_files} must contain paths for the given {.arg scheme_code}.",
        "i" = "Results for scheme 'XYZ' would be on a path like 'example/example/XYZ/example.json.gz'."
      ))
    }
  }
}

#' Add Metadata Values to Logfile
#' @param scheme_code Character.
#' @param scheme_lookup_path Character.
#' @param datetime Character.
#' @param report_type Character.
#' @return Nothing. Prints to already-opened logfile.
#' @noRd
log_meta <- function(scheme_code, scheme_lookup_path, datetime, report_type) {
  schemes <- scheme_lookup_path |> readr::read_csv(col_types = "c")
  scheme_name <- schemes |>
    dplyr::filter(scheme == scheme_code) |>
    dplyr::pull(hosp_site)

  logr::log_print(glue::glue(
    "* Scheme: {scheme_code} ({scheme_name})\n",
    "* Execution datetime: {datetime}\n",
    "* Report type: {report_type}"
  ))
}

#' Add Scenario Files to Logfile
#' @param primary_file Character.
#' @param secondary_file Character.
#' @param return Nothing. Prints to already-opened logfile.
#' @noRd
log_scenario_files <- function(primary_file, secondary_file) {
  logr::log_print(glue::glue(
    "* Scenario files:\n",
    "- primary variant: {primary_file}\n",
    "- secondary variant: {secondary_file}\n"
  ))
}

#' Add Sites Values to Logfile
#' @param site_codes List of named vectors.
#' @return Nothing. Prints to already-opened logfile.
#' @noRd
log_sites <- function(site_codes) {
  sites_readable <- purrr::map(
    site_codes,
    \(sites) {
      dplyr::if_else(
        is.null(sites),
        "all",
        paste(sites, collapse = ", ")
      )
    }
  )

  logr::log_print(glue::glue(
    "* Sites:\n",
    "- Inpatients:  {sites_readable[['ip']]}\n",
    "- Outpatients: {sites_readable[['op']]}\n",
    "- A&E:         {sites_readable[['aae']]}"
  ))
}
