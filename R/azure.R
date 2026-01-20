#' Unzip, Read and Parse an NHP Results File
#' @param container_results Name of a blob_container/storage_container object
#'     that stores results files.
#' @param file Character. The path to a file in the named `container`.
#' @details Assumes you've connected to the container that holds NHP results.
#' @return A nested list.
#' @noRd
get_nhp_results <- function(
  container_results = Sys.getenv("AZ_STORAGE_CONTAINER_RESULTS"),
  file
) {
  container <- get_container(container_name = container_results)

  temp_file <- withr::local_tempfile()
  AzureStor::download_blob(container, file, temp_file)

  readBin(temp_file, raw(), n = file.size(temp_file)) |>
    jsonlite::parse_gzjson_raw(simplifyVector = FALSE) |>
    parse_results() # applies patch logic dependent on app_version in params
}

#' Connect to an Azure Container
#' @param tenant Character. The tenant ID.
#' @param app_id Character. The app ID.
#' @param ep_uri Character. The endpoint URI.
#' @param container_name Character. The container name. Use `Sys.getenv()` with
#'     `"AZ_STORAGE_CONTAINER_RESULTS"` or `"AZ_STORAGE_CONTAINER_RESULTS"`.
#' @details All arguments default to environmental variables stored in your
#'     .Renviron file. Note that you'll be routed automatically to the browser
#'     for authentication if you don't have a cached token already.
#' @return A blob_container/storage_container object.
#' @noRd
get_container <- function(
  tenant = Sys.getenv("AZ_TENANT_ID"),
  app_id = Sys.getenv("AZ_APP_ID"),
  ep_uri = Sys.getenv("AZ_STORAGE_EP"),
  container_name
) {
  # if the app_id variable is empty, we assume that this is running on an Azure VM,
  # and then we will use Managed Identities for authentication.
  token <- if (app_id != "") {
    AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = Sys.getenv("AZ_TENANT_ID"),
      app = app_id,
      auth_type = "device_code"
    )
  } else {
    AzureAuth::get_managed_token("https://storage.azure.com/") |>
      AzureAuth::extract_jwt()
  }

  ep_uri |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)
}

#' Extract Baseline and Projections from Results
#' @param scheme_results List. Results for the given scheme.
#' @noRd
get_baseline_and_projections <- function(scheme_results) {
  scheme_results[["results"]][["default"]] |>
    dplyr::group_by(measure, pod, sitetret) |>
    dplyr::summarise(
      baseline = sum(baseline),
      principal = sum(principal),
      lwr_ci = sum(lwr_ci),
      upr_ci = sum(upr_ci)
    )
}

#' Extract Step Counts from Results
#' @param scheme_results List. Results for the given scheme.
#' @noRd
get_stepcounts <- function(scheme_results) {
  scheme_results[["results"]][["step_counts"]]
}

#' Extract Length of Stay Group from Results
#' @param scheme_results List. Results for the given scheme.
#' @noRd
get_losgroup <- function(scheme_results) {
  los_group_is_null <- is.null(scheme_results[["results"]][["los_group"]])

  if (los_group_is_null) {
    # tretspef+los_group renamed from tretspef_raw+los_group in v4.0
    scheme_results <- scheme_results[["results"]][["tretspef+los_group"]]
  } else {
    scheme_results <- scheme_results[["results"]][["los_group"]]
  }

  scheme_results
}
