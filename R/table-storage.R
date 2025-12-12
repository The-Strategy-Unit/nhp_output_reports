#' Read an Azure Table Storage Table into a Tibble
#' @param app_id Azure Active Directory application (client) ID. Defaults to
#'   `AZ_APP_ID`. If empty, Managed Identity authentication will be used (via
#'   [get_az_token]).
#' @param ep_uri The Azure Table Storage endpoint URI. Defaults to `AZ_TABLE_EP`
#'   and should include a trailing slash, e.g.:
#'   `"https://<storage-account>.table.core.windows.net/"`.
#' @param table_name The name of the Azure Table to query. Defaults to
#'   `AZ_TABLE_NAME`.
#' @return A tibble containing all table entities. Each entity is converted from
#'   JSON into a tibble and row-bound into a single data frame.
read_az_table <- function(
  app_id = Sys.getenv("AZ_APP_ID"),
  ep_uri = Sys.getenv("AZ_TABLE_EP"),
  table_name = Sys.getenv("AZ_TABLE_NAME")
) {
  token <- get_az_token(app_id)

  req <- httr2::request(glue::glue("{ep_uri}{table_name}")) |>
    httr2::req_auth_bearer_token(token$credentials$access_token) |>
    httr2::req_headers(
      `x-ms-version` = "2023-11-03",
      Accept = "application/json;odata=nometadata"
    )
  resp <- httr2::req_perform(req)
  entities <- httr2::resp_body_json(resp)

  entities[[1]] |> # response is contained in a list
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind() |>
    dplyr::rename(dataset = PartitionKey, file = results_file) |>
    dplyr::select(-c(RowKey, Timestamp))
}

#' Acquire an Azure Active Directory Token for Storage Authentication
#' @param app_id Azure Active Directory application (client) ID. Used when
#'   running the function locally. If empty (e.g. when deployed), the
#'   function uses Managed Identity authentication.
#' @return An AzureAuth token object (OAuth or Managed Identity), suitable for
#'   authenticating against Azure Storage services.
get_az_token <- function(app_id) {
  if (app_id != "") {
    AzureAuth::get_azure_token(
      resource = "https://storage.azure.com",
      tenant = "common",
      app = app_id,
      auth_type = "authorization_code",
      use_cache = TRUE # avoid browser-authorisation prompt
    )
  } else {
    AzureAuth::get_managed_token("https://storage.azure.com/")
  }
}
