# Generate scheme-level content and insert into the final-report template

# Prepare workspace
purrr::walk(list.files("R", ".R$", , TRUE, TRUE), source)
library(logr, quietly = TRUE)

# Generate a report folder and populate it with content
populate_template(
  scheme_code = "XYZ", # change to scheme of interest
  site_codes = NULL, # NULL will fetch site codes from Azure
  result_sets = read_az_table(), # fetch results metadata from Azure
  run_stages = list(
    # provide run stage labels from Azure metadata...
    secondary = "final_report_ndg1", # used as a comparator to the primary
    primary = "final_report_ndg2" # the main data source for the report
  ),
  scenario_files = NULL, # ...or provide arbitrary results filepaths
  template_path = NULL, # NULL to read from SharePoint
  report_type = "final" # or 'addendum'
)
