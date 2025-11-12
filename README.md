# nhp_output_reports

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## About

Auto-populate report templates with results from [the New Hospital Programme (NHP) demand model](https://connect.strategyunitwm.nhs.uk/nhp/project_information/).

Note that this repository continues development in the open from the archived [nhp_final_reports](https://github.com/The-Strategy-Unit/nhp_final_reports) private repository.

## Run the process

### Requirements

Before you begin, you must:

1. Clone this repo to your computer.
1. Add to the root of your clone an `.Renviron` file, which should be a populated version of the provided `.Renviron.example` template file.
This is needed for connecting to Azure and Sharepoint.
You can ask a member of the Data Science team to help populate these variables.
1. Check that the scenarios selected for reporting are present in the [tagged scenarios and site selections table](https://connect.strategyunitwm.nhs.uk/nhp/tagged_runs/) (though alternatively you can use the the `scenario_files` and `site_codes` arguments of `populate_template()` in `populate-report.R`).
1. Make sure you're connected to the VPN (you're likely to get a 'curl' error if not).

Below is a simplified directory tree of the required structure (some files/folders not shown for brevity).
Asterisks indicate that the content doesn't exist in the repository and must be added locally.

```
.
├── .Renviron [*]            # a copy of .Renviron.example with values added for each key
├── data/                    # model jsons, mitigators.json, golem-config.yml
│   ├── golem-config.yml     # for pod measures lookup, copied from nhp_outputs
│   ├── mitigators.json      # for mitigator code/name lookup, copied from nhp_outputs
│   ├── scheme-lookup.csv    # for scheme name/code lookup
│   └── tx-lookup.json       # for treatment specialty code lookup, copied from nhp_outputs
├── outputs/                 # files are generated into subfolders here
├── populate-report.R        # script to populate reports
├── R/                       # R functions (some scripts copied from named repos)
└── templates/               # Optional location for locally-stored templates
```

### Steps

Having completed the requirements, you should:

1. Run the code in `populate-report.R`, which:
    a. Creates a new folder in the form `outputs/YYYYMMDD-HHMMSS_scheme-name-code_final/` to store outputs.
    a. Fetches results data and site selections from Azure for the given `scheme_code`.
    a. Reads the report template from SharePoint into an {officer} rdocx-class object.
    a. Generates charts and tables as png files to the subfolder `figures/` and then inserts them into the rdocx object.
    a. Calculates all the in-text values, names them and adds them to the rdocx object as 'document properties'.
    a. Writes the rdocx object to a docx file.
    a. Generates an accompanying log file that records metadata about the run.
1. Open the docx file, select-all (<kbd>Ctrl</kbd> + <kbd>A</kbd>) and hit <kbd>F9</kbd> to refresh the fields (it's also possible, but tedious, to right-click each one and select 'update field'). This will pull in the custom document properties into the correct locations in the document. It may also ask if you want to update the table of contents, which is fine.
1. Check that the values and figures have been inserted as expected and quality-assure them.

See READMEs in `data/` and `R/` for further detail about those topics.

### Authentication

There are two points in the process that will require you to authenticate:

1. Accessing results data from Azure.
1. Downloading the report template from SharePoint.

The browser will open and ask you to authenticate with your Microsoft work account.
Once authenticated, you shouldn't need to do it again.
Instead, messages will print to the console when credentials are being checked.

You should ensure you're connected to the VPN for these steps to trigger correctly.

## How to insert content

This section explains the process of setting up and inserting values and figures (tables and charts) into the docx template.
The information here will only be needed when new content needs to be inserted into the document.

In short:

* for figures, we add 'target strings' (e.g. '[Insert Figure 8.2]') to the template and replace them with generated figures
* for values, we programmatically insert 'custom properties' to the docx and then insert them into document-property 'fields' (e.g. '**{** DOCPROPERTY item_01 **}**') that we've added to the document

### Template

Report templates live on SharePoint and are not owned by the Data Science team, though we have the responsibility to insert calculated values and figures (charts and tables).

To retrieve a template programmatically, you must ensure that the `SP_SU_SITE` (the name of the Strategy Unit's SharePoint site) and `SP_TEMPLATE_PATH` (the path to the template docx file on that SharePoint site) variables are completed in your `.Renviron` file.
If the template cannot be found by the code, check if the location of the file has changed on SharePoint.

As a backup option, it's also possible to provide a local version of the template.
Put it in `templates/` and provide the file path to the `template_path` argument of `populate_template()` in `populate-report.R`.

The process for inserting calculated values into the template differs from the process for inserting the figures, as explained in the following sections.

### Insert values

Due to limitations in Microsoft Word, [{officer} isn't consistent when find-and-replacing individual strings](https://ardata-fr.github.io/officeverse/officer-for-word.html#content-replacement).
Instead, the document should contain manually-inserted 'fields' at each insert location, which can be replaced with 'custom document properties'.

This is a bit like mail merge, but custom properties are stored in the metadata of the document, rather than being provided from an external source (e.g. a spreadsheet).

#### Custom properties

Word files have 'document properties' that store metadata information.
We can set up individual 'custom' properties and then overwrite them programmatically with our calculated values.

If the template needs a new custom property, then you must manually add them to the custom property list.
To view custom properties in Word, go to File > Info > 'Properties' dropdown > Advanced Properties > Custom.

To add a new custom property, add to the 'Name' box the unique name for the value in the form 'item_57' (where the item number matches the one in the `generate-values.R` script) and in the 'Value' box add a placeholder in the form '[Insert Value 57]'.
Click 'OK' to add the new property to the 'Properties' list.

If adding lots of properties at once, you could also do this process programmatically rather than manually.
Here's some illustrative code to set up 50 custom properties named 'item_01' etc, that use the placeholder value '[Insert Value 1]'.
This is similar to what was done to set up the template with custom properties in the first place.

``` r
# Add custom document properties to a docx template

# Read generic template (from local in this example)
path <- "templates/generic-template.docx"  # imaginary file
docx <- officer::read_docx(path)

# Set up properties and placeholders list(item_01 = "[item_01]")
item_names <- glue::glue("item_{stringr::str_pad(1:50, 2, pad = '0')}")
item_placeholders <- glue::glue("[Insert Value {stringr::str_remove(item_names, 'item_')}]")
custom_properties <- item_placeholders |> purrr::set_names(item_names)

# Add properties to template file
docx2 <- officer::set_doc_properties(docx, values = custom_properties)
print(docx2, target = "templates/generic-template-with-docprops.docx")
```

#### Fields

The custom properties are inserted into the document at specific, special locations called 'fields'.

You can insert a field at the cursor by pressing <kbd>Ctrl</kbd>+<kbd>F9</kbd>, which inserts a pair of curly brackets, then you type 'DOCPROPERTY item_01' inside it, where 'item_01 is the name of a custom property that contains a calculated value.
Then right-click the field and select 'Update Field'

It's quite ugly to look at '**{** DOCPROPERTY item_01 **}**' in the Word template, so we can toggle to seeing the placeholder value for the custom property.
So, if you right-click the field and select 'Toggle Field Codes', it will change from the curly-bracketed text to say '[Insert Value 01]' (which is what we typed in as the 'value' for this custom property).
This is a better visual cue for anyone reading the template compared to the raw 'DOCPROPERTY' string.

### Insert figures

Inserting figures works differently to inserting values. We don't have to set custom properties or fields.

Instead, the template should contain 'target strings' that identify the places where dynamically-generated figures (images and tables) can be inserted.
There's nothing special about these strings; they're just bits of text in the form '[Insert Figure 8.2]' (which mimics the same style as the placeholder custom-property values).
{officer} functions are used to locate these strings (`cursor_reach()`) and replace them with the associated image file (`body_add_img()`).

For reference, the insert locations for figures are as follows:

* Expat/repat parameter tables
  * Figure expat: table of expat parameter values (if selected)
  * Figure repat (local): table of repat parameter values (if selected)
  * Figure repat (nonlocal): table of repat parameter values (if selected)
* Principal change-factor bar charts
  * Figure 8.2: inpatient admissions, activity avoidance
  * Figure 8.3: inpatient bed days, activity avoidance
  * Figure 8.4: inpatient bed days, efficiencies
  * Figure 8.5: outpatient attendances, activity avoidance
  * Figure 8.6: A&E arrivals, activity avoidance
* Activity distribution plots
  * Figure 9.1: inpatient bed days (all PODs), beeswarm
  * Figure 9.2: inpatient bed days (all PODs), S-curve
  * Figure 9.6: outpatient attendances, beeswarm (note: attendance/tele-attendance measures combined, which is not possible in the outputs app)
  * Figure 9.7: outpatient attendances, S-curve (attendance/tele-attendance measures combined)
  * Figure 9.8: A&E arrivals, beeswarm (walk-in-/ambulance-arrivals measures combined)
  * Figure 9.9: A&E arrivals, S-curve (walk-in-/ambulance-arrivals measures combined)
* Summary table
  * Figure 9.3: inpatient bed days and admissions (split by POD)
* Waterfall
  * Figure 9.4: inpatient bed days (all PODs)
* Length-of-stay table
  * Figure 9.5: inpatient bed days (split by POD)
* Activity distribution plots (NDG variant 1)
  * Figure 9.10: inpatient bed days (all PODs), beeswarm
  * Figure 9.11: inpatient bed days (all PODs), S-curve
