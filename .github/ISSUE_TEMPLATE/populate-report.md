---
name: Populate output report
about: Track the steps for generating a new output report draft.
title: 'Populate report template: SCHEME NAME (SCHEME ODE)'
labels: must, populate-report
assignees: matt-dray
---

Replace instances of [NAME] below with the name of the person who will complete the task (use their GitHub @ handle).

After receiving a request in the Data Science team inbox for a new output report:

- [ ] Get from the model-relationship manager (MRM) the type of report to run ('final', 'refresh', etc); the scenario names for both non-demographic growth (NDG) variants and their creation datetimes; and the list of sites to filter by (may be separate site sets for inpatients, outpatients and A&E), if any [NAME]
- [ ] Create a new entity in the tagged-runs lookup (in Azure Table Storage) with the scenario's metadata and site details [NAME]
- [ ] Refresh [the 'tagged runs and sites' report](https://connect.strategyunitwm.nhs.uk/nhp/tagged_runs/) on Connect to check the previous step was completed correctly [NAME]
- [ ] Run off the report and populate the document fields with the custom properties ([see the README](https://github.com/The-Strategy-Unit/nhp_final_reports/blob/main/README.md) for instructions) [NAME]
- [ ] Check the calculated values [NAME]
- [ ] Check the generated figures against the outputs app—which you can launch from the [tagged runs and sites report](https://connect.strategyunitwm.nhs.uk/nhp/tagged_runs/nhp-tagged-runs.html) with the correct scenario loaded—remembering to use the app's site filter as required [NAME]
- [ ] Add the report to the relevant folder in SharePoint (under the `GROUP */` directory) [NAME]
- [ ] Alert the MRM that the populated report is ready (echo back the scenario names, their create datetimes and the specified sites to double-check the correct ones were used) [NAME]
