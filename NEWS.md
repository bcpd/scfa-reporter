# scfaReporter 0.1.0

* First collaborator-facing release.
* Adds documented demo datasets and manifests plus tests exercising
  `run_scfa_pipeline()` and Tukey comparisons.
* Introduces the Quarto SCFA report template (`inst/rmarkdown/templates/scfa-report`)
  with parameter validation and CSV export.
* README now shows GitHub installation, pipeline usage, and template rendering
  so collaborators can get started quickly.

# scfaReporter 0.0.0.9000

* Initial development snapshot with SCFA IO helpers, normalization pipeline,
  and Quarto-ready plotting/modeling utilities.
* Added simulated demo datasets (`demo_scfa_dataset1/2` plus manifests) for quick
  pipeline exercises, with documentation under `R/data_doc.R` and generation
  script `data-raw/make_demo_scfa_data.R`.
