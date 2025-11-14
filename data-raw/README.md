# data-raw

This folder holds scripts used to generate package data. The SCFA demo data
shipped with `scfaReporter` is produced by `make_demo_scfa_data.R` via
`usethis::use_data()`.

- `make_demo_scfa_data.R`: simulates two small datasets (3 analytes, groups A/B)
  plus accompanying manifests and saves them as `demo_scfa_dataset1`,
  `demo_manifest_dataset1`, `demo_scfa_dataset2`, and
  `demo_manifest_dataset2`.

To regenerate the data, run:

```r
source("data-raw/make_demo_scfa_data.R")
```

The resulting `.rda` files live in `data/` and are documented in `R/data_doc.R`.
