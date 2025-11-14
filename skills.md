# Skills

This repository is for an R package that generates standardized short chain fatty acid (SCFA) reports from raw data using a Quarto template.

High-level goals:

- Wrap existing SCFA analysis scripts into a reusable R package: `scfaReporter`.
- Use a Quarto `.qmd` report (currently `SCFA_report_template.qmd`) as a parameterised reporting template.
- Support normalization (e.g. mg/kg material, mg/µL DNA), plots by group, and ANOVA / linear mixed models with Tukey all-pairs post-hoc, as well as no statistical testing.

The assistant should use the skills defined below to edit files and suggest commands, keeping the package `R CMD check` clean and tests passing.

---

## Skill: scaffold_scfa_package

Goal:
- Create a minimal, working R package skeleton for the SCFA reporter.

When to use:
- At the very beginning of the project, in an empty directory.

Inputs:
- Package name (for example `scfaReporter`).
- Author name.
- License choice (MIT is fine).

Steps:
1. In a new directory named after the package, start an R session.
2. Install development helpers: usethis, devtools, roxygen2, testthat, pkgdown, codemetar, lintr, styler, precommit, rmarkdown, quarto.
3. Use `usethis::create_package(".")` to create the package.
4. Initialize Git and add a basic license, README, and NEWS.
5. Enable roxygen2 with markdown and testthat (3rd edition).
6. Add pkgdown site support.
7. Add GitHub Actions for multi-OS R CMD check and pkgdown deployment.
8. Enable pre-commit hooks and generate `codemeta.json`.
9. Ensure `DESCRIPTION`, `NAMESPACE`, `R/`, `tests/testthat/`, `.github/workflows/`, and pkgdown configuration exist and are valid.

Output:
- A standard, empty R package ready to receive SCFA-specific code and templates.

---

## Skill: setup_scfa_package_structure (NEW - extracted from scaffold)

Goal: Create minimal valid package structure

Steps:
1. Run `usethis::create_package(".")`
2. Verify: `DESCRIPTION` and `NAMESPACE` exist
3. Run: `devtools::document()`
4. Verify: Package loads with `devtools::load_all()`

Checkpoint: `devtools::check()` shows only NOTE about missing functions

---

## Skill: organize_existing_scfa_code

Goal:
- Move the SCFA analysis scripts found in the qmd file into (1) `scripts/` and (2) structured, documented R functions in `R/`.

When to use:
- After the package skeleton exists and the raw SCFA scripts are present in the repo (under `scripts/`).

Inputs:
- Existing scripts inside qmd chunks:
  - `load data`
  - `normalize data`
  - `prepare the plots and perform statistics`
  - `export the normalised data into an excel (csv) file`

Steps:
1. Create the following R source files in the `R/` directory:
   - `io_scfa.R` for reading raw SCFA data and manifests.
   - `normalize_scfa.R` for normalization to mg/kg material or mg/µL DNA.
   - `models_scfa.R` for ANOVA or linear mixed models plus Tukey post-hoc.
   - `plots_scfa.R` for SCFA plots by group and analyte.
   - `pipeline_scfa.R` for a single “orchestration” function.
2. Refactor the user’s existing scripts into these files, defining a small public API such as:
   - `read_scfa_raw(path, ...)`
   - `read_scfa_manifest(path, ...)`
   - `normalize_scfa(df, manifest, basis)`
   - `fit_scfa_models(df, model, random)`
   - `plot_scfa(df, ...)`
   - `run_scfa_pipeline(scfa_path, manifest_path, target_unit, basis, model, random)`

    Example function signature for `normalize_scfa.R`:
```r
   #' Normalize SCFA data
   #' @param df Data frame with raw SCFA measurements
   #' @param manifest Data frame with sample metadata
   #' @param basis Character: "mg_per_kg", "mg_per_uL_DNA", or "as_is"
   #' @return Normalized data frame with added columns
   #' @export
   normalize_scfa <- function(df, manifest, basis = "as_is") {
     # Implementation
   }
```
3. Keep internal helpers unexported; only the public API functions should be exported.
4. For each exported function, add complete roxygen2 documentation:
   - A short title and description.
   - Parameters (`@param` entries).
   - Return value (`@return`).
   - At least one minimal example (`@examples`).
   - An export tag (`@export`).
5. Ensure required imports (such as readr, dplyr, ggplot2, emmeans, lmerTest) are declared in `DESCRIPTION` and used via namespace-safe calls.
6. Regenerate documentation and load the package to confirm it builds and loads without errors.
7. Add minimal tests for the most important functions (for example, unit conversion and model fitting) in `tests/testthat/`.

### Refactoring Strategy:
1. Extract pure functions first (no side effects)
2. Separate I/O operations from transformations
3. Use consistent naming: `verb_noun()` pattern
4. Keep functions < 50 lines; split if longer
5. Use pipeable functions (data as first argument)

Output:
- All SCFA logic lives as reusable, documented R functions in the `R/` directory with a clear, limited public API.

---

## Skill: wrap_scfa_qmd_as_template

Goal:
- Convert the existing SCFA report file `SCFA_report_template.qmd` into a reusable package template that calls the package API instead of raw scripts.

When to use:
- After the SCFA functions in `R/` exist and can be called programmatically.

Inputs:
- The existing Quarto report file: `SCFA_report_template.qmd`.

Steps:
1. Create the directory structure for an R Markdown / Quarto template:
   - `inst/rmarkdown/templates/scfa-report/`
   - `inst/rmarkdown/templates/scfa-report/template.yaml`
   - `inst/rmarkdown/templates/scfa-report/skeleton/skeleton.qmd`
2. Create `template.yaml` with the following content:
```yaml
   name: SCFA Report
   description: >
     Standardized short chain fatty acid analysis report
   create_dir: true
```
3. Move or copy `SCFA_report_template.qmd` into `skeleton/skeleton.qmd`.
4. Edit `template.yaml` so it defines a template named “SCFA Report” with a short description and `create_dir: true`.
5. In `skeleton.qmd`, keep the report structure and formatting but:
   - Add a `params` section in the YAML header for inputs such as:
     - `project_id`
     - `scfa_path`
     - `manifest_path`
     - `target_unit` (for example “mg/L” or “mmol/L”)
     - `basis` (for example “mg_per_L”, “mg_per_kg”, “mg_per_uL_DNA”)
     - `model` (“anova” or “lmm”)
     - `random` (for example `~ 1|subject` when LMM is used)
   - Replace inline data processing code with a call to the package’s pipeline function `run_scfa_pipeline(...)`.
   - Use the returned objects (data, plots, statistics) in the rest of the report.
6. Remove hard-coded paths and `getwd()` assumptions; rely on `params` for all input file locations.
6. Ensure the template can be instantiated via `rmarkdown::draft(...)` with `template = "scfa-report"` and `package = "scfaReporter"`.
8. Add parameter validation in skeleton.qmd:
```{r setup}
# Validate required parameters
required_params  0) {
  stop("Missing required parameters: ", paste(missing, collapse = ", "))
}
```


Output:
- A package-embedded SCFA report template that can be reused by specifying parameters, and that relies only on the package API.

---

## Skill: test_scfa_template_rendering

Goal:
- Verify the Quarto template renders correctly with various parameter combinations

Steps:
1. Create `tests/testthat/test-template.R`
2. Test rendering with minimal params
3. Test each normalization basis
4. Test both ANOVA and LMM models
5. Verify output contains expected sections
6. Check that plots are generated

Validation:
- Template renders without errors for all param combinations
- Output HTML/PDF contains all expected elements

---

## Skill: add_demo_scfa_data_and_tests

Goal:
- Provide a small synthetic SCFA dataset and manifest plus tests that validate the pipeline (units, normalization, models, Tukey).

When to use:
- After the main SCFA functions and template exist and basic behaviour is working.

Inputs:
- Desired analytes and groups (for example three analytes and two groups).

Steps:
1. Create a script in `data-raw/` (for example `make_demo_scfa_data.R`) that:
   - Simulates a small SCFA dataset with columns such as `sample_id`, `analyte`, `value`, and `unit`.
   - Creates a matching manifest with `sample_id`, `group`, and optional fields like `subject`, `mass_g`, `dna_ul`, `extraction_volume_ml`, `dilution_factor`.
   - Uses `usethis::use_data()` to save the simulated objects into the `data/` directory.
2. Add a short roxygen2 documentation block in an R file (for example `data_doc.R`) describing the demo data object and its columns.
3. Add tests under `tests/testthat/` to check that:
   - Unit conversions (for example mmol/L to mg/L and back) are internally consistent within a tolerance.
   - The pipeline function can run end-to-end on the demo data without error.
   - The output contains a plot and a statistics table with Tukey all-pairs results.
4. Run documentation, tests, and a full package check.
5. Fix any failing tests, missing imports, or check notes.

Output:
- Demo SCFA data shipped with the package and tests that validate core functionality.

---

## Skill: recover_from_failed_scfa_skill

Goal:
- Diagnose and recover from failures during package development without losing progress

When to use:
- When any other skill fails partway through
- When `R CMD check` suddenly starts failing
- When the package becomes unloadable
- Before attempting a risky refactoring

Inputs:
- Description of what failed (error message, skill name, step number)
- Current state of package (does it load? do tests pass?)

Steps:

### 1. Assess the Damage
```r
# Try to load the package
devtools::load_all()

# Check what's broken
devtools::check(error_on = "never")

# Run tests to see what fails
devtools::test()
```

### 2. Choose Recovery Strategy

**Strategy A: Partial Rollback (Recommended First)**

Use when only recent changes are problematic
```bash
# See what changed
git status
git diff

# Discard specific files
git checkout -- path/to/problematic/file.R

# Rebuild documentation
Rscript -e "devtools::document()"
```

**Strategy B: Full Rollback to Last Working State**

Use when multiple files are affected or state is unclear
```bash
# Reset all uncommitted changes
git reset --hard HEAD

# Or go back to a specific commit
git log --oneline  # Find last working commit
git checkout <commit-hash>

# Verify package works
Rscript -e "devtools::load_all(); devtools::test()"
```

**Strategy C: Isolate and Fix**

Use when you want to keep most changes but fix specific issues
```r
# Isolate failing tests
devtools::test_file("tests/testthat/test-normalize.R")

# Check specific function
devtools::load_all()
normalize_scfa(demo_scfa_data, demo_manifest, "mg_per_kg")

# Lint specific file
lintr::lint("R/normalize_scfa.R")
```

### 3. Common Issues and Fixes

**Issue: NAMESPACE conflicts**
```r
# Regenerate NAMESPACE
devtools::document()
devtools::load_all()
```

**Issue: Missing dependencies**
```r
# Add to DESCRIPTION
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
```

**Issue: Data files not found**
```r
# Rebuild data
source("data-raw/make_demo_scfa_data.R")

# Verify data exists
devtools::load_all()
data(package = "scfaReporter")
```

**Issue: Template not found**
```bash
# Reinstall package to update inst/ contents
Rscript -e "devtools::install()"

# Verify template
Rscript -e "rmarkdown::draft('test', template = 'scfa-report', package = 'scfaReporter')"
```

**Issue: Quarto render fails**
```r
# Test template in isolation
quarto::quarto_render("inst/rmarkdown/templates/scfa-report/skeleton/skeleton.qmd")

# Check params are valid
params <- list(
  scfa_path = "path/to/data.csv",
  manifest_path = "path/to/manifest.csv",
  basis = "mg_per_kg"
)
```

### 4. Prevent Future Issues

**Create a checkpoint before risky changes:**
```bash
# Commit working state
git add -A
git commit -m "Checkpoint: before refactoring normalize_scfa"

# Create a backup branch
git branch backup-before-normalization
```

**Use incremental testing:**
```r
# After each function, test immediately
devtools::load_all()
devtools::test()
devtools::check()
```

### 5. Document What Went Wrong

Add to `docs/troubleshooting.md`:
```markdown
## [Date] - Issue with normalize_scfa

**Problem**: Function failed when basis = "mg_per_uL_DNA"
**Cause**: Missing validation for NA values in manifest$dna_ul
**Fix**: Added stopifnot validation
**Commit**: abc123f
```

Success Criteria:
- Package loads without errors: `devtools::load_all()`
- All tests pass: `devtools::test()`
- Check is clean: `devtools::check()` returns 0 errors, 0 warnings, 0 notes
- Git history is clean (no uncommitted cruft)

Common Issues:
- **Can't load package**: Try `devtools::document()` then `devtools::load_all()`
- **Old functions still exist**: Restart R session and reload
- **Changes not taking effect**: Reinstall with `devtools::install()`

Output:
- Package restored to working state
- Documentation of what failed and how it was fixed
- Optional: New tests added to prevent regression

---

## Skill: prepare_scfa_package_for_release

Goal:
- Make the SCFA reporter package ready for internal or external use.

When to use:
- After the package builds, tests, and the template all work.

Inputs:
- Target version number (for example `0.1.0`).
- Any known limitations to document.

Steps:
1. Update `DESCRIPTION` with a clear Title, Description, Version, Authors, and Imports.
2. Update the README to show:
   - Installation instructions.
   - A minimal usage example that runs the pipeline on demo data and knits the SCFA report template.
3. Update `NEWS.md` with a section for the new version summarizing changes.
4. Run a full local package check and build the pkgdown site.
5. Confirm that continuous integration (GitHub Actions) is green.
6. Tag the version in Git and push the tag.

Output:
- A versioned, documented SCFA reporter R package with a working template, tests, and CI.

## Success Criteria:
- `R CMD check` passes with 0 errors, 0 warnings, 0 notes
- `devtools::test()` shows all tests passing
- Package can be installed with `devtools::install()

## Common Issues:
- If NAMESPACE conflicts arise, run `devtools::document()` to regenerate
- If tests fail due to missing data, ensure `data-raw/` scripts have run
- If template not found, rebuild package with `devtools::install()`

## Validation:
After each major step, include verification commands:

```r
# Check package structure
devtools::check()

# Verify template exists
rmarkdown::draft("test", template = "scfa-report", package = "scfaReporter")

# Run demo pipeline
scfaReporter::run_scfa_pipeline(...)
```
