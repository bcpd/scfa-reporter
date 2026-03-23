make_adjusted_fixture <- function() {
  scfa_data <- tibble::tibble(
    sample = paste0("S", 1:8),
    acetic_acid = c(0.6, 0.8, 0.7, 0.9, 1.4, 1.5, 1.6, 1.7)
  )

  manifest <- tibble::tibble(
    mbi_sample_id = paste0("S", 1:8),
    disease = factor(rep(c("N", "Y"), each = 4), levels = c("N", "Y")),
    age = c(34, 36, 39, 41, 52, 54, 57, 60),
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    subject_id = paste0("P", 1:8)
  )

  list(scfa_data = scfa_data, manifest = manifest)
}

test_that("run_scfa_pipeline supports adjusted lm models with custom group columns", {
  fixture <- make_adjusted_fixture()
  results <- run_scfa_pipeline(
    scfa_data = fixture$scfa_data,
    manifest = fixture$manifest,
    analytes = "acetic_acid",
    stats = "lm",
    lm_formula = log1p(value) ~ disease + age + sex,
    group_col = "disease",
    time_col = NULL,
    x_col = NULL,
    facet_col = NULL
  )

  expect_true("disease" %in% names(results$summary))
  expect_false("time" %in% names(results$summary))
  expect_s3_class(results$plots[["acetic_acid"]]$facet, "FacetNull")
  expect_true("anova" %in% names(results$models[["acetic_acid"]]))
  expect_true("group_contrasts" %in% names(results$models[["acetic_acid"]]))
  expect_true(any(results$models[["acetic_acid"]]$anova$Term == "disease"))
})

test_that("render_scfa_report renders an adjusted lm report without time faceting", {
  skip_if_not_installed("quarto")
  skip_if_not(nzchar(unname(Sys.which("quarto"))))
  skip_if_not_installed("kableExtra")

  fixture <- make_adjusted_fixture()
  output_dir <- tempdir()
  output_file <- file.path(output_dir, "lm_report.html")

  render_scfa_report(
    output_file = basename(output_file),
    output_dir = output_dir,
    project_id = "P-00U3",
    sample_type = "Plasma",
    scfa_data = fixture$scfa_data,
    manifest = fixture$manifest,
    analytes = "acetic_acid",
    normalization_multiplier = 1,
    normalization_basis = "as_is",
    stats = "lm",
    lm_formula = log1p(value) ~ disease + age + sex,
    group_col = "disease",
    time_col = NULL,
    x_col = NULL,
    facet_col = NULL,
    export_csv = FALSE
  )

  expect_true(file.exists(output_file))
})
