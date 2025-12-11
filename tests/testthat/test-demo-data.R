test_that("demo dataset 1 runs through ANOVA pipeline", {
  analytes <- c("acetic_acid", "propionic_acid", "butyric_acid")
  results <- run_scfa_pipeline(
    scfa_data = demo_scfa_dataset1,
    manifest = demo_manifest_dataset1,
    analytes = analytes,
    stats = "anova",
    aov_formula = value ~ group,
    y_label = "arbitrary units"
  )

  expect_true(all(analytes %in% names(results$plots)))
  expect_s3_class(results$plots[[1]], "ggplot")
  tukey <- results$models[["acetic_acid"]]$tukey
  expect_true(!is.null(tukey))
})

test_that("demo dataset 2 runs through multi-time ANOVA pipeline", {
  analytes <- c("acetic_acid", "propionic_acid", "butyric_acid")
  results <- run_scfa_pipeline(
    scfa_data = demo_scfa_dataset2,
    manifest = demo_manifest_dataset2,
    analytes = analytes,
    stats = "anova",
    aov_formula = value ~ group * time,
    y_label = "arbitrary units"
  )

  expect_s3_class(results$plots[["butyric_acid"]], "ggplot")
  tukey <- results$models[["butyric_acid"]]$tukey
  expect_true(!is.null(tukey))
})
