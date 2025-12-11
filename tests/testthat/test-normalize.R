test_that("normalize_scfa applies multiplier and metadata", {
  raw <- tibble::tibble(
    mbi_sample_id = c("S1", "S2"),
    acetic_acid = c(1, 2)
  )
  normalized <- normalize_scfa(
    raw,
    analytes = "acetic_acid",
    basis = "mmol_per_L_plasma",
    multiplier = 7
  )
  expect_equal(normalized$acetic_acid, c(7, 14))
  expect_equal(attr(normalized, "basis"), "mmol_per_L_plasma")
  expect_equal(attr(normalized, "multiplier"), 7)
})

test_that("scfa_long_format reshapes data correctly", {
  data <- tibble::tibble(
    mbi_sample_id = c("S1", "S2"),
    subject_id = c("P1", "P2"),
    group = c("A", "B"),
    time = c("T0", "T0"),
    acetic_acid = c(1, 2),
    propionic_acid = c(3, 4)
  )
  long <- scfa_long_format(
    data,
    analytes = c("acetic_acid", "propionic_acid")
  )
  expect_equal(nrow(long), 4)
  expect_setequal(unique(long$analyte), c("acetic_acid", "propionic_acid"))
})

test_that("fit_scfa_models handles lmer failures gracefully", {
  data <- tibble::tibble(
    mbi_sample_id = c("S1", "S2"),
    subject_id = c("P1", "P1"),
    group = c("A", "A"),
    time = c("T0", "T1"),
    value = c(1, 2),
    analyte = factor("acetic_acid")
  )
  output <- fit_scfa_models(
    data,
    stats = "lmer",
    lmer_formula = value ~ group + (1 | subject_id)
  )
  expect_named(output, "acetic_acid")
  expect_true("coefficients" %in% names(output[[1]]))
})
