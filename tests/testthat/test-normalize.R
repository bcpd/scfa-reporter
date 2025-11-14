test_that("normalize_scfa applies multiplier and metadata", {
  raw <- tibble::tibble(
    MBI.Sample.ID = c("S1", "S2"),
    Acetic.Acid = c(1, 2)
  )
  normalized <- normalize_scfa(
    raw,
    analytes = "Acetic.Acid",
    basis = "mmol_per_L_plasma",
    multiplier = 7
  )
  expect_equal(normalized$Acetic.Acid, c(7, 14))
  expect_equal(attr(normalized, "basis"), "mmol_per_L_plasma")
  expect_equal(attr(normalized, "multiplier"), 7)
})

test_that("scfa_long_format reshapes data correctly", {
  data <- tibble::tibble(
    MBI.Sample.ID = c("S1", "S2"),
    Subject_ID = c("P1", "P2"),
    Group = c("A", "B"),
    Time = c("T0", "T0"),
    Acetic.Acid = c(1, 2),
    Propionic.Acid = c(3, 4)
  )
  long <- scfa_long_format(
    data,
    analytes = c("Acetic.Acid", "Propionic.Acid")
  )
  expect_equal(nrow(long), 4)
  expect_setequal(unique(long$Analyte), c("Acetic.Acid", "Propionic.Acid"))
})

test_that("fit_scfa_models handles lmer failures gracefully", {
  data <- tibble::tibble(
    MBI.Sample.ID = c("S1", "S2"),
    Subject_ID = c("P1", "P1"),
    Group = c("A", "A"),
    Time = c("T0", "T1"),
    value = c(1, 2),
    Analyte = factor("Acetic.Acid")
  )
  output <- fit_scfa_models(
    data,
    stats = "lmer",
    lmer_formula = value ~ Group + (1 | Subject_ID)
  )
  expect_named(output, "Acetic.Acid")
  expect_true("coefficients" %in% names(output[[1]]))
})
