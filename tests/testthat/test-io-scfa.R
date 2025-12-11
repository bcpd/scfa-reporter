test_that("read_scfa_raw cleans chromatogram column names", {
  withr::with_tempdir({
    file.create(c("sample1.xls", "sample2.xls"))
    fake_chrom <- data.frame(
      `Peak Name` = c("Acetic Acid", "Propionic Acid"),
      Amount = c("1.5", "2.5"),
      stringsAsFactors = FALSE
    )

    result <- read_scfa_raw(
      dir = ".",
      sheet = "Integration",
      skip = 39,
      sample_col = "sample_id",
      read_fun = function(path, sheet, skip, progress) fake_chrom
    )

    expect_equal(result$sample_id, c("sample1", "sample2"))
    expect_true(all(c("acetic_acid", "propionic_acid") %in% names(result)))
  })
})
