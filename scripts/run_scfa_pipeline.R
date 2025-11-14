# Example script that orchestrates the SCFA workflow with the package API.
# Update the file paths to match your project before running.

library(scfaReporter)

# Paths to your inputs -----------------------------------------------------
chromatogram_dir <- "chromatograms"
manifest_path <- "P-00UH univ saxet.xlsx"

# Analysis configuration ---------------------------------------------------
analytes <- default_scfa_analytes()

results <- run_scfa_pipeline(
  scfa_dir = chromatogram_dir,
  manifest_path = manifest_path,
  analytes = analytes,
  normalization_basis = "mmol_per_L_plasma",
  normalization_multiplier = 7,
  stats = "lmer",
  lmer_formula = value ~ Group * Time + (1 | Subject_ID),
  x_col = "Group",
  facet_col = "Time",
  y_label = "mmol SCFA / L plasma"
)

# Persist outputs ----------------------------------------------------------
utils::write.csv(
  results$normalized[, c("MBI.Sample.ID", analytes)],
  file = "scfa_concentration_normalized.csv",
  row.names = FALSE
)

utils::write.csv(results$summary, file = "scfa_summary.csv", row.names = FALSE)
