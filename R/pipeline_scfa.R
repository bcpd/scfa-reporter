#' Default SCFA analyte names used in the Microbiome Insights workflow
#'
#' @return Character vector with ordered analyte names.
#' @export
default_scfa_analytes <- function() {
  c(
    "Acetic.Acid",
    "Propionic.Acid",
    "Isobutyric.Acid",
    "Butyric.Acid",
    "Isovaleric.Acid",
    "Valeric.Acid",
    "Hexanoic.Acid",
    "Heptanoic.Acid",
    "Isocaproic.acid"
  )
}

#' High-level helper that runs the SCFA processing pipeline
#'
#' @param scfa_dir Directory with chromatogram `.xls` files.
#' @param manifest_path Path to the manifest workbook.
#' @param analytes Character vector with analyte column names. Defaults to
#'   [default_scfa_analytes()].
#' @param subset_pattern Optional regular expression used to subset chromatogram
#'   files.
#' @param normalization_basis Text label describing the normalization.
#' @param normalization_multiplier Numeric multiplier (defaults to `1`).
#' @param stats Statistical engine (`"lmer"`, `"anova"`, or `"none"`).
#' @param lmer_formula Formula passed to [lmerTest::lmer()] when
#'   `stats = "lmer"`.
#' @param aov_formula Formula passed to [stats::aov()] when `stats = "anova"`.
#' @param x_col Column for the x-axis in the plots.
#' @param facet_col Optional column used for facetting plots.
#' @param y_label Y-axis label for the plots.
#'
#' @return A named list that bundles raw data, manifest, normalized data, long
#'   format tibble, model objects, summary tables, and ggplot objects.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- run_scfa_pipeline(
#'   scfa_dir = "chromatograms",
#'   manifest_path = "P-00UH univ saxet.xlsx",
#'   normalization_multiplier = 7,
#'   stats = "lmer"
#' )
#' }
run_scfa_pipeline <- function(scfa_dir,
                              manifest_path,
                              analytes = default_scfa_analytes(),
                              subset_pattern = NULL,
                              normalization_basis = "mmol_per_L_plasma",
                              normalization_multiplier = 1,
                              stats = c("lmer", "anova", "none"),
                              lmer_formula = value ~ Group * Time + (1 | Subject_ID),
                              aov_formula = NULL,
                              x_col = "Group",
                              facet_col = "Time",
                              y_label = "SCFA concentration") {
  stats <- match.arg(stats)

  raw_data <- read_scfa_raw(dir = scfa_dir, subset_pattern = subset_pattern)
  manifest <- read_scfa_manifest(manifest_path)
  merged <- merge_scfa_data(raw_data, manifest)
  normalized <- normalize_scfa(
    merged,
    analytes = analytes,
    basis = normalization_basis,
    multiplier = normalization_multiplier
  )
  scfa_long <- scfa_long_format(normalized, analytes = analytes)
  summary_tbl <- scfa_summary(scfa_long)
  model_list <- fit_scfa_models(
    scfa_long,
    stats = stats,
    lmer_formula = lmer_formula,
    aov_formula = aov_formula
  )
  plots <- plot_scfa_panels(
    scfa_long,
    x_col = x_col,
    facet_col = facet_col,
    y_label = y_label
  )

  list(
    raw_data = raw_data,
    manifest = manifest,
    merged = merged,
    normalized = normalized,
    long = scfa_long,
    summary = summary_tbl,
    models = model_list,
    plots = plots
  )
}
