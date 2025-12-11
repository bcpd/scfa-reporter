#' Default SCFA analyte names used in the Microbiome Insights workflow
#'
#' @return Character vector with ordered analyte names.
#' @export
default_scfa_analytes <- function() {
  janitor::make_clean_names(
    c(
      "Acetic Acid",
      "Propionic Acid",
      "Isobutyric Acid",
      "Butyric Acid",
      "Isovaleric Acid",
      "Valeric Acid",
      "Hexanoic Acid",
      "Heptanoic Acid",
      "Isocaproic acid"
    )
  )
}

clean_formula_vars <- function(formula) {
  if (is.null(formula) || !inherits(formula, "formula")) {
    return(formula)
  }
  vars <- all.vars(formula)
  replacements <- janitor::make_clean_names(vars)
  names(replacements) <- vars
  formula_str <- paste(deparse(formula), collapse = " ")
  for (i in seq_along(vars)) {
    formula_str <- gsub(
      paste0("\\b", vars[[i]], "\\b"),
      replacements[[i]],
      formula_str
    )
  }
  stats::as.formula(formula_str)
}

#' High-level helper that runs the SCFA processing pipeline
#'
#' @param scfa_dir Directory with chromatogram `.xls` files. Ignored when
#'   `scfa_data` is supplied.
#' @param manifest_path Path to the manifest workbook. Ignored when `manifest`
#'   is supplied.
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
#' @param scfa_data Optional tibble that mimics the output of
#'   [read_scfa_raw()]. When supplied, `scfa_dir` is ignored.
#' @param manifest Optional tibble returned by [read_scfa_manifest()]. When
#'   supplied, `manifest_path` is ignored.
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
                              lmer_formula = value ~ group * time + (1 | subject_id),
                              aov_formula = NULL,
                              x_col = "group",
                              facet_col = "time",
                              y_label = "SCFA concentration",
                              scfa_data = NULL,
                              manifest = NULL) {
  stats <- match.arg(stats)
  analytes <- janitor::make_clean_names(analytes)
  x_col <- janitor::make_clean_names(x_col)
  if (!is.null(facet_col)) {
    facet_col <- janitor::make_clean_names(facet_col)
  }
  aov_formula <- clean_formula_vars(aov_formula)
  lmer_formula <- clean_formula_vars(lmer_formula)

  if (is.null(scfa_data)) {
    if (missing(scfa_dir)) {
      stop("Provide either 'scfa_dir' or 'scfa_data'.", call. = FALSE)
    }
    raw_data <- read_scfa_raw(dir = scfa_dir, subset_pattern = subset_pattern)
  } else {
    if (!is.data.frame(scfa_data)) {
      stop("'scfa_data' must be a data frame/tibble.", call. = FALSE)
    }
    raw_data <- janitor::clean_names(scfa_data)
  }

  if (is.null(manifest)) {
    if (missing(manifest_path)) {
      stop("Provide either 'manifest_path' or 'manifest'.", call. = FALSE)
    }
    manifest_tbl <- read_scfa_manifest(manifest_path)
  } else {
    if (!is.data.frame(manifest)) {
      stop("'manifest' must be a data frame/tibble.", call. = FALSE)
    }
    manifest_tbl <- janitor::clean_names(manifest)
  }

  merged <- merge_scfa_data(raw_data, manifest_tbl)
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
    manifest = manifest_tbl,
    merged = merged,
    normalized = normalized,
    long = scfa_long,
    summary = summary_tbl,
    models = model_list,
    plots = plots
  )
}
