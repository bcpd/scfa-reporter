#' Render the SCFA Quarto report
#'
#' @param output_file Name of the rendered report (defaults to
#'   `"scfa_report.html"`).
#' @param output_dir Directory where the report should be written. Defaults to
#'   the current working directory.
#' @inheritParams run_scfa_pipeline
#' @param project_id Project identifier printed in the report header.
#' @param sample_type Text describing the matrix (for the methods section).
#' @param export_csv Logical; when `TRUE` writes `scfa_concentration_normalized.csv`
#'   next to the rendered report.
#' @param ... Additional arguments forwarded to
#'   \code{quarto::quarto_render()}, e.g. `quiet = TRUE`.
#'
#' @return Invisibly returns the path to the rendered report.
#' @export
#'
#' @examples
#' \dontrun{
#' render_scfa_report(
#'   output_file = "demo1.html",
#'   scfa_data = demo_scfa_dataset1,
#'   manifest = demo_manifest_dataset1,
#'   analytes = c("Acetic.Acid", "Propionic.Acid", "Butyric.Acid"),
#'   stats = "anova",
#'   aov_formula = value ~ Group
#' )
#' }
render_scfa_report <- function(
  output_file = "scfa_report.html",
  output_dir = getwd(),
  project_id = "SCFA Project",
  sample_type = "Plasma",
  scfa_data = NULL,
  manifest = NULL,
  scfa_dir = NULL,
  manifest_path = NULL,
  analytes = default_scfa_analytes(),
  normalization_multiplier = 1,
  normalization_basis = "as_is",
  stats = c("lmer", "anova", "none"),
  lmer_formula = value ~ Group * Time + (1 | Subject_ID),
  aov_formula = NULL,
  x_col = "Group",
  facet_col = "Time",
  y_label = "SCFA concentration",
  export_csv = TRUE,
  ...
) {
  stats <- match.arg(stats)
  if (is.null(scfa_data) && is.null(scfa_dir)) {
    stop("Provide either 'scfa_data' or 'scfa_dir'.", call. = FALSE)
  }
  if (is.null(manifest) && is.null(manifest_path)) {
    stop("Provide either 'manifest' or 'manifest_path'.", call. = FALSE)
  }

  template <- system.file(
    "rmarkdown/templates/scfa-report/skeleton/skeleton.qmd",
    package = "scfaReporter"
  )
  if (!nzchar(template)) {
    stop("Template not found in the installed package.", call. = FALSE)
  }

  params <- list(
    project_id = project_id,
    sample_type = sample_type,
    scfa_dir = scfa_dir,
    manifest_path = manifest_path,
    analytes = analytes,
    normalization_multiplier = normalization_multiplier,
    normalization_basis = normalization_basis,
    stats = stats,
    lmer_formula = lmer_formula,
    aov_formula = aov_formula,
    x_col = x_col,
    facet_col = facet_col,
    y_label = y_label,
    export_csv = export_csv,
    scfa_data = scfa_data,
    manifest_override = manifest
  )

  quarto::quarto_render(
    input = template,
    execute_params = params,
    output_file = output_file,
    output_dir = output_dir,
    ...
  )
}
