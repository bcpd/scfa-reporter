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

  if (inherits(aov_formula, "formula")) {
    aov_formula <- Reduce(paste, deparse(aov_formula))
  }
  if (inherits(lmer_formula, "formula")) {
    lmer_formula <- Reduce(paste, deparse(lmer_formula))
  }

  scfa_data_path <- NULL
  if (!is.null(scfa_data)) {
    scfa_data_path <- tempfile(fileext = ".rds")
    saveRDS(scfa_data, scfa_data_path)
    scfa_data <- NULL
  }
  manifest_path_override <- NULL
  if (!is.null(manifest)) {
    manifest_path_override <- tempfile(fileext = ".rds")
    saveRDS(manifest, manifest_path_override)
    manifest <- NULL
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
    manifest_override = manifest,
    scfa_data_path = scfa_data_path,
    manifest_path_override = manifest_path_override
  )

  if (is.null(output_dir)) {
    output_dir <- getwd()
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_name <- basename(output_file)
  output_path <- file.path(output_dir, output_name)

  tmp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  tmp_dir <- file.path(tmp_root, paste0("scfa_report_", Sys.getpid()))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_input <- file.path(tmp_dir, basename(template))
  file.copy(template, tmp_input, overwrite = TRUE)

  cleanup_warning <- FALSE
  tryCatch(
    {
      quarto::quarto_render(
        input = tmp_input,
        execute_params = params,
        output_file = output_name,
        ...
      )
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("Refusing to remove directory", msg, fixed = TRUE)) {
        warning("Quarto cleanup issue: ", msg)
        cleanup_warning <<- TRUE
      } else {
        stop(e)
      }
    }
  )

  rendered_file <- file.path(tmp_dir, output_name)
  if (!file.exists(rendered_file)) {
    stop("Rendered file not found at ", rendered_file)
  }
  file.copy(rendered_file, output_path, overwrite = TRUE)

  csv_tmp <- file.path(tmp_dir, "scfa_concentration_normalized.csv")
  if (file.exists(csv_tmp)) {
    file.copy(csv_tmp, file.path(output_dir, "scfa_concentration_normalized.csv"), overwrite = TRUE)
  }

  if (!cleanup_warning) {
    unlink(tmp_dir, recursive = TRUE)
  }

  invisible(output_path)
}
