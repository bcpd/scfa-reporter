#' Reshape SCFA data to long format
#'
#' @param data Tibble that contains SCFA measurements.
#' @param analytes Character vector with the analyte column names.
#' @param id_cols Character vector with metadata columns that should remain
#'   untouched when pivoting. When `NULL` (default), commonly used columns
#'   present in `data` (`mbi_sample_id`, `subject_id`, `group`, `time`) are
#'   detected automatically.
#' @param analyte_col Name of the resulting analyte column (defaults to
#'   `"Analyte"`).
#' @param value_col Name of the resulting value column (defaults to `"value"`).
#'
#' @return A tibble in long format that is ready for modeling and plotting.
#' @export
#'
#' @examples
#' \dontrun{
#' scfa_long <- scfa_long_format(normalized, analytes = default_analytes)
#' }
scfa_long_format <- function(data,
                             analytes,
                             id_cols = NULL,
                             analyte_col = "analyte",
                             value_col = "value") {
  data <- janitor::clean_names(data)
  analytes <- janitor::make_clean_names(analytes)
  analyte_col <- janitor::make_clean_names(analyte_col)
  value_col <- janitor::make_clean_names(value_col)

  if (is.null(id_cols)) {
    id_cols <- intersect(
      c("mbi_sample_id", "subject_id", "group", "time"),
      names(data)
    )
  } else {
    id_cols <- janitor::make_clean_names(id_cols)
  }

  missing_ids <- id_cols[!id_cols %in% names(data)]
  if (length(missing_ids)) {
    for (col in missing_ids) {
      data[[col]] <- NA
    }
    if (!is.null(id_cols)) {
      warning(
        "Added missing id columns with NA values: ",
        paste(missing_ids, collapse = ", ")
      )
    }
  }

  result <- tidyr::pivot_longer(
    data = data,
    cols = dplyr::all_of(analytes),
    names_to = analyte_col,
    values_to = value_col
  )
  result[[analyte_col]] <- factor(result[[analyte_col]], levels = analytes)
  result
}

clean_optional_name <- function(x) {
  if (is.null(x) || identical(x, "")) {
    return(NULL)
  }
  janitor::make_clean_names(x)
}

#' Generate descriptive statistics for each SCFA
#'
#' @param scfa_long Long-format tibble produced by [scfa_long_format()].
#' @param analyte_col Name of the analyte column.
#' @param group_col Grouping column.
#' @param time_col Optional time/visit column. When `NULL` or absent in
#'   `scfa_long`, the summary is aggregated by analyte and group only.
#' @param subject_col Subject identifier column.
#' @param value_col Measurement column.
#'
#' @return A tibble with counts and summary statistics per analyte/group, and
#'   per analyte/group/time when a time column is available.
#' @importFrom rlang .data
#' @export
scfa_summary <- function(scfa_long,
                         analyte_col = "analyte",
                         group_col = "group",
                         time_col = "time",
                         subject_col = "subject_id",
                         value_col = "value") {
  scfa_long <- janitor::clean_names(scfa_long)
  analyte_col <- janitor::make_clean_names(analyte_col)
  group_col <- clean_optional_name(group_col)
  time_col <- clean_optional_name(time_col)
  subject_col <- janitor::make_clean_names(subject_col)
  value_col <- janitor::make_clean_names(value_col)

  if (is.null(group_col)) {
    stop("Provide a non-empty 'group_col'.", call. = FALSE)
  }
  if (!group_col %in% names(scfa_long)) {
    stop("Column '", group_col, "' was not found in scfa_long.", call. = FALSE)
  }
  if (!subject_col %in% names(scfa_long)) {
    scfa_long[[subject_col]] <- NA
  }
  if (!value_col %in% names(scfa_long)) {
    scfa_long[[value_col]] <- NA
  }

  grouping_cols <- c(analyte_col, group_col)
  if (!is.null(time_col) && time_col %in% names(scfa_long)) {
    grouping_cols <- c(grouping_cols, time_col)
  }

  grouped <- dplyr::group_by(scfa_long, !!!rlang::syms(grouping_cols))
  summarized <- dplyr::summarise(
    grouped,
    Samples = sum(!is.na(.data[[value_col]])),
    Subjects = dplyr::n_distinct(.data[[subject_col]]),
    Mean = mean(.data[[value_col]], na.rm = TRUE),
    SD = stats::sd(.data[[value_col]], na.rm = TRUE),
    SEM = SD / sqrt(pmax(Samples, 1)),
    Min = min(.data[[value_col]], na.rm = TRUE),
    Max = max(.data[[value_col]], na.rm = TRUE),
    .groups = "drop"
  )
  dplyr::arrange(summarized, !!!rlang::syms(grouping_cols))
}

#' Fit SCFA statistical models
#'
#' @param scfa_long Long-format tibble produced by [scfa_long_format()].
#' @param stats Type of model to fit (`"lmer"`, `"anova"`, `"lm"`, `"none"`).
#' @param analyte_col Name of the analyte column.
#' @param value_col Name of the measurement column.
#' @param lmer_formula Formula passed to [lmerTest::lmer()] when `stats = "lmer"`.
#' @param lm_formula Formula passed to [stats::lm()] when `stats = "lm"`.
#' @param aov_formula Formula passed to [stats::aov()] when `stats = "anova"`.
#' @param group_var Column used for group contrasts.
#' @param time_var Optional column used for time contrasts.
#'
#' @return A named list where each element contains the fitted model,
#'   model tests, coefficients, and emmeans contrasts for a single analyte.
#' @importFrom lmerTest lmer
#' @importFrom emmeans emmeans contrast
#' @export
fit_scfa_models <- function(scfa_long,
                            stats = c("lmer", "anova", "lm", "none"),
                            analyte_col = "analyte",
                            value_col = "value",
                            lmer_formula = value ~ group * time + (1 | subject_id),
                            lm_formula = NULL,
                            aov_formula = NULL,
                            group_var = "group",
                            time_var = "time") {
  stats <- match.arg(stats)
  scfa_long <- janitor::clean_names(scfa_long)
  analyte_col <- janitor::make_clean_names(analyte_col)
  value_col <- janitor::make_clean_names(value_col)
  group_var <- clean_optional_name(group_var)
  time_var <- clean_optional_name(time_var)

  if (is.null(group_var)) {
    stop("Provide a non-empty 'group_var'.", call. = FALSE)
  }

  if (stats == "lm" && is.null(lm_formula)) {
    stop("Provide 'lm_formula' when stats = 'lm'.", call. = FALSE)
  }

  split_data <- split(scfa_long, scfa_long[[analyte_col]])

  lapply(split_data, function(df) {
    df <- df[!is.na(df[[value_col]]), ]
    out <- list(data = df)

    if (stats == "lmer") {
      fit <- tryCatch(
        lmerTest::lmer(formula = lmer_formula, data = df),
        error = identity
      )
      if (inherits(fit, "error")) {
        out$model <- NULL
        out$coefficients <- tibble::tibble(Term = "Model", Message = fit$message)
      } else {
        out$model <- fit
        coef_tbl <- as.data.frame(summary(fit)$coefficients, stringsAsFactors = FALSE)
        coef_tbl <- tibble::rownames_to_column(coef_tbl, "Term")
        out$coefficients <- tibble::as_tibble(coef_tbl)

        anova_tbl <- as.data.frame(stats::anova(fit, type = 3), stringsAsFactors = FALSE)
        anova_tbl <- tibble::rownames_to_column(anova_tbl, "Term")
        out$anova <- tibble::as_tibble(anova_tbl)
        if (group_var %in% names(df)) {
          emm_group <- tryCatch(
            emmeans::emmeans(
              fit,
              specs = if (!is.null(time_var) && time_var %in% names(df)) {
                stats::as.formula(paste("~", group_var, "|", time_var))
              } else {
                stats::as.formula(paste("~", group_var))
              }
            ),
            error = identity
          )
          if (!inherits(emm_group, "error")) {
            out$group_contrasts <- as.data.frame(
              emmeans::contrast(emm_group, method = "pairwise", adjust = "tukey")
            )
          }
        }
        if (!is.null(time_var) && time_var %in% names(df)) {
          emm_time <- tryCatch(
            emmeans::emmeans(
              fit,
              specs = stats::as.formula(paste("~", time_var, "|", group_var))
            ),
            error = identity
          )
          if (!inherits(emm_time, "error")) {
            out$time_contrasts <- as.data.frame(
              emmeans::contrast(emm_time, method = "pairwise", adjust = "tukey")
            )
          }
        }
      }
    }

    if (stats == "lm") {
      fit <- tryCatch(
        stats::lm(formula = lm_formula, data = df),
        error = identity
      )
      if (inherits(fit, "error")) {
        out$model <- NULL
        out$coefficients <- tibble::tibble(Term = "Model", Message = fit$message)
      } else {
        out$model <- fit
        coef_tbl <- as.data.frame(summary(fit)$coefficients, stringsAsFactors = FALSE)
        coef_tbl <- tibble::rownames_to_column(coef_tbl, "Term")
        out$coefficients <- tibble::as_tibble(coef_tbl)

        tests_tbl <- tryCatch(
          suppressWarnings(as.data.frame(emmeans::joint_tests(fit), stringsAsFactors = FALSE)),
          error = identity
        )
        if (!inherits(tests_tbl, "error")) {
          if ("model term" %in% names(tests_tbl)) {
            tests_tbl <- stats::setNames(tests_tbl, sub("^model term$", "Term", names(tests_tbl)))
          } else if (!"Term" %in% names(tests_tbl)) {
            tests_tbl <- tibble::rownames_to_column(tests_tbl, "Term")
          }
          out$anova <- tibble::as_tibble(tests_tbl)
        }

        if (group_var %in% names(df)) {
          emm_group <- tryCatch(
            suppressWarnings(emmeans::emmeans(fit, specs = stats::as.formula(paste("~", group_var)))),
            error = identity
          )
          if (!inherits(emm_group, "error")) {
            out$group_contrasts <- as.data.frame(
              emmeans::contrast(emm_group, method = "pairwise", adjust = "tukey")
            )
          }
        }
      }
    }

    if (stats == "anova" && !is.null(aov_formula)) {
      aov_fit <- stats::aov(formula = aov_formula, data = df)
      aov_tbl <- as.data.frame(stats::anova(aov_fit), stringsAsFactors = FALSE)
      aov_tbl <- tibble::rownames_to_column(aov_tbl, "Term")
      out$aov <- tibble::as_tibble(aov_tbl)
      out$tukey <- stats::TukeyHSD(aov_fit)
    }

    out
  })
}
