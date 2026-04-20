standardize_manifest_names <- function(manifest) {
  rename_first_match <- function(df, target, candidates) {
    hit <- intersect(candidates, names(df))
    if (length(hit) == 0) {
      return(df)
    }
    from <- hit[[1]]
    if (from != target) {
      names(df)[names(df) == from] <- target
    }
    df
  }

  manifest <- rename_first_match(
    manifest,
    "mbi_sample_id",
    c("mbi_sample_id", "sample_id", "sample")
  )
  manifest <- rename_first_match(
    manifest,
    "your_sample_id",
    c("your_sample_id", "your_sampleid")
  )
  manifest <- rename_first_match(
    manifest,
    "subject_id",
    c("subject_id", "subjectid", "subject", "participant_id")
  )
  manifest <- rename_first_match(
    manifest,
    "group",
    c("group", "treatment_group", "study_group")
  )
  manifest <- rename_first_match(
    manifest,
    "time",
    c("time", "time_point", "timepoint", "visit", "week", "day")
  )
  manifest
}

#' Read chromatogram exports for SCFA quantification
#'
#' @param dir Directory that contains Thermo Chromeleon `.xls` exports.
#' @param subset_pattern Optional regular expression used to subset the files that
#'   should be imported. When `NULL` (default) all `.xls` files are used.
#' @param sheet Sheet name inside each workbook (defaults to `"Integration"`).
#' @param skip Number of header rows to skip before the peak table (defaults to `39`).
#' @param sample_col Name of the column that will hold the sample identifier in
#'   the returned tibble (will be cleaned with [janitor::make_clean_names()]).
#' @param read_fun Function used to read each chromatogram workbook (defaults to
#'   [readxl::read_xls()]).
#'
#' @return A tibble where each row represents one chromatogram/sample and each
#'   column represents a quantified SCFA peak.
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' chromatograms <- read_scfa_raw("chromatograms")
#' }
read_scfa_raw <- function(
  dir = ".",
  subset_pattern = NULL,
  sheet = "Integration",
  skip = 39,
  sample_col = "Sample",
  read_fun = readxl::read_xls
) {
  if (!dir.exists(dir)) {
    stop("Directory '", dir, "' does not exist.", call. = FALSE)
  }

  files <- list.files(dir, pattern = "\\.xls$", full.names = TRUE)
  if (!is.null(subset_pattern)) {
    files <- files[grepl(subset_pattern, basename(files))]
  }
  if (length(files) == 0) {
    stop("No .xls chromatogram files were found in ", dir, call. = FALSE)
  }

  sample_col_clean <- janitor::make_clean_names(sample_col)

  samples <- lapply(files, function(path) {
    dat <- read_fun(
      path = path,
      sheet = sheet,
      skip = skip,
      progress = FALSE
    ) %>%
      janitor::clean_names()
    if (!all(c("peak_name", "amount") %in% names(dat))) {
      stop(
        "peak_name and amount columns were not found in ",
        basename(path),
        call. = FALSE
      )
    }
    dat <- dat[, c("peak_name", "amount")]
    dat$amount <- suppressWarnings(as.numeric(dat$amount))
    dat <- stats::na.omit(dat)
    dat <- dat[dat$peak_name != "", ]
    sample_name <- tools::file_path_sans_ext(basename(path))
    stats::setNames(dat, c("peak_name", sample_name))
  })

  combined <- Reduce(
    function(x, y) dplyr::full_join(x, y, by = "peak_name"),
    samples
  )
  combined <- combined[
    !combined$peak_name %in% c("Component 2", "2-Ethylbutyric Acid"),
  ]

  matrix <- as.data.frame(t(combined[, -1]), stringsAsFactors = FALSE)
  colnames(matrix) <- combined$peak_name
  matrix <- tibble::rownames_to_column(matrix, var = sample_col_clean)
  matrix <- janitor::clean_names(matrix)
  matrix[is.na(matrix)] <- 0
  tibble::as_tibble(matrix)
}

#' Read the SCFA manifest/metadata workbook
#'
#' @param path Path to the Excel workbook that contains the manifest.
#' @param sheet Sheet name that holds the metadata (defaults to `"Sample Sheet"`).
#' @param skip Number of header rows to skip before the true table starts.
#'
#' @return A tibble with the manifest and a derived `Subject_ID` column that can
#'   be used for longitudinal modeling.
#' @export
#'
#' @examples
#' \dontrun{
#' manifest <- read_scfa_manifest("P-00UH univ saxet.xlsx")
#' }
read_scfa_manifest <- function(path, sheet = "Sample Sheet", skip = 20) {
  if (!file.exists(path)) {
    stop("Manifest file '", path, "' does not exist.", call. = FALSE)
  }
  manifest <- readxl::read_xlsx(
    path = path,
    sheet = sheet,
    skip = skip,
    progress = FALSE
  )
  manifest <- manifest %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    standardize_manifest_names()

  required_cols <- c("your_sample_id", "mbi_sample_id")
  if (!all(required_cols %in% names(manifest))) {
    missing <- required_cols[!required_cols %in% names(manifest)]
    stop(
      "Manifest is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  manifest
}

#' Merge manifest information with SCFA quantifications
#'
#' @param scfa_data Tibble returned by [read_scfa_raw()].
#' @param manifest Tibble returned by [read_scfa_manifest()].
#' @param sample_id_col Column in `manifest` that uniquely identifies samples
#'   (defaults to `"MBI.Sample.ID"`).
#' @param sample_col Column in `scfa_data` that contains the sample identifier
#'   (defaults to `"Sample"`).
#'
#' @return A tibble with manifest columns followed by the SCFA measurements.
#' @export
#'
#' @examples
#' \dontrun{
#' merged <- merge_scfa_data(chromatograms, manifest)
#' }
merge_scfa_data <- function(
  scfa_data,
  manifest,
  sample_id_col = "mbi_sample_id",
  sample_col = "sample"
) {
  scfa_data <- janitor::clean_names(scfa_data)
  manifest <- janitor::clean_names(manifest)

  sample_id_clean <- janitor::make_clean_names(sample_id_col)
  sample_col_clean <- janitor::make_clean_names(sample_col)

  if (!sample_col_clean %in% names(scfa_data)) {
    stop(
      "Column '",
      sample_col_clean,
      "' was not found in scfa_data.",
      call. = FALSE
    )
  }
  if (!sample_id_clean %in% names(manifest)) {
    stop(
      "Column '",
      sample_id_clean,
      "' was not found in manifest.",
      call. = FALSE
    )
  }
  join_ready <- scfa_data
  names(join_ready)[names(join_ready) == sample_col_clean] <- sample_id_clean
  dplyr::left_join(manifest, join_ready, by = sample_id_clean)
}
