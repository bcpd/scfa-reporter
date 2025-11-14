#' Read chromatogram exports for SCFA quantification
#'
#' @param dir Directory that contains Thermo Chromeleon `.xls` exports.
#' @param subset_pattern Optional regular expression used to subset the files that
#'   should be imported. When `NULL` (default) all `.xls` files are used.
#' @param sheet Sheet name inside each workbook (defaults to `"Integration"`).
#' @param skip Number of header rows to skip before the peak table (defaults to `39`).
#' @param sample_col Name of the column that will hold the sample identifier in
#'   the returned tibble.
#'
#' @return A tibble where each row represents one chromatogram/sample and each
#'   column represents a quantified SCFA peak.
#' @export
#'
#' @examples
#' \dontrun{
#' chromatograms <- read_scfa_raw("chromatograms")
#' }
read_scfa_raw <- function(dir = ".", subset_pattern = NULL, sheet = "Integration",
                          skip = 39, sample_col = "Sample") {
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

  samples <- lapply(files, function(path) {
    dat <- readxl::read_xls(path = path, sheet = sheet, skip = skip, progress = FALSE)
    if (!all(c("Peak.Name", "Amount") %in% names(dat))) {
      stop("Peak.Name and Amount columns were not found in ", basename(path), call. = FALSE)
    }
    dat <- dat[, c("Peak.Name", "Amount")]
    dat$Amount <- suppressWarnings(as.numeric(dat$Amount))
    dat <- stats::na.omit(dat)
    dat <- dat[dat$Peak.Name != "", ]
    sample_name <- tools::file_path_sans_ext(basename(path))
    stats::setNames(dat, c("Peak.Name", sample_name))
  })

  combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "Peak.Name"), samples)
  combined <- combined[!combined$Peak.Name %in% c("Component 2", "2-Ethylbutyric Acid"), ]

  matrix <- as.data.frame(t(combined[, -1]), stringsAsFactors = FALSE)
  colnames(matrix) <- combined$Peak.Name
  matrix <- tibble::rownames_to_column(matrix, var = sample_col)
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
  manifest <- readxl::read_xlsx(path = path, sheet = sheet, skip = skip, progress = FALSE)
  manifest <- tibble::as_tibble(manifest)
  manifest$Subject_ID <- gsub("-T[0-9]+$", "", manifest$Your.Sample.ID)
  manifest$Subject_ID[is.na(manifest$Subject_ID) | manifest$Subject_ID == ""] <- manifest$MBI.Sample.ID
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
merge_scfa_data <- function(scfa_data,
                            manifest,
                            sample_id_col = "MBI.Sample.ID",
                            sample_col = "Sample") {
  if (!sample_col %in% names(scfa_data)) {
    stop("Column '", sample_col, "' was not found in scfa_data.", call. = FALSE)
  }
  if (!sample_id_col %in% names(manifest)) {
    stop("Column '", sample_id_col, "' was not found in manifest.", call. = FALSE)
  }
  join_ready <- scfa_data
  names(join_ready)[names(join_ready) == sample_col] <- sample_id_col
  dplyr::left_join(manifest, join_ready, by = sample_id_col)
}
