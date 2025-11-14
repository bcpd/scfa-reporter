#' Demo SCFA dataset 1
#'
#' Simulated Chromeleon-style SCFA peak table with 20 samples across two groups
#' (A/B) and three analytes (acetate, propionate, butyrate). Group B shows higher
#' acetate values.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{Sample}{Sample identifier matching the manifest `MBI.Sample.ID`.}
#'   \item{Acetic.Acid}{Acetate concentrations (arbitrary units).}
#'   \item{Propionic.Acid}{Propionate concentrations.}
#'   \item{Butyric.Acid}{Butyrate concentrations.}
#' }
#' @source `data-raw/make_demo_scfa_data.R`
"demo_scfa_dataset1"

#' Demo manifest for dataset 1
#'
#' Metadata describing the samples in `demo_scfa_dataset1`, including group
#' assignments and time point (all T0).
#'
#' @format A tibble with columns `MBI.Sample.ID`, `Your.Sample.ID`,
#'   `Subject_ID`, `Group`, `Time`, and `Sample_Type`.
#' @source `data-raw/make_demo_scfa_data.R`
"demo_manifest_dataset1"

#' Demo SCFA dataset 2
#'
#' Simulated SCFA measurements for repeated samples collected at T0, T1, and T2
#' across two groups. Group B has elevated butyrate concentrations only.
#'
#' @format A tibble with columns `Sample`, `Acetic.Acid`, `Propionic.Acid`, and
#'   `Butyric.Acid`.
#' @source `data-raw/make_demo_scfa_data.R`
"demo_scfa_dataset2"

#' Demo manifest for dataset 2
#'
#' Manifest accompanying `demo_scfa_dataset2`, with repeated measures for each
#' subject at T0, T1, and T2 plus group assignments.
#'
#' @format A tibble with columns `MBI.Sample.ID`, `Your.Sample.ID`, `Subject_ID`,
#'   `Group`, `Time`, and `Sample_Type`.
#' @source `data-raw/make_demo_scfa_data.R`
"demo_manifest_dataset2"
