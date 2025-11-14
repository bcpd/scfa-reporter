# Generates small demo SCFA datasets for package documentation and tests.

set.seed(123)

analytes <- c("Acetic.Acid", "Propionic.Acid", "Butyric.Acid")

# Dataset 1 ---------------------------------------------------------------

n_per_group <- 10
groups1 <- rep(c("A", "B"), each = n_per_group)
samples1 <- sprintf("SCFA%02d", seq_along(groups1))

base_means1 <- data.frame(
  Group = c("A", "B"),
  Acetic.Acid = c(5, 8),   # group B higher acetate only
  Propionic.Acid = c(3, 3),
  Butyric.Acid = c(2, 2)
)

make_values <- function(group_vec, means_df) {
  do.call(rbind, lapply(group_vec, function(g) {
    mu <- as.numeric(means_df[means_df$Group == g, analytes])
    mu + stats::rnorm(length(analytes), mean = 0, sd = 0.5)
  }))
}

values1 <- make_values(groups1, base_means1)
colnames(values1) <- analytes

demo_scfa_dataset1 <- tibble::as_tibble(values1)
demo_scfa_dataset1$Sample <- samples1
demo_scfa_dataset1 <- demo_scfa_dataset1[, c("Sample", analytes)]

subjects1 <- sprintf("SUBJ%02d", seq_len(length(groups1)))
demo_manifest_dataset1 <- tibble::tibble(
  MBI.Sample.ID = samples1,
  Your.Sample.ID = samples1,
  Subject_ID = subjects1,
  Group = factor(groups1, levels = c("A", "B")),
  Time = factor(rep("T0", length(groups1)), levels = "T0"),
  Sample_Type = "Plasma"
)

# Dataset 2 ---------------------------------------------------------------
subjects2 <- sprintf("SUBJ%02d", 1:12)
group_assign <- rep(c("A", "B"), each = 6)
times <- c("T0", "T1", "T2")

manifest_rows <- do.call(rbind, lapply(seq_along(subjects2), function(i) {
  subject <- subjects2[i]
  group <- group_assign[i]
  data.frame(
    Subject_ID = subject,
    Group = group,
    Time = times,
    stringsAsFactors = FALSE
  )
}))
manifest_rows$MBI.Sample.ID <- sprintf("SCFA2_%02d", seq_len(nrow(manifest_rows)))
manifest_rows$Your.Sample.ID <- manifest_rows$MBI.Sample.ID
manifest_rows$Sample_Type <- "Plasma"

base_means2 <- data.frame(
  Group = c("A", "B"),
  Acetic.Acid = c(5, 5),
  Propionic.Acid = c(3, 3),
  Butyric.Acid = c(2, 4)   # group B higher butyrate only
)

values2 <- make_values(manifest_rows$Group, base_means2)
colnames(values2) <- analytes

demo_scfa_dataset2 <- tibble::as_tibble(values2)
demo_scfa_dataset2$Sample <- manifest_rows$MBI.Sample.ID
demo_scfa_dataset2 <- demo_scfa_dataset2[, c("Sample", analytes)]

demo_manifest_dataset2 <- tibble::as_tibble(manifest_rows)
demo_manifest_dataset2$Group <- factor(demo_manifest_dataset2$Group, levels = c("A", "B"))
demo_manifest_dataset2$Time <- factor(demo_manifest_dataset2$Time, levels = times)

# Save datasets -----------------------------------------------------------
usethis::use_data(
  demo_scfa_dataset1,
  demo_manifest_dataset1,
  demo_scfa_dataset2,
  demo_manifest_dataset2,
  overwrite = TRUE
)
