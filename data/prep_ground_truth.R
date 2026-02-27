library(dplyr)
library(readr)

raw <- read_csv("/Users/nick/dropbox/Projects/petrographer/data/lyons_annotations/output_objects.csv",
                show_col_types = FALSE)

selected <- c("8-587_xp_000065.jpg", "8-587_xp_000060.jpg", "8-587_xp_000027.jpg",
               "8-587_xp_000029.jpg", "8-587_xp_000020.jpg", "8-587_xp_000043.jpg")

dat <- raw |>
  filter(`File Name` %in% selected) |>
  rename(file_name = `File Name`, class_name = `Class Name`,
         object_number = `Object Number`, area = Area)

img_area <- 2048 * 1536  # actual dimensions

summary_stats <- dat |>
  group_by(file_name) |>
  summarise(
    n_inclusions = n(),
    total_inclusion_area = sum(area),
    mean_area = mean(area),
    median_area = median(area),
    sd_area = sd(area),
    min_area = min(area),
    max_area = max(area),
    q25_area = quantile(area, 0.25),
    q75_area = quantile(area, 0.75),
    .groups = "drop"
  ) |>
  mutate(
    inclusion_pct = round(total_inclusion_area / img_area * 100, 1),
    sample_id = gsub("8-587_xp_|.jpg", "", file_name),
    display_name = paste0("Sample ", row_number())
  )

# Size categories (using area in pixels — rough mapping to petrographic sizes)
size_breaks <- c(0, 500, 2000, 8000, Inf)
size_labels <- c("Fine", "Medium", "Coarse", "Very Coarse")

dat_sized <- dat |>
  mutate(size_category = cut(area, breaks = size_breaks, labels = size_labels))

size_dist <- dat_sized |>
  group_by(file_name, size_category) |>
  summarise(count = n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = size_category, values_from = count, values_fill = 0) |>
  mutate(across(all_of(size_labels), ~ round(. / rowSums(across(all_of(size_labels))) * 100, 1),
                .names = "{.col}_pct"))

summary_stats <- summary_stats |>
  left_join(size_dist, by = "file_name")

write_csv(summary_stats, "data/ground_truth_summary.csv")
write_csv(dat_sized, "data/ground_truth_objects.csv")

cat("Summary stats:\n")
print(summary_stats |> select(display_name, n_inclusions, inclusion_pct, Fine_pct, Medium_pct, Coarse_pct, `Very Coarse_pct`))
