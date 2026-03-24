# Run segmentation inference on all trainer tiles
# Uses RFDETRSegPreview directly (no SAHI) for mask-based predictions
# Generates: ground truth CSVs, color-coded mask overlays

library(reticulate)
devtools::load_all("/Users/nick/Dropbox/Projects/petrographer")

rfdetr <- import("rfdetr")
sv <- import("supervision")
PIL <- import("PIL")
np <- import("numpy")
skimage <- import("skimage")

# --- Config ---
PROJECT_DIR <- "/Users/nick/Dropbox/Projects/petrog-trainer"
LYONS_DIR <- "/Users/nick/Dropbox/Projects/petrographer/data/raw/Lyons_data"
SEG_MODEL_PATH <- "/Users/nick/Dropbox/Projects/petrographer/.petrographer/models/inclusions_shell_seg_preview/20251027T235034Z-08bb7/checkpoint_best_total.pth"

OUTPUT_IMG_DIR <- file.path(PROJECT_DIR, "www/images/original")
OUTPUT_ANNO_DIR <- file.path(PROJECT_DIR, "www/images/annotated")
DATA_DIR <- file.path(PROJECT_DIR, "data")

# Size class breaks (mask pixel area)
SIZE_BREAKS <- c(0, 500, 2000, 8000, Inf)
SIZE_LABELS <- c("Fine", "Medium", "Coarse", "Very Coarse")

# Size class colors (RGBA for supervision) — match app bar charts
SIZE_COLORS <- list(
  Fine        = c(52L, 152L, 219L),   # blue
  Medium      = c(46L, 204L, 113L),   # green
  Coarse      = c(243L, 156L, 18L),   # orange
  `Very Coarse` = c(231L, 76L, 60L)   # red
)

# --- Tile definitions ---
tiles <- list(
  # Fabric C - 8-587 (15 curated interior tiles)
  list(fabric = "C", sample = "8-587", dir = file.path(LYONS_DIR, "c"),
       files = paste0("8-587_xp_", sprintf("%06d", c(16,19,29,30,32,33,34,36,40,43,44,46,47,48,51)), ".jpg")),
  # Fabric C - 139-45
  list(fabric = "C", sample = "139-45", dir = file.path(LYONS_DIR, "c"),
       files = paste0("139-45_xp_", sprintf("%06d", c(21,23,35,36,42,57,59,60)), ".jpg")),
  # Fabric C - 90-53
  list(fabric = "C", sample = "90-53", dir = file.path(LYONS_DIR, "c"),
       files = paste0("90-53_xp_", sprintf("%06d", c(11,13,30)), ".jpg")),
  # Fabric A - 53-204
  list(fabric = "A", sample = "53-204", dir = file.path(LYONS_DIR, "a"),
       files = paste0("53-204_xp_", sprintf("%06d", c(10,12,14)), ".jpg")),
  # Fabric D - 17-112
  list(fabric = "D", sample = "17-112", dir = file.path(LYONS_DIR, "d"),
       files = paste0("17-112_xp_", sprintf("%06d", c(37,41,43)), ".jpg")),
  # Fabric E - 12_251
  list(fabric = "E", sample = "12_251", dir = file.path(LYONS_DIR, "e"),
       files = paste0("12_251_xp_", sprintf("%06d", c(30,32,35)), ".jpg")),
  # Fabric W - 17-142
  list(fabric = "W", sample = "17-142", dir = file.path(LYONS_DIR, "w"),
       files = paste0("17-142_xp_", sprintf("%06d", c(45,55)), ".jpg"))
)

# --- Load model ---
cat("Loading RFDETRSegPreview model...\n")
model <- rfdetr$RFDETRSegPreview(
  pretrain_weights = SEG_MODEL_PATH,
  device = "cpu"
)
cat("Model loaded.\n\n")

# --- Helper: get size class ---
get_size_class <- function(area) {
  idx <- findInterval(area, SIZE_BREAKS)
  SIZE_LABELS[min(idx, length(SIZE_LABELS))]
}

# --- Helper: extract morphology from one mask ---
extract_morphology <- function(bool_mask) {
  labeled <- skimage$measure$label(bool_mask)
  storage.mode(labeled) <- "integer"
  props <- skimage$measure$regionprops(labeled)
  if (length(props) == 0) return(NULL)
  p <- props[[1]]

  area <- as.numeric(p$area)
  perimeter <- as.numeric(p$perimeter)
  circularity <- if (perimeter > 0) (4 * pi * area) / (perimeter^2) else NA
  major <- as.numeric(p$major_axis_length)
  minor <- as.numeric(p$minor_axis_length)
  aspect_ratio <- if (minor > 0) major / minor else NA
  centroid <- as.numeric(p$centroid)

  list(
    area = area,
    perimeter = perimeter,
    circularity = min(circularity, 1.0),  # cap at 1
    eccentricity = as.numeric(p$eccentricity),
    aspect_ratio = aspect_ratio,
    solidity = as.numeric(p$solidity),
    extent = as.numeric(p$extent),
    orientation = as.numeric(p$orientation),
    major_axis_length = major,
    minor_axis_length = minor,
    centroid_x = centroid[2],
    centroid_y = centroid[1]
  )
}

# --- Helper: render color-coded mask overlay ---
render_color_overlay <- function(img_pil, detections, size_classes, output_path) {
  # Create a custom color lookup based on size class
  n <- length(size_classes)
  colors <- matrix(0L, nrow = n, ncol = 3)
  for (i in seq_len(n)) {
    col <- SIZE_COLORS[[size_classes[i]]]
    if (is.null(col)) col <- SIZE_COLORS[["Medium"]]
    colors[i, ] <- col
  }

  # Use supervision ColorLookup with custom colors
  color_array <- np$array(colors, dtype = np$uint8)

  annotator <- sv$MaskAnnotator(
    opacity = 0.45,
    color = sv$Color$from_rgb_tuple(c(100L, 100L, 100L)),  # fallback
    color_lookup = sv$ColorLookup$INDEX
  )

  # Set custom colors per detection
  annotated <- np$array(img_pil)

  # Draw masks manually with per-detection colors
  masks <- detections$mask
  for (i in seq_len(n)) {
    mask_i <- masks[i - 1L, , ]  # 0-indexed
    col <- SIZE_COLORS[[size_classes[i]]]
    if (is.null(col)) col <- SIZE_COLORS[["Medium"]]

    # Apply semi-transparent color overlay where mask is TRUE
    mask_bool <- np$array(mask_i, dtype = np$bool_)
    for (ch in 1:3) {
      channel <- annotated[, , ch - 1L]
      blended <- np$where(mask_bool,
                          np$uint8(np$clip(channel * 0.55 + col[ch] * 0.45, 0L, 255L)),
                          channel)
      annotated[, , ch - 1L] <- blended
    }
  }

  # Save
  result <- PIL$Image$fromarray(annotated)
  result$save(output_path)
}

# --- Main processing loop ---
dir.create(OUTPUT_IMG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_ANNO_DIR, showWarnings = FALSE, recursive = TRUE)

all_summary <- list()
all_objects <- list()
img_area_default <- 2048 * 1536

for (tile_group in tiles) {
  fabric <- tile_group$fabric
  sample_name <- tile_group$sample
  img_dir <- tile_group$dir

  cat(sprintf("=== Fabric %s, Sample %s ===\n", fabric, sample_name))

  for (fname in tile_group$files) {
    img_path <- file.path(img_dir, fname)
    if (!file.exists(img_path)) {
      cat(sprintf("  SKIP (not found): %s\n", fname))
      next
    }

    # Run inference
    img_pil <- PIL$Image$open(img_path)
    img_size <- img_pil$size  # (width, height)
    img_pixels <- as.numeric(img_size[[1]]) * as.numeric(img_size[[2]])

    detections <- tryCatch(
      model$predict(img_pil, threshold = 0.2),
      error = function(e) { cat(sprintf("  ERROR on %s: %s\n", fname, e$message)); NULL }
    )
    if (is.null(detections)) next

    n_det <- nrow(detections$xyxy)
    if (n_det == 0) {
      cat(sprintf("  %s: 0 detections, skipping\n", fname))
      next
    }

    # Extract morphology for each detection
    masks <- detections$mask
    objects <- list()
    size_classes <- character(n_det)

    for (i in seq_len(n_det)) {
      mask_i <- masks[i - 1L, , ]
      morph <- tryCatch(extract_morphology(mask_i), error = function(e) NULL)
      if (is.null(morph)) {
        # Fallback to bbox
        bbox <- as.numeric(detections$xyxy[i - 1L, ])
        morph <- list(
          area = (bbox[3] - bbox[1]) * (bbox[4] - bbox[2]),
          perimeter = NA, circularity = NA, eccentricity = NA,
          aspect_ratio = NA, solidity = NA, extent = NA, orientation = NA,
          major_axis_length = NA, minor_axis_length = NA,
          centroid_x = (bbox[1] + bbox[3]) / 2, centroid_y = (bbox[2] + bbox[4]) / 2
        )
      }

      sc <- get_size_class(morph$area)
      size_classes[i] <- sc
      bbox <- as.numeric(detections$xyxy[i - 1L, ])

      objects[[i]] <- data.frame(
        file_name = fname,
        class_name = "inclusion",  # merge shell + inclusion
        object_number = i,
        area = morph$area,
        perimeter = morph$perimeter,
        circularity = morph$circularity,
        eccentricity = morph$eccentricity,
        aspect_ratio = morph$aspect_ratio,
        solidity = morph$solidity,
        extent = morph$extent,
        Centroid = sprintf("(%d, %.1f, %.1f)", i - 1, morph$centroid_x, morph$centroid_y),
        BoundingBox = sprintf("(%d, %d, %d, %d, %d, %d)", i - 1,
                              as.integer(bbox[1]), as.integer(bbox[2]),
                              i, as.integer(bbox[3]), as.integer(bbox[4])),
        size_category = sc,
        stringsAsFactors = FALSE
      )
    }

    obj_df <- do.call(rbind, objects)
    all_objects[[length(all_objects) + 1]] <- obj_df

    # Summary stats
    total_area <- sum(obj_df$area)
    incl_pct <- round(total_area / img_pixels * 100, 1)

    size_counts <- table(factor(obj_df$size_category, levels = SIZE_LABELS))
    size_pcts <- round(as.numeric(size_counts) / n_det * 100, 1)

    # Mean morphology
    mean_circ <- mean(obj_df$circularity, na.rm = TRUE)
    mean_ar <- mean(obj_df$aspect_ratio, na.rm = TRUE)
    mean_sol <- mean(obj_df$solidity, na.rm = TRUE)

    summary_row <- data.frame(
      file_name = fname,
      n_inclusions = n_det,
      total_inclusion_area = total_area,
      mean_area = round(mean(obj_df$area), 1),
      median_area = round(median(obj_df$area), 1),
      sd_area = round(sd(obj_df$area), 1),
      min_area = min(obj_df$area),
      max_area = max(obj_df$area),
      q25_area = round(quantile(obj_df$area, 0.25, na.rm = TRUE), 1),
      q75_area = round(quantile(obj_df$area, 0.75, na.rm = TRUE), 1),
      inclusion_pct = incl_pct,
      sample_id = gsub(".*_xp_0*", "", gsub("\\.jpg", "", fname)),
      sample_name = sample_name,
      fabric_type = fabric,
      polarization = "XPL",
      mean_circularity = round(mean_circ, 3),
      mean_aspect_ratio = round(mean_ar, 3),
      mean_solidity = round(mean_sol, 3),
      Fine = as.integer(size_counts["Fine"]),
      Medium = as.integer(size_counts["Medium"]),
      Coarse = as.integer(size_counts["Coarse"]),
      `Very Coarse` = as.integer(size_counts["Very Coarse"]),
      Fine_pct = size_pcts[1],
      Medium_pct = size_pcts[2],
      Coarse_pct = size_pcts[3],
      `Very Coarse_pct` = size_pcts[4],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    all_summary[[length(all_summary) + 1]] <- summary_row

    # Copy original image
    file.copy(img_path, file.path(OUTPUT_IMG_DIR, fname), overwrite = TRUE)

    # Render color-coded mask overlay
    anno_name <- gsub("\\.jpg$", "_result.png", fname)
    anno_path <- file.path(OUTPUT_ANNO_DIR, anno_name)
    tryCatch(
      render_color_overlay(img_pil, detections, size_classes, anno_path),
      error = function(e) cat(sprintf("  Overlay error: %s\n", e$message))
    )

    cat(sprintf("  %s: %d detections, %.1f%% area, circ=%.2f, AR=%.2f\n",
                fname, n_det, incl_pct, mean_circ, mean_ar))
  }
}

# --- Save CSVs ---
summary_df <- do.call(rbind, all_summary)
summary_df$display_name <- paste0("Sample ", seq_len(nrow(summary_df)))

objects_df <- do.call(rbind, all_objects)

readr::write_csv(summary_df, file.path(DATA_DIR, "ground_truth_summary.csv"))
readr::write_csv(objects_df, file.path(DATA_DIR, "ground_truth_objects.csv"))

cat(sprintf("\n=== DONE ===\nTiles: %d\nObjects: %d\n", nrow(summary_df), nrow(objects_df)))
cat("By fabric:", paste(names(table(summary_df$fabric_type)),
                        table(summary_df$fabric_type), sep = "=", collapse = ", "), "\n")
