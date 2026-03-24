"""Generate color-coded annotated images where inclusions are colored by size class.

Uses model prediction bounding boxes from output_objects.csv. Colors are designed to
match the bar chart palette in the Shiny app and provide clear visual distinction
between size classes.
"""

import os
import csv
import re
import numpy as np
from PIL import Image, ImageDraw, ImageFont

# Paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(SCRIPT_DIR)
PETROGRAPHER_DIR = os.path.expanduser("~/Dropbox/Projects/petrographer")
IMG_DIR = os.path.join(PETROGRAPHER_DIR, "data/lyons_annotations/img")
OUTPUT_DIR = os.path.join(PROJECT_DIR, "www/images/annotated")
OBJECTS_CSV = os.path.join(PROJECT_DIR, "data/ground_truth_objects.csv")

# Size class colors (RGBA) — match the bar chart in the Shiny app
SIZE_COLORS = {
    "Fine":        {"fill": (52, 152, 219, 100),  "outline": (52, 152, 219, 255)},   # blue
    "Medium":      {"fill": (46, 204, 113, 100),  "outline": (46, 204, 113, 255)},   # green
    "Coarse":      {"fill": (243, 156, 18, 100),  "outline": (243, 156, 18, 255)},   # orange
    "Very Coarse": {"fill": (231, 76, 60, 100),   "outline": (231, 76, 60, 255)},    # red
}

SIZE_LABELS = ["Fine", "Medium", "Coarse", "Very Coarse"]


def parse_bbox(bbox_str):
    """Parse bounding box string like '(0, 830, 219, 1, 874, 267)' -> (x1, y1, x2, y2)."""
    nums = [float(x) for x in re.findall(r'[\d.]+', bbox_str)]
    # Format is (obj_idx, x_min, y_min, obj_idx+1, x_max, y_max)
    if len(nums) >= 6:
        return int(nums[1]), int(nums[2]), int(nums[4]), int(nums[5])
    return None


def generate_annotated_image(img_path, objects, output_path):
    """Draw color-coded bounding boxes on the original image."""
    img = Image.open(img_path).convert("RGBA")
    overlay = Image.new("RGBA", img.size, (0, 0, 0, 0))
    draw = ImageDraw.Draw(overlay)

    for obj in objects:
        size_class = obj["size_category"]
        bbox = parse_bbox(obj["BoundingBox"])
        if bbox is None:
            continue

        x1, y1, x2, y2 = bbox
        colors = SIZE_COLORS.get(size_class, SIZE_COLORS["Medium"])

        # Draw filled rectangle with semi-transparent fill
        draw.rectangle([x1, y1, x2, y2], fill=colors["fill"], outline=colors["outline"], width=2)

    # Composite overlay onto original
    result = Image.alpha_composite(img, overlay)

    # Add legend
    result = add_legend(result)

    # Save as RGB PNG
    result = result.convert("RGB")
    result.save(output_path, quality=95)


def add_legend(img):
    """Add a size class color legend to the bottom-right corner."""
    box_size = 20
    padding = 12
    line_height = 28
    legend_width = 170
    legend_height = len(SIZE_LABELS) * line_height + padding * 2 + 24

    x0 = img.width - legend_width - padding
    y0 = img.height - legend_height - padding

    # Semi-transparent background
    legend_bg = Image.new("RGBA", img.size, (0, 0, 0, 0))
    legend_draw = ImageDraw.Draw(legend_bg)
    legend_draw.rounded_rectangle(
        [x0, y0, x0 + legend_width, y0 + legend_height],
        radius=8,
        fill=(0, 0, 0, 180)
    )
    img = Image.alpha_composite(img, legend_bg)
    draw = ImageDraw.Draw(img)

    try:
        font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 15)
        font_small = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 13)
    except (OSError, IOError):
        font = ImageFont.load_default()
        font_small = font

    draw.text((x0 + padding, y0 + padding), "Size Class", fill=(255, 255, 255, 255), font=font)

    for i, label in enumerate(SIZE_LABELS):
        y = y0 + padding + 24 + i * line_height
        color = SIZE_COLORS[label]["outline"][:3]
        draw.rectangle([x0 + padding, y, x0 + padding + box_size, y + box_size], fill=color)
        draw.text((x0 + padding + box_size + 10, y + 2), label,
                  fill=(255, 255, 255, 255), font=font_small)

    return img


def main():
    # Read objects CSV and group by file
    objects_by_file = {}
    with open(OBJECTS_CSV) as f:
        reader = csv.DictReader(f)
        for row in reader:
            fname = row["file_name"]
            objects_by_file.setdefault(fname, []).append(row)

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    print(f"Generating color-coded annotations for {len(objects_by_file)} images...")

    for filename in sorted(objects_by_file.keys()):
        img_path = os.path.join(IMG_DIR, filename)
        if not os.path.exists(img_path):
            print(f"  WARNING: {img_path} not found")
            continue

        out_name = filename.replace(".jpg", "_result.png")
        out_path = os.path.join(OUTPUT_DIR, out_name)

        objects = objects_by_file[filename]
        generate_annotated_image(img_path, objects, out_path)

        # Count by size class
        counts = {}
        for obj in objects:
            sc = obj["size_category"]
            counts[sc] = counts.get(sc, 0) + 1
        count_str = ", ".join(f"{k}: {v}" for k, v in sorted(counts.items()))
        print(f"  {filename}: {len(objects)} inclusions ({count_str})")

    print("Done!")


if __name__ == "__main__":
    main()
