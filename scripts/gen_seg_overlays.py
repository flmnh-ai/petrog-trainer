"""Generate color-coded segmentation mask overlays for all tiles.

Reads the saved ground_truth_objects.csv for detection data, then re-runs
inference to get masks for overlay rendering. Also fixes area NAs and
regenerates the summary CSV.
"""

import os
import sys
import csv
import json
import numpy as np
from PIL import Image

# Paths
PROJECT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUTPUT_ANNO_DIR = os.path.join(PROJECT_DIR, "www/images/annotated")
OUTPUT_IMG_DIR = os.path.join(PROJECT_DIR, "www/images/original")
DATA_DIR = os.path.join(PROJECT_DIR, "data")
LYONS_DIR = os.path.expanduser("~/Dropbox/Projects/petrographer/data/raw/Lyons_data")
SEG_MODEL_PATH = os.path.expanduser(
    "~/Dropbox/Projects/petrographer/.petrographer/models/"
    "inclusions_shell_seg_preview/20251027T235034Z-08bb7/checkpoint_best_total.pth"
)

# Size class config
SIZE_BREAKS = [0, 500, 2000, 8000, float('inf')]
SIZE_LABELS = ["Fine", "Medium", "Coarse", "Very Coarse"]
SIZE_COLORS_RGB = {
    "Fine":        (52, 152, 219),
    "Medium":      (46, 204, 113),
    "Coarse":      (243, 156, 18),
    "Very Coarse": (231, 76, 60),
}

# Curated 20-tile set: 4 per fabric type (C gets 8 across 3 sherds)
TILES = {
    ("A", "53-204", "a"): [f"53-204_xp_{i:06d}.jpg" for i in [9,10,12]],
    ("C", "90-53", "c"):  [f"90-53_xp_{i:06d}.jpg" for i in [20,25,30]],
    ("C", "8-587", "c"):  [f"8-587_xp_{i:06d}.jpg" for i in [30,34,40]],
    ("C", "139-45", "c"): [f"139-45_xp_{i:06d}.jpg" for i in [35,57]],
    ("D", "17-112", "d"): [f"17-112_xp_{i:06d}.jpg" for i in [37,41,43]],
    ("E", "12_251", "e"): [f"12_251_xp_{i:06d}.jpg" for i in [25,30,35]],
    ("W", "17-142", "w"): [f"17-142_xp_{i:06d}.jpg" for i in [38,45,55]],
}


def get_size_class(area):
    for i, (lo, hi) in enumerate(zip(SIZE_BREAKS[:-1], SIZE_BREAKS[1:])):
        if lo < area <= hi:
            return SIZE_LABELS[i]
    return SIZE_LABELS[-1]


def extract_morphology(mask):
    """Extract morphological properties from a boolean mask."""
    from skimage.measure import label, regionprops
    labeled = label(mask.astype(np.uint8))
    props = regionprops(labeled)
    if not props:
        return None
    p = props[0]
    area = float(p.area)
    perim = float(p.perimeter)
    circ = (4 * np.pi * area) / (perim ** 2) if perim > 0 else 0
    major = float(p.major_axis_length)
    minor = float(p.minor_axis_length)
    ar = major / minor if minor > 0 else 1.0
    return {
        'area': area,
        'perimeter': perim,
        'circularity': min(circ, 1.0),
        'eccentricity': float(p.eccentricity),
        'aspect_ratio': ar,
        'solidity': float(p.solidity),
        'extent': float(p.extent),
        'orientation': float(p.orientation),
        'major_axis_length': major,
        'minor_axis_length': minor,
        'centroid_x': float(p.centroid[1]),
        'centroid_y': float(p.centroid[0]),
    }


def render_color_overlay(img_array, masks, size_classes, output_path):
    """Render color-coded mask overlay and save."""
    overlay = img_array.copy().astype(np.float32)
    alpha = 0.45

    for i, sc in enumerate(size_classes):
        mask = masks[i]
        color = SIZE_COLORS_RGB.get(sc, SIZE_COLORS_RGB["Medium"])
        for ch in range(3):
            overlay[:, :, ch] = np.where(
                mask,
                overlay[:, :, ch] * (1 - alpha) + color[ch] * alpha,
                overlay[:, :, ch]
            )

    # Add legend
    overlay = add_legend(overlay.astype(np.uint8))
    Image.fromarray(overlay).save(output_path)


def add_legend(img):
    """Add size class legend to bottom-right."""
    from PIL import ImageDraw, ImageFont
    img_pil = Image.fromarray(img)
    draw = ImageDraw.Draw(img_pil)

    box_size = 20
    padding = 12
    line_height = 28
    legend_w = 170
    legend_h = len(SIZE_LABELS) * line_height + padding * 2 + 24
    x0 = img.shape[1] - legend_w - padding
    y0 = img.shape[0] - legend_h - padding

    # Semi-transparent background
    bg = Image.new("RGBA", img_pil.size, (0, 0, 0, 0))
    bg_draw = ImageDraw.Draw(bg)
    bg_draw.rounded_rectangle([x0, y0, x0 + legend_w, y0 + legend_h], radius=8, fill=(0, 0, 0, 180))
    img_pil = Image.alpha_composite(img_pil.convert("RGBA"), bg)
    draw = ImageDraw.Draw(img_pil)

    try:
        font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 15)
        font_sm = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 13)
    except (OSError, IOError):
        font = ImageFont.load_default()
        font_sm = font

    draw.text((x0 + padding, y0 + padding), "Size Class", fill=(255, 255, 255), font=font)
    for i, label in enumerate(SIZE_LABELS):
        y = y0 + padding + 24 + i * line_height
        draw.rectangle([x0 + padding, y, x0 + padding + box_size, y + box_size],
                       fill=SIZE_COLORS_RGB[label])
        draw.text((x0 + padding + box_size + 10, y + 2), label,
                  fill=(255, 255, 255), font=font_sm)

    return np.array(img_pil.convert("RGB"))


def main():
    import rfdetr

    os.makedirs(OUTPUT_ANNO_DIR, exist_ok=True)
    os.makedirs(OUTPUT_IMG_DIR, exist_ok=True)

    print("Loading RFDETRSegPreview model...")
    model = rfdetr.RFDETRSegPreview(pretrain_weights=SEG_MODEL_PATH, device="cpu")
    print("Model loaded.\n")

    all_summary = []
    all_objects = []
    sample_num = 0

    for (fabric, sample_name, fabric_dir), files in TILES.items():
        print(f"=== Fabric {fabric}, Sample {sample_name} ===")
        img_dir = os.path.join(LYONS_DIR, fabric_dir)

        for fname in files:
            img_path = os.path.join(img_dir, fname)
            if not os.path.exists(img_path):
                print(f"  SKIP: {fname}")
                continue

            img_pil = Image.open(img_path)
            img_array = np.array(img_pil)
            img_pixels = img_array.shape[0] * img_array.shape[1]

            # Run inference
            detections = model.predict(img_pil, threshold=0.2)
            n_det = len(detections.xyxy)
            if n_det == 0:
                print(f"  {fname}: 0 detections")
                continue

            masks = detections.mask  # (N, H, W) boolean
            bboxes = detections.xyxy  # (N, 4)

            # Extract morphology + size classes
            objects = []
            size_classes = []
            valid_areas = []

            for i in range(n_det):
                mask = masks[i]
                morph = extract_morphology(mask)

                if morph is None or morph['area'] < 1:
                    # Skip empty/invalid masks
                    continue

                sc = get_size_class(morph['area'])
                size_classes.append(sc)
                valid_areas.append(morph['area'])
                bbox = bboxes[i]

                objects.append({
                    'file_name': fname,
                    'class_name': 'inclusion',
                    'object_number': len(objects) + 1,
                    'area': morph['area'],
                    'perimeter': morph['perimeter'],
                    'circularity': morph['circularity'],
                    'eccentricity': morph['eccentricity'],
                    'aspect_ratio': morph['aspect_ratio'],
                    'solidity': morph['solidity'],
                    'extent': morph['extent'],
                    'Centroid': f"({len(objects)}, {morph['centroid_x']:.1f}, {morph['centroid_y']:.1f})",
                    'BoundingBox': f"({len(objects)}, {int(bbox[0])}, {int(bbox[1])}, {len(objects)+1}, {int(bbox[2])}, {int(bbox[3])})",
                    'size_category': sc,
                })

            if not objects:
                print(f"  {fname}: no valid detections")
                continue

            n_valid = len(objects)
            all_objects.extend(objects)

            # Filter masks to only valid ones for overlay
            valid_mask_indices = []
            idx = 0
            for i in range(n_det):
                mask = masks[i]
                morph = extract_morphology(mask)
                if morph is not None and morph['area'] >= 1:
                    valid_mask_indices.append(i)
            valid_masks = masks[valid_mask_indices]

            # Summary
            total_area = sum(valid_areas)
            incl_pct = round(total_area / img_pixels * 100, 1)
            areas = np.array(valid_areas)

            sc_counts = {s: 0 for s in SIZE_LABELS}
            for sc in size_classes:
                sc_counts[sc] += 1

            circs = [o['circularity'] for o in objects if o['circularity'] is not None]
            ars = [o['aspect_ratio'] for o in objects if o['aspect_ratio'] is not None]
            sols = [o['solidity'] for o in objects if o['solidity'] is not None]

            sample_num += 1
            all_summary.append({
                'file_name': fname,
                'n_inclusions': n_valid,
                'total_inclusion_area': total_area,
                'mean_area': round(float(areas.mean()), 1),
                'median_area': round(float(np.median(areas)), 1),
                'sd_area': round(float(areas.std()), 1),
                'min_area': float(areas.min()),
                'max_area': float(areas.max()),
                'q25_area': round(float(np.percentile(areas, 25)), 1),
                'q75_area': round(float(np.percentile(areas, 75)), 1),
                'inclusion_pct': incl_pct,
                'sample_id': fname.replace('.jpg', '').split('_xp_')[1] if '_xp_' in fname else f"{sample_num:03d}",
                'display_name': f"Sample {sample_num}",
                'sample_name': sample_name,
                'fabric_type': fabric,
                'polarization': 'XPL',
                'mean_circularity': round(np.mean(circs), 3) if circs else 0,
                'mean_aspect_ratio': round(np.mean(ars), 3) if ars else 0,
                'mean_solidity': round(np.mean(sols), 3) if sols else 0,
                'Fine': sc_counts['Fine'],
                'Medium': sc_counts['Medium'],
                'Coarse': sc_counts['Coarse'],
                'Very Coarse': sc_counts['Very Coarse'],
                'Fine_pct': round(sc_counts['Fine'] / n_valid * 100, 1),
                'Medium_pct': round(sc_counts['Medium'] / n_valid * 100, 1),
                'Coarse_pct': round(sc_counts['Coarse'] / n_valid * 100, 1),
                'Very Coarse_pct': round(sc_counts['Very Coarse'] / n_valid * 100, 1),
            })

            # Copy original
            img_pil.save(os.path.join(OUTPUT_IMG_DIR, fname))

            # Render overlay
            anno_name = fname.replace('.jpg', '_result.jpg')
            render_color_overlay(img_array, valid_masks, size_classes,
                                os.path.join(OUTPUT_ANNO_DIR, anno_name))

            print(f"  {fname}: {n_valid} det, {incl_pct}% area, "
                  f"circ={np.mean(circs):.2f}, AR={np.mean(ars):.2f}")

    # Write CSVs
    summary_fields = list(all_summary[0].keys())
    with open(os.path.join(DATA_DIR, 'ground_truth_summary.csv'), 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=summary_fields)
        w.writeheader()
        w.writerows(all_summary)

    obj_fields = list(all_objects[0].keys())
    with open(os.path.join(DATA_DIR, 'ground_truth_objects.csv'), 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=obj_fields)
        w.writeheader()
        w.writerows(all_objects)

    print(f"\n=== DONE ===")
    print(f"Tiles: {len(all_summary)}")
    print(f"Objects: {len(all_objects)}")
    fabrics = {}
    for s in all_summary:
        fabrics[s['fabric_type']] = fabrics.get(s['fabric_type'], 0) + 1
    print(f"By fabric: {fabrics}")


if __name__ == "__main__":
    main()
