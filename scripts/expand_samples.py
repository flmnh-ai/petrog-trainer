"""Expand the sample pool with tiles from additional samples and polarization types.

Adds:
- 139-45 and 90-53 (Fabric C, XPL) from predictions.json
- NPP shell-tempered tiles (PPL and XPL) from training annotations

Generates:
- Updated ground_truth_summary.csv and ground_truth_objects.csv
- Copies images to www/images/original/
"""

import json
import csv
import os
import re
import shutil
import numpy as np
from collections import defaultdict

# Paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(SCRIPT_DIR)
PETROGRAPHER = os.path.expanduser("~/Dropbox/Projects/petrographer")

PREDICTIONS_JSON = os.path.join(PETROGRAPHER, "vignettes/results/lyons/predictions.json")
RAW_LYONS = os.path.join(PETROGRAPHER, "data/raw/Lyons_data")
NPP_TRAIN_JSON = os.path.join(PETROGRAPHER, "data/processed/inclusions/train/_annotations.coco.json")
NPP_TRAIN_DIR = os.path.join(PETROGRAPHER, "data/processed/inclusions/train")
NPP_VALID_JSON = os.path.join(PETROGRAPHER, "data/processed/inclusions/valid/_annotations.coco.json")
NPP_VALID_DIR = os.path.join(PETROGRAPHER, "data/processed/inclusions/valid")

OUTPUT_DIR = os.path.join(PROJECT_DIR, "www/images/original")
DATA_DIR = os.path.join(PROJECT_DIR, "data")

# Existing 8-587 tiles (keep these — they use output_objects.csv calibration)
EXISTING_8587 = [
    "8-587_xp_000016.jpg", "8-587_xp_000019.jpg", "8-587_xp_000029.jpg",
    "8-587_xp_000030.jpg", "8-587_xp_000032.jpg", "8-587_xp_000033.jpg",
    "8-587_xp_000034.jpg", "8-587_xp_000036.jpg", "8-587_xp_000040.jpg",
    "8-587_xp_000043.jpg", "8-587_xp_000044.jpg", "8-587_xp_000046.jpg",
    "8-587_xp_000047.jpg", "8-587_xp_000048.jpg", "8-587_xp_000051.jpg",
]

# Size breaks for bbox areas (predictions.json uses bbox area, not mask area)
# These are calibrated to roughly match the output_objects.csv mask-area size classes
BBOX_SIZE_BREAKS = [0, 700, 3000, 12000, float('inf')]
SIZE_LABELS = ["Fine", "Medium", "Coarse", "Very Coarse"]

# Minimum bbox area to filter noise (calibrated to get 50-100 detections/image)
MIN_BBOX_AREA = 1500

IMG_AREA_LYONS = 2048 * 1536


def get_size_class(area, breaks=BBOX_SIZE_BREAKS):
    for i, (lo, hi) in enumerate(zip(breaks[:-1], breaks[1:])):
        if lo < area <= hi:
            return SIZE_LABELS[i]
    return SIZE_LABELS[-1]


def is_interior_tile(img_path):
    """Quick heuristic: check if corners have black pixels (edge indicator)."""
    try:
        from PIL import Image
        img = Image.open(img_path)
        w, h = img.size
        # Check 4 corners (20x20 pixel patches)
        corners = [
            img.crop((0, 0, 20, 20)),
            img.crop((w-20, 0, w, 20)),
            img.crop((0, h-20, 20, h)),
            img.crop((w-20, h-20, w, h)),
        ]
        for corner in corners:
            arr = np.array(corner)
            # If mean brightness < 30, it's likely epoxy/black background
            if arr.mean() < 30:
                return False
        return True
    except Exception:
        return True  # If can't check, include it


def process_lyons_predictions():
    """Process predictions.json for 139-45 and 90-53."""
    print("=== Processing Lyons predictions (139-45, 90-53) ===")

    with open(PREDICTIONS_JSON) as f:
        pred = json.load(f)

    id_to_img = {img['id']: img for img in pred['images']}

    # Group annotations by image
    anns_by_img = defaultdict(list)
    for ann in pred['annotations']:
        anns_by_img[ann['image_id']].append(ann)

    results = []
    all_objects = []

    for prefix, fabric_dir in [('139-45', 'c'), ('90-53', 'c')]:
        print(f"\n  {prefix} (Fabric C):")
        img_dir = os.path.join(RAW_LYONS, fabric_dir)

        # Get all images for this sample
        sample_imgs = [img for img in pred['images'] if img['file_name'].startswith(prefix)]
        sample_imgs.sort(key=lambda x: x['file_name'])

        selected = 0
        for img_info in sample_imgs:
            fname = img_info['file_name']
            img_path = os.path.join(img_dir, fname)

            if not os.path.exists(img_path):
                continue

            # Check interior
            if not is_interior_tile(img_path):
                continue

            # Filter annotations by area
            anns = [a for a in anns_by_img.get(img_info['id'], []) if a['area'] >= MIN_BBOX_AREA]

            if len(anns) < 30:  # Too few — might be edge/sparse
                continue

            # Compute stats
            areas = [a['area'] for a in anns]
            total_area = sum(areas)
            incl_pct = round(total_area / IMG_AREA_LYONS * 100, 1)

            # Size distribution
            size_counts = {s: 0 for s in SIZE_LABELS}
            for a in anns:
                sc = get_size_class(a['area'])
                size_counts[sc] += 1

            size_pcts = {s: round(size_counts[s] / len(anns) * 100, 1) for s in SIZE_LABELS}

            results.append({
                'file_name': fname,
                'n_inclusions': len(anns),
                'total_inclusion_area': total_area,
                'mean_area': round(np.mean(areas), 1),
                'median_area': round(np.median(areas), 1),
                'sd_area': round(np.std(areas), 1),
                'min_area': min(areas),
                'max_area': max(areas),
                'q25_area': round(np.percentile(areas, 25), 1),
                'q75_area': round(np.percentile(areas, 75), 1),
                'inclusion_pct': incl_pct,
                'sample_id': fname.replace('.jpg', '').split('_xp_')[1],
                'sample_name': prefix,
                'fabric_type': 'C',
                'polarization': 'XPL',
                **{f'{s}': size_counts[s] for s in SIZE_LABELS},
                **{f'{s}_pct': size_pcts[s] for s in SIZE_LABELS},
            })

            # Object-level data
            for i, a in enumerate(anns):
                bbox = a['bbox']  # [x, y, w, h]
                sc = get_size_class(a['area'])
                all_objects.append({
                    'file_name': fname,
                    'class_name': 'inclusion',
                    'object_number': i + 1,
                    'area': a['area'],
                    'Centroid': f"({i}, {bbox[0]+bbox[2]/2:.1f}, {bbox[1]+bbox[3]/2:.1f})",
                    'BoundingBox': f"({i}, {int(bbox[0])}, {int(bbox[1])}, {i+1}, {int(bbox[0]+bbox[2])}, {int(bbox[1]+bbox[3])})",
                    'size_category': sc,
                })

            # Copy image
            dest = os.path.join(OUTPUT_DIR, fname)
            shutil.copy2(img_path, dest)
            selected += 1

            if selected >= 8:  # 8 tiles per sample
                break

        print(f"    Selected {selected} interior tiles")

    return results, all_objects


def process_npp_tiles():
    """Process NPP training annotations for PPL and XPL tiles."""
    print("\n=== Processing NPP tiles (shell-tempered, PPL/XPL) ===")

    results = []
    all_objects = []

    for json_path, img_dir, split_name in [
        (NPP_TRAIN_JSON, NPP_TRAIN_DIR, 'train'),
        (NPP_VALID_JSON, NPP_VALID_DIR, 'valid'),
    ]:
        with open(json_path) as f:
            coco = json.load(f)

        id_to_img = {img['id']: img for img in coco['images']}

        anns_by_img = defaultdict(list)
        for ann in coco['annotations']:
            anns_by_img[ann['image_id']].append(ann)

        # Find good PPL and XPL tiles (not from Lyons samples)
        ppl_selected = 0
        xpl_selected = 0

        for img_info in sorted(coco['images'], key=lambda x: x['file_name']):
            fname = img_info['file_name']

            # Skip Lyons samples that got into NPP training set
            if any(fname.startswith(p) for p in ['105-27', '139-45', '15-604', '17-112',
                                                   '53-246', '8-521', '8-587', '94-18']):
                continue

            # Determine polarization
            if '_ppl_' in fname.lower() or '_ppl.' in fname.lower():
                pol = 'PPL'
                if ppl_selected >= 5:
                    continue
            elif '_xpl_' in fname.lower() or '_xp_' in fname.lower() or '_xp.' in fname.lower():
                pol = 'XPL'
                if xpl_selected >= 3:
                    continue
            else:
                continue

            img_path = os.path.join(img_dir, fname)
            if not os.path.exists(img_path):
                continue

            # Check not an edge tile
            if not is_interior_tile(img_path):
                continue

            # Get annotations
            anns = anns_by_img.get(img_info['id'], [])
            if len(anns) < 15:
                continue

            # Image dimensions
            img_w = img_info['width']
            img_h = img_info['height']
            img_pixels = img_w * img_h

            # Use mask-area size breaks for COCO annotations (they have proper areas)
            mask_breaks = [0, 200, 800, 3000, float('inf')]  # Adjusted for smaller tile size

            areas = [a['area'] for a in anns]
            total_area = sum(areas)
            incl_pct = round(total_area / img_pixels * 100, 1)

            if incl_pct < 3 or incl_pct > 60:
                continue

            size_counts = {s: 0 for s in SIZE_LABELS}
            for a in anns:
                sc = get_size_class(a['area'], breaks=mask_breaks)
                size_counts[sc] += 1

            n = len(anns)
            size_pcts = {s: round(size_counts[s] / n * 100, 1) for s in SIZE_LABELS}

            # Parse sample name from filename
            parts = fname.split('_')
            if 'clam' in fname or 'oyster' in fname:
                sample_name = '_'.join(parts[:2])  # e.g., "clam_NPP"
                temper_type = parts[0].title()
            elif fname.startswith('NPP'):
                sample_name = parts[0]
                temper_type = "Shell"
            else:
                sample_name = parts[0]
                temper_type = "Unknown"

            # Create a clean display filename
            clean_name = f"{sample_name}_{pol.lower()}_{ppl_selected if pol == 'PPL' else xpl_selected:03d}.jpg"

            results.append({
                'file_name': clean_name,
                'n_inclusions': n,
                'total_inclusion_area': total_area,
                'mean_area': round(np.mean(areas), 1),
                'median_area': round(np.median(areas), 1),
                'sd_area': round(np.std(areas), 1),
                'min_area': min(areas),
                'max_area': max(areas),
                'q25_area': round(np.percentile(areas, 25), 1),
                'q75_area': round(np.percentile(areas, 75), 1),
                'inclusion_pct': incl_pct,
                'sample_id': f"npp_{ppl_selected if pol == 'PPL' else xpl_selected:03d}",
                'sample_name': sample_name,
                'fabric_type': f'Shell ({temper_type})',
                'polarization': pol,
                **{f'{s}': size_counts[s] for s in SIZE_LABELS},
                **{f'{s}_pct': size_pcts[s] for s in SIZE_LABELS},
            })

            # Object-level data
            for i, a in enumerate(anns):
                bbox = a['bbox']
                sc = get_size_class(a['area'], breaks=mask_breaks)
                all_objects.append({
                    'file_name': clean_name,
                    'class_name': 'inclusion',
                    'object_number': i + 1,
                    'area': a['area'],
                    'Centroid': f"({i}, {bbox[0]+bbox[2]/2:.1f}, {bbox[1]+bbox[3]/2:.1f})",
                    'BoundingBox': f"({i}, {int(bbox[0])}, {int(bbox[1])}, {i+1}, {int(bbox[0]+bbox[2])}, {int(bbox[1]+bbox[3])})",
                    'size_category': sc,
                })

            # Copy image with clean name
            shutil.copy2(img_path, os.path.join(OUTPUT_DIR, clean_name))

            if pol == 'PPL':
                ppl_selected += 1
            else:
                xpl_selected += 1

        print(f"  {split_name}: {ppl_selected} PPL, {xpl_selected} XPL tiles selected")

    return results, all_objects


def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Load existing 8-587 data
    existing_summary = []
    with open(os.path.join(DATA_DIR, 'ground_truth_summary.csv')) as f:
        reader = csv.DictReader(f)
        for row in reader:
            row['sample_name'] = '8-587'
            row['fabric_type'] = 'C'
            row['polarization'] = 'XPL'
            existing_summary.append(row)

    existing_objects = []
    with open(os.path.join(DATA_DIR, 'ground_truth_objects.csv')) as f:
        reader = csv.DictReader(f)
        for row in reader:
            existing_objects.append(row)

    print(f"Existing 8-587 data: {len(existing_summary)} tiles, {len(existing_objects)} objects")

    # Process new data sources
    lyons_summary, lyons_objects = process_lyons_predictions()
    npp_summary, npp_objects = process_npp_tiles()

    # Combine all
    all_summary = existing_summary + lyons_summary + npp_summary
    all_objects = existing_objects + lyons_objects + npp_objects

    # Add display names
    for i, row in enumerate(all_summary):
        row['display_name'] = f"Sample {i + 1}"

    # Write combined CSVs
    summary_fields = ['file_name', 'n_inclusions', 'total_inclusion_area', 'mean_area',
                       'median_area', 'sd_area', 'min_area', 'max_area', 'q25_area', 'q75_area',
                       'inclusion_pct', 'sample_id', 'display_name', 'sample_name', 'fabric_type',
                       'polarization',
                       'Fine', 'Medium', 'Coarse', 'Very Coarse',
                       'Fine_pct', 'Medium_pct', 'Coarse_pct', 'Very Coarse_pct']

    with open(os.path.join(DATA_DIR, 'ground_truth_summary.csv'), 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=summary_fields, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(all_summary)

    object_fields = ['file_name', 'class_name', 'object_number', 'area', 'Centroid',
                      'BoundingBox', 'size_category']

    with open(os.path.join(DATA_DIR, 'ground_truth_objects.csv'), 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=object_fields, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(all_objects)

    print(f"\n=== TOTAL ===")
    print(f"Tiles: {len(all_summary)}")
    print(f"Objects: {len(all_objects)}")

    # Summary by source
    by_sample = defaultdict(int)
    by_pol = defaultdict(int)
    for row in all_summary:
        by_sample[row.get('sample_name', 'unknown')] += 1
        by_pol[row.get('polarization', 'XPL')] += 1

    print(f"\nBy sample: {dict(by_sample)}")
    print(f"By polarization: {dict(by_pol)}")


if __name__ == "__main__":
    main()
