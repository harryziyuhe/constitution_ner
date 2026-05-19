import json
import re
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Any

# ---------- CONFIG ----------
SCRIPT_DIR = Path(__file__).resolve().parent
DATA_DIR = SCRIPT_DIR.parent / "data" / "annotations"
OUTPUT_FILE = SCRIPT_DIR.parent / "data" / "merged.jsonl"
ID_PATTERN = re.compile(r"^row-(\d+)$")
FILE_PATTERN = re.compile(r"(\d+)\s*-\s*(\d+).*\.jsonl$")
# ----------------------------


def extract_row_num(row_id: str) -> Optional[int]:
    """Extract numeric part from 'row-XXX'."""
    m = ID_PATTERN.match(row_id)
    return int(m.group(1)) if m else None


def extract_range_from_filename(path: Path) -> Optional[Tuple[int, int]]:
    """Extract (A, B) from filenames like '101 - 300 annotations.jsonl'."""
    m = FILE_PATTERN.match(path.name)
    if not m:
        return None
    return int(m.group(1)), int(m.group(2))


def iter_jsonl(path: Path):
    with path.open("r", encoding="utf-8") as f:
        for line in f:
            line = f.readline()
            if not line:
                break
            line = line.strip()
            if line:
                yield json.loads(line)


def load_jsonl(path: Path) -> List[Dict[str, Any]]:
    """Read entire JSONL file into a list of dicts."""
    records = []
    with path.open("r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                records.append(json.loads(line))
    return records


def main():
    files_with_ranges: List[Tuple[int, int, Path]] = []

    # Collect files + inferred ranges
    for path in DATA_DIR.glob("*.jsonl"):
        rng = extract_range_from_filename(path)
        if rng is None:
            raise ValueError(f"Filename does not match range pattern: {path.name}")
        files_with_ranges.append((rng[0], rng[1], path))

    if not files_with_ranges:
        raise RuntimeError(f"No JSONL files found in {DATA_DIR}")

    # Sort files by A (first number) just for consistent logging
    files_with_ranges.sort(key=lambda x: x[0])

    # Load all files once
    loaded_files = []
    for A, B, path in files_with_ranges:
        records = load_jsonl(path)
        loaded_files.append((A, B, path, records))
        print(f"{path.name}: loaded {len(records)} rows (declared {A}-{B})")

    # Choose base file as the one with the largest number of rows
    base_A, base_B, base_path, base_records = max(
        loaded_files, key=lambda x: len(x[3])
    )
    print(f"\nUsing base file: {base_path.name} with {len(base_records)} rows")

    # Build merged-by-id from base file
    merged_by_id: Dict[str, Dict[str, Any]] = {}
    for rec in base_records:
        row_id = rec.get("id")
        if not row_id:
            continue
        merged_by_id[row_id] = rec

    # Build index to preserve order later (sort by numeric part of id)
    all_ids = list(merged_by_id.keys())
    all_ids.sort(key=lambda rid: extract_row_num(rid) or 0)

    # Overlay entities from all files (including base itself is harmless)
    for A, B, path, records in loaded_files:
        updated_count = 0
        for rec in records:
            row_id = rec.get("id")
            if not row_id:
                continue

            row_num = extract_row_num(row_id)
            # Optional: use the filename range as a sanity check / guide
            if row_num is not None and not (A <= row_num <= B):
                # If a row falls outside the declared range, we skip it
                # (this should not happen in your current setup).
                continue

            if row_id not in merged_by_id:
                # Row not in base file – log and skip
                print(f"Warning: id {row_id} from {path.name} not found in base file.")
                continue

            entities = rec.get("entities") or []
            if entities:
                base_entities = merged_by_id[row_id].get("entities") or []
                if base_entities and base_entities != entities:
                    # Very unlikely per your description, but warn if conflict
                    print(
                        f"Conflict on {row_id} between {path.name} and base; "
                        f"overwriting base entities."
                    )
                merged_by_id[row_id]["entities"] = entities
                updated_count += 1

        print(f"{path.name}: applied entities to {updated_count} rows (range {A}-{B})")

    # Reconstruct merged list in id order
    merged: List[Dict[str, Any]] = [merged_by_id[row_id] for row_id in all_ids]

    # Write merged JSONL
    with OUTPUT_FILE.open("w", encoding="utf-8") as f:
        for rec in merged:
            f.write(json.dumps(rec, ensure_ascii=False) + "\n")

    print(f"\nWrote {len(merged)} total rows → {OUTPUT_FILE}")
    print("Length should match the base/original file.")


if __name__ == "__main__":
    main()
