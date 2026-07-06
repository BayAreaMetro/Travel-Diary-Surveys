"""Compute Interquartile Range (IQR)-based summaries for weights.

Reference for weight trimming:
  https://nhts.ornl.gov/assets/2022/doc/2022%20NextGen%20NHTS%20Weighting%20Memo.pdf

Example:
    python calc_weight_iqr.py --data-dir 'X:\survey_repos\ProjRoot_Tue-Thu20260616\WgtRoot_Tue-Thu20260616_nocommuteweighting\output\full_weighted_dataset' --output-dir 'E:\Box\Modeling and Surveys\Surveys\Travel Diary Survey\BATS_2023\MTC_SFCTA_VTA Travel Diary Discussion\Data Review\Examine_large_weights' --datasets hh person trip
"""

import argparse
import csv
from pathlib import Path
from statistics import quantiles

DATASET_CONFIG = {
    "hh": {
        "csv": "hh.csv",
        "column": "hh_weight",
    },
    "person": {
        "csv": "person.csv",
        "column": "person_weight",
    },
    "trip": {
        "csv": "trip.csv",
        "column": "trip_weight",
    },
}

def read_values(csv_path: Path, column: str) -> tuple[int, int, list[float]]:
    total_rows = 0
    nonblank_rows = 0
    values: list[float] = []
    with csv_path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            total_rows += 1
            raw = row.get(column, "").strip()
            if not raw:
                continue
            nonblank_rows += 1
            value = float(raw)
            if value == 0:
                continue
            values.append(value)
    if not values:
        raise ValueError(f"No numeric values found in column {column!r} of {csv_path}")
    return total_rows, nonblank_rows, values


def process_dataset(dataset: str, csv_path: Path, column: str, output_path: Path) -> None:
    total_rows, nonblank_rows, values = read_values(csv_path, column)
    q1, q2, q3 = quantiles(values, n=4, method="inclusive")
    iqr = q3 - q1
    max_weight = max(values)
    threshold = q3 + 3 * iqr
    above_threshold = [value for value in values if value > threshold]
    above_count = len(above_threshold)
    above_sum = sum(above_threshold)

    report_lines = [
        f"dataset: {dataset}",
        f"file: {csv_path}",
        f"column: {column}",
        f"rows: {total_rows}",
        f"nonblank_rows: {nonblank_rows}",
        f"nonzero_nonblank_rows: {len(values)}",
        f"q1: {q1}",
        f"median: {q2}",
        f"q3: {q3}",
        f"iqr: {iqr}",
        f"max_weight: {max_weight}",
        f"threshold: {threshold}",
        f"count_above_threshold: {above_count}",
        f"sum_above_threshold: {above_sum}",
    ]

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text("\n".join(report_lines) + "\n", encoding="utf-8")

    for line in report_lines:
        print(line)
    print(f"saved: {output_path}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Compute the IQR for one or more weight columns.",
    )
    parser.add_argument(
        "--dataset",
        choices=tuple(DATASET_CONFIG),
        default="hh",
        help="Which dataset defaults to use",
    )
    parser.add_argument(
        "--datasets",
        nargs="+",
        choices=tuple(DATASET_CONFIG),
        help="Run multiple datasets in one command",
    )
    parser.add_argument(
        "--data-dir",
        type=Path,
        help="Directory containing hh.csv, person.csv, and trip.csv",
    )
    parser.add_argument("--csv", type=Path, help="Override the dataset CSV path")
    parser.add_argument("--column", help="Override the weight column")
    parser.add_argument(
        "--output-dir",
        type=Path,
        required=True,
        help="Directory to save the results reports",
    )
    args = parser.parse_args()

    datasets = args.datasets or [args.dataset]

    for dataset in datasets:
        dataset_defaults = DATASET_CONFIG[dataset]
        if args.csv is not None:
            csv_path = args.csv
        else:
            if args.data_dir is None:
                parser.error("--data-dir is required unless --csv is provided")
            csv_path = args.data_dir / dataset_defaults["csv"]
        column = args.column or dataset_defaults["column"]
        output_path = args.output_dir / f"{dataset}_weight_iqr_results.txt"
        process_dataset(dataset, csv_path, column, output_path)


if __name__ == "__main__":
    main()