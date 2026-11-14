"""SMILES processing pipeline using RDKit and ChEMBL structure pipeline."""

import argparse
import csv
import datetime
import gzip
import logging
import os
import sys
from collections.abc import Iterable
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any

from chembl_structure_pipeline import standardizer
from rdkit import rdBase
from rdkit.Chem import (
    Mol,
    MolToInchiKey,
    MolToSmiles,
    RemoveStereochemistry,
    SmilesMolSupplier,
)
from rdkit.Chem.rdMolDescriptors import (
    CalcCrippenDescriptors,
    CalcExactMolWt,
    CalcMolFormula,
)

DEFAULT_BATCH_SIZE = 1000
DEFAULT_PROGRESS_INTERVAL = 10000
DEFAULT_MAX_WORKERS: int | None = None

OUTPUT_COLUMNS: list[str] = [
    "structure_smiles_initial",
    "structure_smiles",
    "structure_inchikey",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_smiles_no_stereo",
    "structure_xlogp",
]


def _resolve_log_level_from_env(default: int = logging.INFO) -> int:
    """Resolve TIMA log level from the TIMA_LOG_LEVEL environment variable."""
    level_name = os.getenv("TIMA_LOG_LEVEL", "INFO")
    if not isinstance(level_name, str):
        return default
    level_name = level_name.strip().upper()
    level_map = {
        "TRACE": logging.DEBUG,
        "DEBUG": logging.DEBUG,
        "INFO": logging.INFO,
        "WARN": logging.WARNING,
        "WARNING": logging.WARNING,
        "ERROR": logging.ERROR,
        "FATAL": logging.CRITICAL,
        "CRITICAL": logging.CRITICAL,
    }
    return level_map.get(level_name, default)


DEFAULT_LOG_LEVEL = _resolve_log_level_from_env()
LOG_FORMAT = "[%(asctime)s] [%(levelname)-5s] %(message)s"
LOG_DATE_FORMAT = "%Y-%m-%d %H:%M:%S"


class MillisecondFormatter(logging.Formatter):
    """Log formatter that appends millisecond precision to timestamps."""

    def formatTime(self, record: logging.LogRecord, datefmt: str | None = None) -> str:
        """Format log record timestamp with local timezone and milliseconds."""
        ct = datetime.datetime.fromtimestamp(
            record.created, tz=datetime.timezone.utc
        ).astimezone()
        if datefmt:
            s = ct.strftime(datefmt)
        else:
            s = ct.strftime("%Y-%m-%d %H:%M:%S")
        return f"{s}.{int(record.msecs):03d}"


__all__ = [
    "process_molecule",
    "process_smiles",
    "validate_input_file",
    "validate_output_file",
    "validate_processing_params",
]


def get_logger(name: str = __name__, level: int = DEFAULT_LOG_LEVEL) -> logging.Logger:
    """Create or retrieve a logger writing formatted timestamps to stderr."""
    configured_logger = logging.getLogger(name)
    configured_logger.setLevel(level)
    if not configured_logger.handlers:
        console_handler = logging.StreamHandler(sys.stderr)
        formatter = MillisecondFormatter(LOG_FORMAT, datefmt=LOG_DATE_FORMAT)
        console_handler.setFormatter(formatter)
        console_handler.setLevel(level)
        configured_logger.addHandler(console_handler)
    return configured_logger


logger = get_logger()

# Silence noisy RDKit parser warnings unless a caller re-enables them.
rdBase.DisableLog("rdApp.warning")


class SmilesProcessingError(Exception):
    """Base exception for SMILES processing errors."""


class FileValidationError(SmilesProcessingError):
    """Raised when input or output file validation fails."""


class MoleculeProcessingError(SmilesProcessingError):
    """Raised when individual molecule processing fails."""


def validate_input_file(
    input_file: str, *, _logger: logging.Logger | None = None
) -> Path:
    """Validate that the input SMILES path exists and is a readable file."""
    path = Path(input_file)
    if not path.exists():
        raise FileValidationError(f"Input SMILES file not found: {input_file}")
    if not path.is_file():
        raise FileValidationError(f"Input path is not a file: {input_file}")
    if not os.access(path, os.R_OK):
        raise FileValidationError(f"Input file not readable: {input_file}")
    (_logger or logger).info(f"Input file validated: {input_file}")
    return path


def validate_output_file(
    output_file: str, *, _logger: logging.Logger | None = None
) -> Path:
    """Ensure the parent directory of the output path exists and is writable."""
    path = Path(output_file)
    out_dir = path.parent
    if not out_dir.exists():
        out_dir.mkdir(parents=True, exist_ok=True)
        (_logger or logger).info(f"Created output directory: {out_dir}")
    if not os.access(out_dir, os.W_OK):
        raise FileValidationError(f"Output directory not writable: {out_dir}")
    (_logger or logger).info(f"Output file validated: {output_file}")
    return path


def validate_processing_params(
    num_workers: int | None,
    batch_size: int,
    progress_interval: int,
    *,
    _logger: logging.Logger | None = None,
) -> tuple[int, int, int]:
    """Validate worker count, batch size, and progress reporting interval."""
    if num_workers is None:
        num_workers = min(32, (os.cpu_count() or 1) * 2)
    if num_workers < 1:
        raise ValueError("num_workers must be >= 1")
    if batch_size < 1:
        raise ValueError("batch_size must be >= 1")
    if progress_interval < 1:
        raise ValueError("progress_interval must be >= 1")
    (_logger or logger).info(
        f"Processing parameters: workers={num_workers}, batch_size={batch_size}, "
        f"progress_interval={progress_interval}"
    )
    return num_workers, batch_size, progress_interval


def process_molecule(mol: Any, original_smiles: str) -> list[Any] | None:
    """Standardize a molecule clone and compute SMILES, InChIKey, formula, mass, and xLogP.

    Isotope notation is preserved in SMILES, InChIKey, separated-isotope formulas,
    and exact molecular weight calculations.
    """
    if mol is None:
        return None

    try:
        # Clone before standardizing/stripping stereo so callers keep their Mol intact.
        m = standardizer.standardize_mol(Mol(mol))

        smiles = MolToSmiles(m)
        inchikey = MolToInchiKey(m)
        formula = CalcMolFormula(m, separateIsotopes=True)
        exact_mass = CalcExactMolWt(m)
        xlogp, _ = CalcCrippenDescriptors(m)

        RemoveStereochemistry(m)
        smiles_no_stereo = MolToSmiles(m)

        return [
            original_smiles,
            smiles,
            inchikey,
            formula,
            exact_mass,
            smiles_no_stereo,
            xlogp,
        ]
    except (RuntimeError, ValueError, TypeError) as e:
        logger.warning(f"Failed to process SMILES '{original_smiles}': {e}")
        return None


def open_output_file(output_file: Path):
    """Open a CSV or gzipped CSV output stream in UTF-8 text mode."""
    if str(output_file).endswith(".gz"):
        return gzip.open(output_file, "wt", newline="", encoding="utf-8")
    return open(output_file, "w", newline="", encoding="utf-8")


def _process_molecule_wrapper(args: tuple[Any, str]) -> list[Any] | None:
    """Unpack a `(mol, smiles)` tuple for `Executor.map`."""
    mol, smiles = args
    return process_molecule(mol, smiles)


def process_batch(
    executor: ThreadPoolExecutor,
    batch: Iterable[Any],
    original_smiles_list: Iterable[str],
    writer: Any,
    current_count: int,
    progress_interval: int,
    *,
    _logger: logging.Logger | None = None,
) -> int:
    """Process a batch of molecules in the executor and write rows on the caller thread."""
    log = _logger or logger
    batch_processed = 0
    batch_failed = 0

    pairs = list(zip(batch, original_smiles_list))
    results = list(executor.map(_process_molecule_wrapper, pairs))

    for result in results:
        if result:
            writer.writerow(result)
            batch_processed += 1
            total = current_count + batch_processed
            if total % progress_interval == 0:
                log.info(f"Processed {total} molecules")
        else:
            batch_failed += 1

    if batch_failed > 0:
        log.warning(f"Batch processing: {batch_failed}/{len(pairs)} molecules failed")

    return batch_processed


def process_smiles(
    input_smi_file: str,
    output_csv_file: str,
    num_workers: int | None = DEFAULT_MAX_WORKERS,
    batch_size: int = DEFAULT_BATCH_SIZE,
    progress_interval: int = DEFAULT_PROGRESS_INTERVAL,
    *,
    _logger: logging.Logger | None = None,
) -> int:
    """Read `input_smi_file`, compute molecular descriptors in batches, and write CSV output."""
    log = _logger or logger
    log.info("Starting SMILES processing pipeline")
    log.info(f"Input: {input_smi_file}")
    log.info(f"Output: {output_csv_file}")

    input_path = validate_input_file(input_smi_file, _logger=log)
    output_path = validate_output_file(output_csv_file, _logger=log)
    num_workers, batch_size, progress_interval = validate_processing_params(
        num_workers, batch_size, progress_interval, _logger=log
    )

    try:
        supplier = SmilesMolSupplier(str(input_path), nameColumn=-1)
        log.info("SMILES supplier initialized")
    except (OSError, RuntimeError, ValueError) as e:
        raise SmilesProcessingError(f"Failed to load SMILES file: {e}") from e

    molecules_processed = 0

    with open_output_file(output_path) as f:
        writer = csv.writer(f)
        writer.writerow(OUTPUT_COLUMNS)

        batch: list[Any] = []
        originals: list[str] = []

        with ThreadPoolExecutor(max_workers=num_workers) as executor:
            for i, mol in enumerate(supplier):
                if mol:
                    batch.append(mol)
                    originals.append(supplier.GetItemText(i))

                    if len(batch) >= batch_size:
                        molecules_processed += process_batch(
                            executor,
                            batch,
                            originals,
                            writer,
                            molecules_processed,
                            progress_interval,
                            _logger=log,
                        )
                        batch.clear()
                        originals.clear()

            if batch:
                molecules_processed += process_batch(
                    executor,
                    batch,
                    originals,
                    writer,
                    molecules_processed,
                    progress_interval,
                    _logger=log,
                )

    log.info(f"Processing complete. Total molecules processed: {molecules_processed}")
    return molecules_processed


def main(argv: list[str] | None = None) -> int:
    """CLI entry point returning 0 on success or 1 on processing error."""
    parser = argparse.ArgumentParser(
        description="Process SMILES file to compute molecular properties",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument("input_file", help="Input SMILES file path (.smi)")
    parser.add_argument("output_file", help="Output CSV file path (.csv or .csv.gz)")
    parser.add_argument("-w", "--workers", type=int, default=DEFAULT_MAX_WORKERS)
    parser.add_argument("-b", "--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    parser.add_argument(
        "-p", "--progress-interval", type=int, default=DEFAULT_PROGRESS_INTERVAL
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable verbose logging"
    )

    args = parser.parse_args(argv)

    if args.verbose:
        dbg = get_logger(__name__, level=logging.DEBUG)
        for h in dbg.handlers:
            h.setLevel(logging.DEBUG)

    try:
        count = process_smiles(
            input_smi_file=args.input_file,
            output_csv_file=args.output_file,
            num_workers=args.workers,
            batch_size=args.batch_size,
            progress_interval=args.progress_interval,
        )
        print(f"Success! Processed {count} molecules.")
        return 0
    except (SmilesProcessingError, OSError, RuntimeError, ValueError) as e:
        get_logger().error(f"SMILES processing failed: {e}")
        return 1


# Guarded so importing via reticulate::source_python() does not invoke argparse.
if __name__ == "__main__" and len(sys.argv) > 1:
    sys.exit(main())
