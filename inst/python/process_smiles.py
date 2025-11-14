"""
SMILES Processing Pipeline

Processes SMILES (Simplified Molecular Input Line Entry System)
chemical structure strings using RDKit to compute molecular properties.

Author: TIMA Development Team
License: AGPL (>= 3)
"""

import csv
import gzip
import logging
import os
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import List, Optional, Tuple, Any

from rdkit import RDLogger
from rdkit.Chem import (
    SmilesMolSupplier,
    MolToSmiles,
    MolToInchiKey,
    RemoveStereochemistry,
)
from rdkit.Chem.Descriptors import ExactMolWt, MolLogP
from rdkit.Chem.rdMolDescriptors import CalcMolFormula


# =====================================================================================
# Configuration and Constants
# =====================================================================================

DEFAULT_BATCH_SIZE = 1000
DEFAULT_PROGRESS_INTERVAL = 10000
DEFAULT_MAX_WORKERS = None  # Auto-detect

OUTPUT_COLUMNS = [
    "structure_smiles_initial",
    "structure_smiles",
    "structure_inchikey",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_smiles_no_stereo",
    "structure_xlogp",
]

DEFAULT_LOG_LEVEL = logging.INFO
LOG_FORMAT = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"

logger = logging.getLogger(__name__)
logger.setLevel(DEFAULT_LOG_LEVEL)
if not logger.handlers:
    console_handler = logging.StreamHandler(sys.stderr)
    console_handler.setFormatter(logging.Formatter(LOG_FORMAT))
    console_handler.setLevel(DEFAULT_LOG_LEVEL)
    logger.addHandler(console_handler)

RDLogger.DisableLog("rdApp.warning")


# =====================================================================================
# Custom Exceptions
# =====================================================================================

class SmilesProcessingError(Exception):
    """Base exception for SMILES processing errors."""


class FileValidationError(SmilesProcessingError):
    """Raised when input/output file validation fails."""


class MoleculeProcessingError(SmilesProcessingError):
    """Raised when individual molecule processing fails."""


# =====================================================================================
# Validation Functions
# =====================================================================================

def validate_input_file(input_file: str) -> Path:
    path = Path(input_file)
    if not path.exists():
        raise FileValidationError(f"Input SMILES file not found: {input_file}")
    if not path.is_file():
        raise FileValidationError(f"Input path is not a file: {input_file}")
    if not os.access(path, os.R_OK):
        raise FileValidationError(f"Input file not readable: {input_file}")
    logger.info(f"Input file validated: {input_file}")
    return path


def validate_output_file(output_file: str) -> Path:
    path = Path(output_file)
    out_dir = path.parent
    if not out_dir.exists():
        out_dir.mkdir(parents=True, exist_ok=True)
        logger.info(f"Created output directory: {out_dir}")
    if not os.access(out_dir, os.W_OK):
        raise FileValidationError(f"Output directory not writable: {out_dir}")
    logger.info(f"Output file validated: {output_file}")
    return path


def validate_processing_params(
    num_workers: Optional[int],
    batch_size: int,
    progress_interval: int,
) -> Tuple[int, int, int]:
    if num_workers is None:
        num_workers = min(32, (os.cpu_count() or 1) * 2)
    if num_workers < 1:
        raise ValueError("num_workers must be >= 1")
    if batch_size < 1:
        raise ValueError("batch_size must be >= 1")
    if progress_interval < 1:
        raise ValueError("progress_interval must be >= 1")
    logger.info(
        f"Processing parameters: workers={num_workers}, batch_size={batch_size}, "
        f"progress_interval={progress_interval}"
    )
    return num_workers, batch_size, progress_interval


# =====================================================================================
# Core Molecule Processing
# =====================================================================================

def process_molecule(mol: Any, original_smiles: str) -> Optional[List[Any]]:
    """
    Process a single molecule to extract chemical properties.

    Args:
        mol: RDKit Mol object
        original_smiles: Original SMILES string

    Returns:
        List of molecular properties or None if processing fails
    """
    if mol is None:
        return None
    try:
        # Calculate all properties in one pass to minimize overhead
        smiles = MolToSmiles(mol)
        inchikey = MolToInchiKey(mol)
        formula = CalcMolFormula(mol)
        exact_mass = ExactMolWt(mol)
        xlogp = MolLogP(mol)

        # Create a copy for stereochemistry removal to avoid modifying original
        mol_no_stereo = mol.__copy__()
        RemoveStereochemistry(mol_no_stereo)
        smiles_no_stereo = MolToSmiles(mol_no_stereo)

        return [
            original_smiles,
            smiles,
            inchikey,
            formula,
            exact_mass,
            smiles_no_stereo,
            xlogp,
        ]
    except Exception as e:
        logger.warning(f"Failed to process SMILES '{original_smiles}': {e}")
        return None


def open_output_file(output_file: Path):
    """
    Open output file with appropriate compression if needed.

    Args:
        output_file: Path to output file

    Returns:
        File handle (text mode, UTF-8 encoded)
    """
    if str(output_file).endswith(".gz"):
        return gzip.open(output_file, "wt", newline="", encoding="utf-8")
    return open(output_file, "w", newline="", encoding="utf-8")


# =====================================================================================
# Batch and Pipeline Processing
# =====================================================================================

def process_batch(
    executor: ThreadPoolExecutor,
    batch: List[Any],
    original_smiles_list: List[str],
    writer: csv.writer,
    current_count: int,
    progress_interval: int,
) -> int:
    """
    Process a batch of molecules in parallel and write results.

    Args:
        executor: ThreadPoolExecutor for parallel processing
        batch: List of RDKit Mol objects
        original_smiles_list: Corresponding original SMILES strings
        writer: CSV writer object
        current_count: Current count of processed molecules
        progress_interval: Logging interval

    Returns:
        Number of successfully processed molecules in this batch
    """
    batch_processed = 0
    # Use map for efficient parallel processing with minimal overhead
    results = executor.map(process_molecule, batch, original_smiles_list)

    for result in results:
        if result:
            writer.writerow(result)
            batch_processed += 1
            total = current_count + batch_processed
            if total % progress_interval == 0:
                logger.info(f"Processed {total} molecules")

    return batch_processed


def process_smiles(
    input_smi_file: str,
    output_csv_file: str,
    num_workers: Optional[int] = DEFAULT_MAX_WORKERS,
    batch_size: int = DEFAULT_BATCH_SIZE,
    progress_interval: int = DEFAULT_PROGRESS_INTERVAL,
) -> int:
    """
    Process SMILES file to compute molecular properties.

    This function reads a SMILES file, processes molecules in parallel batches,
    and outputs molecular properties to a CSV file.

    Args:
        input_smi_file: Path to input SMILES file (.smi)
        output_csv_file: Path to output CSV file (.csv or .csv.gz)
        num_workers: Number of worker threads (None = auto-detect)
        batch_size: Number of molecules to process per batch
        progress_interval: Log progress every N molecules

    Returns:
        Total number of successfully processed molecules

    Raises:
        FileValidationError: If input/output files are invalid
        SmilesProcessingError: If SMILES processing fails
    """
    logger.info("Starting SMILES processing pipeline")
    logger.info(f"Input: {input_smi_file}")
    logger.info(f"Output: {output_csv_file}")

    input_path = validate_input_file(input_smi_file)
    output_path = validate_output_file(output_csv_file)
    num_workers, batch_size, progress_interval = validate_processing_params(
        num_workers, batch_size, progress_interval
    )

    try:
        supplier = SmilesMolSupplier(str(input_path), nameColumn=-1)
        logger.info("SMILES supplier initialized")
    except Exception as e:
        raise SmilesProcessingError(f"Failed to load SMILES file: {e}") from e

    molecules_processed = 0

    with open_output_file(output_path) as f:
        writer = csv.writer(f)
        writer.writerow(OUTPUT_COLUMNS)

        # Pre-allocate lists with capacity to reduce reallocation overhead
        batch = []
        originals = []

        with ThreadPoolExecutor(max_workers=num_workers) as executor:
            for i, mol in enumerate(supplier):
                if mol:
                    batch.append(mol)
                    originals.append(supplier.GetItemText(i))

                # Process batch when full
                if len(batch) >= batch_size:
                    molecules_processed += process_batch(
                        executor, batch, originals, writer,
                        molecules_processed, progress_interval
                    )
                    # Clear lists efficiently
                    batch.clear()
                    originals.clear()

            # Process remaining molecules
            if batch:
                molecules_processed += process_batch(
                    executor, batch, originals, writer,
                    molecules_processed, progress_interval
                )

    logger.info(f"Processing complete. Total molecules processed: {molecules_processed}")
    return molecules_processed


# =====================================================================================
# Command-Line Interface (safe for reticulate)
# =====================================================================================

def main():
    """CLI entry point."""
    import argparse
    parser = argparse.ArgumentParser(
        description="Process SMILES file to compute molecular properties",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument("input_file", help="Input SMILES file path (.smi)")
    parser.add_argument("output_file", help="Output CSV file path (.csv or .csv.gz)")
    parser.add_argument("-w", "--workers", type=int, default=DEFAULT_MAX_WORKERS)
    parser.add_argument("-b", "--batch-size", type=int, default=DEFAULT_BATCH_SIZE)
    parser.add_argument("-p", "--progress-interval", type=int, default=DEFAULT_PROGRESS_INTERVAL)
    parser.add_argument("-v", "--verbose", action="store_true", help="Enable verbose logging")

    args = parser.parse_args()

    if args.verbose:
        logger.setLevel(logging.DEBUG)
        for h in logger.handlers:
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
        sys.exit(0)
    except Exception as e:
        logger.error(f"SMILES processing failed: {e}")
        sys.exit(1)


# =====================================================================================
# Entry Point Guard (safe for reticulate)
# =====================================================================================

# When imported by R or another Python module, `__name__` != "__main__" — this prevents argparse from running.
# When executed directly from the command line, it behaves as a proper CLI.
if __name__ == "__main__" and len(sys.argv) > 1:
    main()
