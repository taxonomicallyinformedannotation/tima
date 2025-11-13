"""
SMILES Processing Pipeline

This module processes SMILES (Simplified Molecular Input Line Entry System)
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

# Constants ----

# Default processing parameters
DEFAULT_BATCH_SIZE = 1000
DEFAULT_PROGRESS_INTERVAL = 10000
DEFAULT_MAX_WORKERS = None  # Auto-detect

# Output CSV column headers
OUTPUT_COLUMNS = [
    "structure_smiles_initial",
    "structure_smiles",
    "structure_inchikey",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_smiles_no_stereo",
    "structure_xlogp",
]

# Logging configuration
DEFAULT_LOG_LEVEL = logging.INFO
LOG_FORMAT = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(DEFAULT_LOG_LEVEL)

# Add console handler if not already present
if not logger.handlers:
    console_handler = logging.StreamHandler(sys.stderr)
    console_handler.setLevel(DEFAULT_LOG_LEVEL)
    formatter = logging.Formatter(LOG_FORMAT)
    console_handler.setFormatter(formatter)
    logger.addHandler(console_handler)

# Disable RDKit warnings by default (can be re-enabled if needed)
RDLogger.DisableLog("rdApp.warning")


# Exception Classes ----


class SmilesProcessingError(Exception):
    """Base exception for SMILES processing errors."""
    pass


class FileValidationError(SmilesProcessingError):
    """Raised when input/output file validation fails."""
    pass


class MoleculeProcessingError(SmilesProcessingError):
    """Raised when individual molecule processing fails."""
    pass


# Validation Functions ----


def validate_input_file(input_file: str) -> Path:
    """
    Validate that input SMILES file exists and is readable.

    Args:
        input_file: Path to input SMILES file

    Returns:
        Path object for the validated file

    Raises:
        FileValidationError: If file doesn't exist or isn't readable
    """
    input_path = Path(input_file)

    if not input_path.exists():
        raise FileValidationError(
            f"Input SMILES file not found: {input_file}\n"
            f"Please verify the file path and ensure the file exists."
        )

    if not input_path.is_file():
        raise FileValidationError(
            f"Input path is not a file: {input_file}\n"
            f"Please provide a valid SMILES file path."
        )

    if not os.access(input_path, os.R_OK):
        raise FileValidationError(
            f"Input file is not readable: {input_file}\n"
            f"Please check file permissions."
        )

    logger.info(f"Input file validated: {input_file}")
    return input_path


def validate_output_file(output_file: str) -> Path:
    """
    Validate output file path and ensure directory exists.

    Args:
        output_file: Path to output CSV file

    Returns:
        Path object for the output file

    Raises:
        FileValidationError: If output directory doesn't exist or isn't writable
    """
    output_path = Path(output_file)

    # Create parent directory if it doesn't exist
    output_dir = output_path.parent
    if not output_dir.exists():
        try:
            output_dir.mkdir(parents=True, exist_ok=True)
            logger.info(f"Created output directory: {output_dir}")
        except OSError as e:
            raise FileValidationError(
                f"Failed to create output directory: {output_dir}\n"
                f"Error: {e}"
            )

    # Check if directory is writable
    if not os.access(output_dir, os.W_OK):
        raise FileValidationError(
            f"Output directory is not writable: {output_dir}\n"
            f"Please check directory permissions."
        )

    logger.info(f"Output file path validated: {output_file}")
    return output_path


def validate_processing_params(
    num_workers: Optional[int],
    batch_size: int,
    progress_interval: int,
) -> Tuple[int, int, int]:
    """
    Validate and adjust processing parameters.

    Args:
        num_workers: Number of parallel workers (None = auto-detect)
        batch_size: Number of molecules per batch
        progress_interval: Interval for progress reporting

    Returns:
        Tuple of (num_workers, batch_size, progress_interval)

    Raises:
        ValueError: If parameters are invalid
    """
    # Validate and determine number of workers
    if num_workers is None:
        # Auto-detect: 2x CPU count, capped at 32
        num_workers = min(32, (os.cpu_count() or 1) * 2)
        logger.debug(f"Auto-detected num_workers: {num_workers}")
    elif num_workers < 1:
        raise ValueError(
            f"num_workers must be >= 1, got: {num_workers}"
        )

    # Validate batch size
    if batch_size < 1:
        raise ValueError(
            f"batch_size must be >= 1, got: {batch_size}"
        )

    # Validate progress interval
    if progress_interval < 1:
        raise ValueError(
            f"progress_interval must be >= 1, got: {progress_interval}"
        )

    logger.info(
        f"Processing parameters: workers={num_workers}, "
        f"batch_size={batch_size}, progress_interval={progress_interval}"
    )

    return num_workers, batch_size, progress_interval


# Core Processing Functions ----


def process_molecule(mol: Any, original_smiles: str) -> Optional[List[Any]]:
    """
    Process a single molecule and compute its properties.

    This function is designed to be thread-safe and handles errors gracefully.

    Args:
        mol: RDKit molecule object
        original_smiles: Original SMILES string

    Returns:
        List of computed properties or None if processing fails:
        [original_smiles, smiles, inchikey, formula, exact_mass,
         smiles_no_stereo, xlogp]

    Note:
        Returns None instead of raising exceptions to allow batch processing
        to continue even if individual molecules fail.
    """
    if mol is None:
        logger.debug(f"Null molecule object for SMILES: {original_smiles}")
        return None

    try:
        # Compute canonical SMILES
        smiles = MolToSmiles(mol)

        # Compute InChI Key (unique molecular identifier)
        inchikey = MolToInchiKey(mol)

        # Compute molecular formula
        formula = CalcMolFormula(mol)

        # Compute exact mass
        exact_mass = ExactMolWt(mol)

        # Create copy and remove stereochemistry
        # Note: RemoveStereochemistry modifies in-place
        mol_no_stereo = mol  # Reference, will be modified
        RemoveStereochemistry(mol_no_stereo)
        smiles_no_stereo = MolToSmiles(mol_no_stereo)

        # Compute XLogP (octanol-water partition coefficient)
        xlogp = MolLogP(mol)

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
        # Log error but don't raise - allows batch processing to continue
        logger.warning(
            f"Failed to process SMILES '{original_smiles}': {e.__class__.__name__}: {e}"
        )
        return None


def open_output_file(output_file: Path):
    """
    Open output file with appropriate handler based on extension.

    Supports both plain CSV and gzip-compressed CSV files.

    Args:
        output_file: Path to output file

    Returns:
        File object (text mode, with newline handling)

    Raises:
        IOError: If file cannot be opened for writing
    """
    try:
        if str(output_file).endswith(".gz"):
            logger.debug(f"Opening gzip-compressed output: {output_file}")
            return gzip.open(output_file, "wt", newline="", encoding="utf-8")
        else:
            logger.debug(f"Opening plain text output: {output_file}")
            return open(output_file, "w", newline="", encoding="utf-8")
    except IOError as e:
        raise IOError(
            f"Failed to open output file '{output_file}' for writing: {e}"
        ) from e


# Main Processing Function ----


def process_smiles(
    input_smi_file: str,
    output_csv_file: str,
    num_workers: Optional[int] = DEFAULT_MAX_WORKERS,
    batch_size: int = DEFAULT_BATCH_SIZE,
    progress_interval: int = DEFAULT_PROGRESS_INTERVAL,
) -> int:
    """
    Process a SMILES file using optimized parallel processing.

    This function reads SMILES from an input file, computes molecular properties
    in parallel, and writes results to a CSV file. Processing is performed in
    batches to optimize memory usage and throughput.

    Args:
        input_smi_file: Path to input SMILES file (.smi format)
        output_csv_file: Path to output CSV file (.csv or .csv.gz)
        num_workers: Number of parallel workers (None = auto-detect)
        batch_size: Number of molecules to process per batch
        progress_interval: Report progress every N molecules

    Returns:
        Total number of molecules successfully processed

    Raises:
        FileValidationError: If input/output files are invalid
        ValueError: If processing parameters are invalid
        Exception: For other processing errors

    Examples:
        >>> # Process with default settings
        >>> count = process_smiles("input.smi", "output.csv")
        >>> print(f"Processed {count} molecules")

        >>> # Process with custom parallelism
        >>> count = process_smiles(
        ...     "input.smi",
        ...     "output.csv.gz",
        ...     num_workers=8,
        ...     batch_size=500
        ... )
    """
    logger.info(f"Starting SMILES processing pipeline")
    logger.info(f"Input: {input_smi_file}")
    logger.info(f"Output: {output_csv_file}")

    # Validate inputs
    input_path = validate_input_file(input_smi_file)
    output_path = validate_output_file(output_csv_file)
    num_workers, batch_size, progress_interval = validate_processing_params(
        num_workers, batch_size, progress_interval
    )

    # Load SMILES supplier
    try:
        supplier = SmilesMolSupplier(str(input_path), nameColumn=-1)
        logger.info("SMILES supplier initialized")
    except Exception as e:
        raise SmilesProcessingError(
            f"Failed to load SMILES file '{input_smi_file}': {e}"
        ) from e

    # Process molecules and write output
    molecules_processed = 0

    try:
        with open_output_file(output_path) as f:
            writer = csv.writer(f)

            # Write header row
            writer.writerow(OUTPUT_COLUMNS)
            logger.debug("CSV header written")

            # Batch processing with thread pool
            batch = []
            original_smiles_list = []

            with ThreadPoolExecutor(max_workers=num_workers) as executor:
                for i, mol in enumerate(supplier):
                    if mol is not None:
                        batch.append(mol)
                        # Get original SMILES from supplier
                        original_smiles_list.append(supplier.GetItemText(i))

                    # Process batch when full
                    if len(batch) >= batch_size:
                        molecules_processed += process_batch(
                            executor,
                            batch,
                            original_smiles_list,
                            writer,
                            molecules_processed,
                            progress_interval,
                        )
                        # Clear batch for next iteration
                        batch.clear()
                        original_smiles_list.clear()

                # Process remaining molecules in final batch
                if batch:
                    molecules_processed += process_batch(
                        executor,
                        batch,
                        original_smiles_list,
                        writer,
                        molecules_processed,
                        progress_interval,
                    )

    except Exception as e:
        logger.error(f"Processing failed: {e}")
        raise

    logger.info(f"Processing complete. Total molecules processed: {molecules_processed}")
    return molecules_processed


def process_batch(
    executor: ThreadPoolExecutor,
    batch: List[Any],
    original_smiles_list: List[str],
    writer: csv.writer,
    current_count: int,
    progress_interval: int,
) -> int:
    """
    Process a batch of molecules in parallel.

    Args:
        executor: Thread pool executor for parallel processing
        batch: List of RDKit molecule objects
        original_smiles_list: List of original SMILES strings
        writer: CSV writer object
        current_count: Current count of processed molecules
        progress_interval: Interval for progress reporting

    Returns:
        Number of molecules successfully processed in this batch
    """
    batch_processed = 0

    # Process batch in parallel
    for result in executor.map(process_molecule, batch, original_smiles_list):
        if result is not None:
            writer.writerow(result)
            batch_processed += 1

            # Report progress
            total_processed = current_count + batch_processed
            if total_processed % progress_interval == 0:
                logger.info(f"Processed {total_processed} molecules")

    return batch_processed


# Entry Point ----


def main():
    """
    Command-line entry point for SMILES processing.

    Usage:
        python process_smiles.py input.smi output.csv [num_workers] [batch_size]
    """
    import argparse

    parser = argparse.ArgumentParser(
        description="Process SMILES file to compute molecular properties",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )

    parser.add_argument(
        "input_file",
        help="Input SMILES file path (.smi)",
    )

    parser.add_argument(
        "output_file",
        help="Output CSV file path (.csv or .csv.gz)",
    )

    parser.add_argument(
        "-w",
        "--workers",
        type=int,
        default=DEFAULT_MAX_WORKERS,
        help="Number of parallel workers (default: auto-detect)",
    )

    parser.add_argument(
        "-b",
        "--batch-size",
        type=int,
        default=DEFAULT_BATCH_SIZE,
        help="Batch size for processing",
    )

    parser.add_argument(
        "-p",
        "--progress-interval",
        type=int,
        default=DEFAULT_PROGRESS_INTERVAL,
        help="Progress reporting interval",
    )

    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose logging",
    )

    args = parser.parse_args()

    # Configure logging based on verbosity
    if args.verbose:
        logger.setLevel(logging.DEBUG)
        for handler in logger.handlers:
            handler.setLevel(logging.DEBUG)

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


if __name__ == "__main__":
    main()

