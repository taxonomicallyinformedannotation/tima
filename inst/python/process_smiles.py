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
from chembl_structure_pipeline import standardizer
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import List, Optional, Tuple, Any, Iterable
from rdkit import RDLogger
from rdkit.Chem import (
    SmilesMolSupplier,
    MolToSmiles,
    MolToInchiKey,
    RemoveStereochemistry,
    Mol,
)
from rdkit.Chem.Descriptors import ExactMolWt, MolLogP
from rdkit.Chem.MolStandardize import rdMolStandardize
from rdkit.Chem.rdMolDescriptors import CalcMolFormula


# =====================================================================================
# Configuration and Constants
# =====================================================================================

DEFAULT_BATCH_SIZE = 1000
DEFAULT_PROGRESS_INTERVAL = 10000
DEFAULT_MAX_WORKERS: Optional[int] = None  # Auto-detect

OUTPUT_COLUMNS: List[str] = [
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

# Isotope mass differences (13C - 12C, etc.)
# These are used to calculate accurate masses for isotope-labeled compounds
# Keys are atomic numbers: H=1, C=6, N=7, O=8
ISOTOPE_MASS_SHIFTS = {
    1: 1.0078250321,  # Deuterium (2H - 1H)
    6: 1.0033548378,  # 13C - 12C  (Carbon atomic number = 6)
    7: 0.9963779,  # 15N - 14N  (Nitrogen atomic number = 7)
    8: 2.0042895,  # 18O - 16O  (Oxygen atomic number = 8)
}

__all__ = [
    "process_smiles",
    "process_molecule",
    "validate_input_file",
    "validate_output_file",
    "validate_processing_params",
    "parse_isotopes_from_smiles",
    "calculate_isotope_mass_shift",
]


def get_logger(name: str = __name__, level: int = DEFAULT_LOG_LEVEL) -> logging.Logger:
    """Create or retrieve a configured logger.

    This avoids global side effects and allows dependency injection for tests.
    """
    logger = logging.getLogger(name)
    logger.setLevel(level)
    if not logger.handlers:
        console_handler = logging.StreamHandler(sys.stderr)
        console_handler.setFormatter(logging.Formatter(LOG_FORMAT))
        console_handler.setLevel(level)
        logger.addHandler(console_handler)
    return logger


# Use module-level logger but allow injection into functions
logger = get_logger()

# Silence RDKit warnings (can be re-enabled by tests if needed)
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


def validate_input_file(
    input_file: str, *, _logger: Optional[logging.Logger] = None
) -> Path:
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
    output_file: str, *, _logger: Optional[logging.Logger] = None
) -> Path:
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
    num_workers: Optional[int],
    batch_size: int,
    progress_interval: int,
    *,
    _logger: Optional[logging.Logger] = None,
) -> Tuple[int, int, int]:
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


# =====================================================================================
# Isotope Handling Functions
# =====================================================================================


def parse_isotopes_from_smiles(smiles: str) -> dict:
    """
    Parse isotope information from SMILES string.

    Counts occurrences of isotope notation like [13C], [15N], [18O], [2H], etc.

    Args:
        smiles: SMILES string

    Returns:
        Dictionary mapping atomic number to count of that isotope
        Example: {'C': {13: 1}, 'N': {15: 1}} for [13C][15N]...
    """
    import re

    isotopes = {}

    # Pattern to match: [number+Element+optional_rest]
    # Examples: [13C], [13C@H], [15N], [18O], [2H], [13C@@H]
    # Matches: [digit(s) + uppercase letter + optional lowercase + anything else + ]
    pattern = r"\[(\d+)([A-Z][a-z]?)([^\]]*)\]"

    matches = list(re.finditer(pattern, smiles))
    logger.debug(f"Parsing '{smiles}' - found {len(matches)} isotope candidates")

    for match in matches:
        mass_num = int(match.group(1))
        element = match.group(2)
        rest = match.group(3)

        if element not in isotopes:
            isotopes[element] = {}
            logger.info(f"Created dict for element {element}")

        isotopes[element][mass_num] = isotopes[element].get(mass_num, 0) + 1
        logger.debug(
            f"Matched isotope [{mass_num}{element}{rest}] -> isotopes is now {isotopes}"
        )

    logger.debug(
        f"After loop, isotopes = {isotopes}, bool(isotopes) = {bool(isotopes)}"
    )
    if isotopes:
        logger.debug(f"Final isotopes dict: {isotopes}")
        return isotopes
    else:
        logger.debug(f"Returning empty dict")
        return isotopes


def calculate_isotope_mass_shift(original_smiles: str) -> float:
    """
    Calculate the mass shift due to isotope labeling in SMILES.

    Args:
        original_smiles: SMILES string potentially containing isotope notation

    Returns:
        Float: Total mass shift in Daltons from isotope substitution
    """
    logger.debug(f"calculate_isotope_mass_shift('{original_smiles[:50]}...')")

    isotopes = parse_isotopes_from_smiles(original_smiles)
    logger.debug(f"  Parsed isotopes: {isotopes}")

    if not isotopes:
        logger.debug(f"  No isotopes found, returning 0.0 Da")
        return 0.0

    total_shift = 0.0

    # Element atomic numbers for common isotopes
    element_to_atomic_num = {"H": 1, "C": 6, "N": 7, "O": 8}

    for element, isotope_counts in isotopes.items():
        atomic_num = element_to_atomic_num.get(element)

        if atomic_num is None:
            # Unknown element, skip
            logger.warning(f"  Unknown element: {element}")
            continue

        if atomic_num not in ISOTOPE_MASS_SHIFTS:
            # No shift data for this element
            logger.warning(
                f"  No mass shift data for {element} (atomic_num={atomic_num})"
            )
            continue

        iso_shift = ISOTOPE_MASS_SHIFTS[atomic_num]
        count = sum(isotope_counts.values())
        shift_contrib = iso_shift * count
        total_shift += shift_contrib
        logger.info(
            f"  {element}: {count} atoms × {iso_shift:.6f} Da = {shift_contrib:.6f} Da"
        )

    logger.info(f"  *** TOTAL ISOTOPE SHIFT: {total_shift:.6f} Da ***")
    return total_shift


# =====================================================================================
# Core Molecule Processing
# =====================================================================================


def _copy_molecule(mol: Mol) -> Mol:
    """Safely copy an RDKit molecule."""
    # Mol() copy constructor performs a deep copy
    try:
        return Mol(mol)
    except Exception:
        # Fallback to __copy__ if necessary
        return mol.__copy__()


def process_molecule(
    mol: Any, original_smiles: str, un: rdMolStandardize.Uncharger
) -> Optional[List[Any]]:
    """
    Process a single molecule to extract chemical properties.

    For isotope-labeled compounds, the exact mass is corrected by adding
    the isotope mass shift to account for heavier isotopes (e.g., 13C, 15N, 18O).

    Args:
        mol: RDKit Mol object
        original_smiles: Original SMILES string (used for isotope detection)
        un: rdMolStandardize.Uncharger

    Returns:
        List of molecular properties or None if processing fails
    """
    # CRITICAL: Calculate isotope shift FIRST from original SMILES
    # This must be done BEFORE any RDKit processing that might strip isotope info
    iso_shift = calculate_isotope_mass_shift(original_smiles)
    logger.debug(f"Processing: {original_smiles[:60]}... iso_shift={iso_shift:.6f} Da")

    if mol is None:
        logger.warning(
            f"process_molecule: received None mol for SMILES '{original_smiles}'"
        )
        return None
    try:
        m, _ = standardizer.get_parent_mol(mol)
        un.uncharge(m)
        smiles = MolToSmiles(m)
        inchikey = MolToInchiKey(m)
        formula = CalcMolFormula(m)
        exact_mass = ExactMolWt(m)
        xlogp = MolLogP(m)

        # Apply the isotope shift that was calculated from original SMILES
        if iso_shift != 0.0:
            logger.info(
                f"Applying isotope shift {iso_shift:.6f} Da to '{original_smiles[:50]}...'"
            )
            exact_mass += iso_shift

        # Create a copy for stereochemistry removal to avoid modifying original
        mol_no_stereo = _copy_molecule(m)
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
    batch: Iterable[Any],
    original_smiles_list: Iterable[str],
    writer: csv.writer,
    current_count: int,
    progress_interval: int,
    *,
    _logger: Optional[logging.Logger] = None,
) -> int:
    """
    Process a batch of molecules in parallel and write results.

    Args:
        executor: ThreadPoolExecutor for parallel processing
        batch: Iterable of RDKit Mol objects
        original_smiles_list: Corresponding original SMILES strings
        writer: CSV writer object
        current_count: Current count of processed molecules
        progress_interval: Logging interval

    Returns:
        Number of successfully processed molecules in this batch
    """
    log = _logger or logger
    batch_processed = 0
    # Use serial processing instead of parallel to avoid thread pool issues
    un = rdMolStandardize.Uncharger()

    for mol, smiles in zip(batch, original_smiles_list):
        result = process_molecule(mol, smiles, un)
        if result:
            writer.writerow(result)
            batch_processed += 1
            total = current_count + batch_processed
            if total % progress_interval == 0:
                log.info(f"Processed {total} molecules")

    return batch_processed


def process_smiles(
    input_smi_file: str,
    output_csv_file: str,
    num_workers: Optional[int] = DEFAULT_MAX_WORKERS,
    batch_size: int = DEFAULT_BATCH_SIZE,
    progress_interval: int = DEFAULT_PROGRESS_INTERVAL,
    *,
    _logger: Optional[logging.Logger] = None,
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
        _logger: Optional logger to use (defaults to module logger)

    Returns:
        Total number of successfully processed molecules

    Raises:
        FileValidationError: If input/output files are invalid
        SmilesProcessingError: If SMILES processing fails
    """
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
    except Exception as e:
        raise SmilesProcessingError(f"Failed to load SMILES file: {e}") from e

    molecules_processed = 0

    with open_output_file(output_path) as f:
        writer = csv.writer(f)
        writer.writerow(OUTPUT_COLUMNS)

        # Pre-allocate lists with capacity to reduce reallocation overhead
        batch: List[Any] = []
        originals: List[str] = []

        with ThreadPoolExecutor(max_workers=num_workers) as executor:
            for i, mol in enumerate(supplier):
                if mol:
                    batch.append(mol)
                    originals.append(supplier.GetItemText(i))

                # Process batch when full
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
                    # Clear lists efficiently
                    batch.clear()
                    originals.clear()

            # Process remaining molecules
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


# =====================================================================================
# Command-Line Interface (safe for reticulate)
# =====================================================================================


def main(argv: Optional[List[str]] = None) -> int:
    """CLI entry point.

    Returns an exit code for easier testing.
    """
    import argparse

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
        # Reconfigure logger at DEBUG level
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
    except Exception as e:
        get_logger().error(f"SMILES processing failed: {e}")
        return 1


# =====================================================================================
# Entry Point Guard (safe for reticulate)
# =====================================================================================

# When imported by R or another Python module, `__name__` != "__main__" — this prevents argparse from running.
# When executed directly from the command line, it behaves as a proper CLI.
if __name__ == "__main__" and len(sys.argv) > 1:
    sys.exit(main())
