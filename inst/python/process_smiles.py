import csv
import concurrent.futures
import gzip
import os
from rdkit.Chem import SmilesMolSupplier, MolToSmiles, MolToInchiKey, RemoveStereochemistry
from rdkit.Chem.Descriptors import ExactMolWt, MolLogP
from rdkit.Chem.rdMolDescriptors import CalcMolFormula
from rdkit import RDLogger

# Disable RDKit warnings
RDLogger.DisableLog("rdApp.warning")

def process_molecule(mol, original_smiles):
    """Processes a molecule and returns computed properties."""
    if mol is None:
        return None
    try:
        smiles = MolToSmiles(mol)
        RemoveStereochemistry(mol)
        return [
            original_smiles,
            smiles,
            MolToInchiKey(mol),
            CalcMolFormula(mol),
            ExactMolWt(mol),
            MolToSmiles(mol),
            MolLogP(mol)
        ]
    except Exception:
        return None

def open_output_file(output_csv_file):
    """Opens an output file, supporting both .csv and .gz formats."""
    if output_csv_file.endswith(".gz"):
        return gzip.open(output_csv_file, "wt", newline="")
    return open(output_csv_file, "w", newline="")

def process_smiles(input_smi_file, output_csv_file, num_workers=None, batch_size=1000, progress_interval=10000):
    """Processes a SMILES file using an optimized parallel approach."""
    
    if num_workers is None:
        num_workers = min(32, (os.cpu_count() or 1) * 2)

    supplier = SmilesMolSupplier(input_smi_file, nameColumn=-1)

    with open_output_file(output_csv_file) as f:
        writer = csv.writer(f)
        writer.writerow(["structure_smiles_initial", "structure_smiles", "structure_inchikey", "structure_molecular_formula",
                         "structure_exact_mass", "structure_smiles_no_stereo", "structure_xlogp"])

        count = 0
        batch = []
        original_smiles_list = []

        with concurrent.futures.ThreadPoolExecutor(max_workers=num_workers) as executor:
            for i, mol in enumerate(supplier):
                if mol:
                    batch.append(mol)
                    original_smiles_list.append(supplier.GetItemText(i))
                
                if len(batch) >= batch_size:
                    for result in executor.map(process_molecule, batch, original_smiles_list):
                        if result:
                            writer.writerow(result)
                            count += 1
                            if count % progress_interval == 0:
                                print(f"Processed {count} molecules...")
                    batch.clear()
                    original_smiles_list.clear()
            
            for result in executor.map(process_molecule, batch, original_smiles_list):
                if result:
                    writer.writerow(result)
                    count += 1

        print(f"Processing complete. Total molecules processed: {count}")
