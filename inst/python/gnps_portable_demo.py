"""Small ctypes demo for the portable GNPS C API."""

from __future__ import annotations

import ctypes
from pathlib import Path


class GnpsCoreResult(ctypes.Structure):
    _fields_ = [
        ("score", ctypes.c_double),
        ("matches", ctypes.c_int),
        ("score_forward", ctypes.c_double),
        ("score_reverse", ctypes.c_double),
    ]


class GnpsJoinResult(ctypes.Structure):
    _fields_ = [
        ("x_idx", ctypes.c_void_p),
        ("y_idx", ctypes.c_void_p),
        ("n_rows", ctypes.c_int),
    ]


def load_lib(path: Path) -> ctypes.CDLL:
    lib = ctypes.CDLL(str(path))

    lib.gnps_chain_dp_core_api.argtypes = [
        ctypes.c_void_p,
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_void_p,
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_double,
        ctypes.c_double,
        ctypes.c_double,
        ctypes.c_double,
        ctypes.c_void_p,
    ]
    lib.gnps_chain_dp_core_api.restype = ctypes.c_int

    lib.gnps_aligned_core_api.argtypes = [
        ctypes.c_void_p,
        ctypes.c_void_p,
        ctypes.c_void_p,
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_void_p,
    ]
    lib.gnps_aligned_core_api.restype = ctypes.c_int

    lib.gnps_join_core_api.argtypes = [
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_void_p,
        ctypes.c_int,
        ctypes.c_double,
        ctypes.c_double,
        ctypes.c_double,
        ctypes.c_double,
        ctypes.c_void_p,
    ]
    lib.gnps_join_core_api.restype = ctypes.c_int

    lib.gnps_free_ptr.argtypes = [ctypes.c_void_p]
    lib.gnps_free_ptr.restype = None

    return lib


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    lib_path = root / "src" / "gnps.so"
    lib = load_lib(lib_path)

    x_mz = (ctypes.c_double * 2)(100.0, 200.0)
    x_int = (ctypes.c_double * 2)(1.0, 4.0)
    y_mz = (ctypes.c_double * 2)(100.005, 200.004)
    y_int = (ctypes.c_double * 2)(1.0, 9.0)

    res = GnpsCoreResult()
    rc = lib.gnps_chain_dp_core_api(
        ctypes.cast(x_mz, ctypes.c_void_p),
        ctypes.cast(x_int, ctypes.c_void_p),
        2,
        ctypes.cast(y_mz, ctypes.c_void_p),
        ctypes.cast(y_int, ctypes.c_void_p),
        2,
        300.0,
        300.01,
        0.01,
        10.0,
        ctypes.cast(ctypes.byref(res), ctypes.c_void_p),
    )
    if rc != 0:
        raise RuntimeError(f"gnps_chain_dp_core_api failed with rc={rc}")
    print("chain_dp:", res.score, res.matches, res.score_forward, res.score_reverse)

    aligned = GnpsCoreResult()
    rc = lib.gnps_aligned_core_api(
        ctypes.cast(x_mz, ctypes.c_void_p),
        ctypes.cast(x_int, ctypes.c_void_p),
        ctypes.cast(y_mz, ctypes.c_void_p),
        ctypes.cast(y_int, ctypes.c_void_p),
        2,
        ctypes.cast(ctypes.byref(aligned), ctypes.c_void_p),
    )
    if rc != 0:
        raise RuntimeError(f"gnps_aligned_core_api failed with rc={rc}")
    print("aligned:", aligned.score, aligned.matches)

    joined = GnpsJoinResult()
    rc = lib.gnps_join_core_api(
        ctypes.cast(x_mz, ctypes.c_void_p),
        2,
        ctypes.cast(y_mz, ctypes.c_void_p),
        2,
        300.0,
        300.01,
        0.01,
        10.0,
        ctypes.cast(ctypes.byref(joined), ctypes.c_void_p),
    )
    if rc != 0:
        raise RuntimeError(f"gnps_join_core_api failed with rc={rc}")

    try:
        x_vals = (ctypes.c_int * joined.n_rows).from_address(joined.x_idx)
        y_vals = (ctypes.c_int * joined.n_rows).from_address(joined.y_idx)
        pairs = [(x_vals[i], y_vals[i]) for i in range(joined.n_rows)]
        print("join rows:", pairs)
    finally:
        lib.gnps_free_ptr(joined.x_idx)
        lib.gnps_free_ptr(joined.y_idx)


if __name__ == "__main__":
    main()
