#!/usr/bin/env python3
"""Small ctypes demo for the portable GNPS C API."""

from __future__ import annotations

import ctypes
from ctypes import POINTER, byref, c_double, c_int, c_void_p
from pathlib import Path


class GnpsCoreResult(ctypes.Structure):
    _fields_ = [
        ("score", c_double),
        ("matches", c_int),
        ("score_forward", c_double),
        ("score_reverse", c_double),
    ]


class GnpsJoinResult(ctypes.Structure):
    _fields_ = [
        ("x_idx", POINTER(c_int)),
        ("y_idx", POINTER(c_int)),
        ("n_rows", c_int),
    ]


def load_lib(path: Path) -> ctypes.CDLL:
    lib = ctypes.CDLL(str(path))

    lib.gnps_chain_dp_core_api.argtypes = [
        POINTER(c_double), POINTER(c_double), c_int,
        POINTER(c_double), POINTER(c_double), c_int,
        c_double, c_double,
        c_double, c_double,
        POINTER(GnpsCoreResult),
    ]
    lib.gnps_chain_dp_core_api.restype = c_int

    lib.gnps_aligned_core_api.argtypes = [
        POINTER(c_double), POINTER(c_double),
        POINTER(c_double), POINTER(c_double),
        c_int,
        POINTER(GnpsCoreResult),
    ]
    lib.gnps_aligned_core_api.restype = c_int

    lib.gnps_join_core_api.argtypes = [
        POINTER(c_double), c_int,
        POINTER(c_double), c_int,
        c_double, c_double,
        c_double, c_double,
        POINTER(GnpsJoinResult),
    ]
    lib.gnps_join_core_api.restype = c_int

    lib.gnps_free_ptr.argtypes = [c_void_p]
    lib.gnps_free_ptr.restype = None

    return lib


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    lib_path = root / "src" / "gnps.so"
    lib = load_lib(lib_path)

    x_mz = (c_double * 2)(100.0, 200.0)
    x_int = (c_double * 2)(1.0, 4.0)
    y_mz = (c_double * 2)(100.005, 200.004)
    y_int = (c_double * 2)(1.0, 9.0)

    res = GnpsCoreResult()
    rc = lib.gnps_chain_dp_core_api(
        x_mz, x_int, 2,
        y_mz, y_int, 2,
        300.0, 300.01,
        0.01, 10.0,
        byref(res),
    )
    if rc != 0:
        raise RuntimeError(f"gnps_chain_dp_core_api failed with rc={rc}")
    print("chain_dp:", res.score, res.matches, res.score_forward, res.score_reverse)

    aligned = GnpsCoreResult()
    rc = lib.gnps_aligned_core_api(x_mz, x_int, y_mz, y_int, 2, byref(aligned))
    if rc != 0:
        raise RuntimeError(f"gnps_aligned_core_api failed with rc={rc}")
    print("aligned:", aligned.score, aligned.matches)

    joined = GnpsJoinResult()
    rc = lib.gnps_join_core_api(
        x_mz, 2,
        y_mz, 2,
        300.0, 300.01,
        0.01, 10.0,
        byref(joined),
    )
    if rc != 0:
        raise RuntimeError(f"gnps_join_core_api failed with rc={rc}")

    try:
        pairs = [(joined.x_idx[i], joined.y_idx[i]) for i in range(joined.n_rows)]
        print("join rows:", pairs)
    finally:
        lib.gnps_free_ptr(joined.x_idx)
        lib.gnps_free_ptr(joined.y_idx)


if __name__ == "__main__":
    main()


