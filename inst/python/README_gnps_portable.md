# Portable GNPS C API (ctypes quickstart)

This package now exposes three C symbols in `tima` native library:

- `gnps_chain_dp_core_api`
- `gnps_aligned_core_api`
- `gnps_join_core_api`

Plus memory helper:

- `gnps_free_ptr`

## Build local shared object for demo

```bash
cd /Users/adrutz/Git/tima
R CMD SHLIB src/gnps.c
```

## Run Python demo

```bash
cd /Users/adrutz/Git/tima
python3 inst/python/gnps_portable_demo.py
```

## Notes

- `gnps_join_core_api` returns 0-based indices and uses `-1` for unmatched side.
- The two output arrays from `gnps_join_core_api` are heap-allocated in C; free both with `gnps_free_ptr`.
- In aligned scoring API (`gnps_aligned_core_api`), use `NaN` in `x_mz` or `y_mz` to represent missing rows.

