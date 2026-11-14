# Portable GNPS C API (ctypes quickstart)

The `tima` native library exports three portable C symbols:

- `gnps_chain_dp_core_api`
- `gnps_aligned_core_api`
- `gnps_join_core_api`

Plus memory helper:

- `gnps_free_ptr`

## Build local shared object for demo

From the repository root, compile either via R:

```bash
R CMD SHLIB -o src/tima.so src/gnps.c src/init.c
```

Or standalone without R headers using any C compiler:

```bash
cc -shared -fPIC -O3 -DGNPS_HAS_R_API=0 src/gnps.c -lm -o src/gnps.so
```

## Run Python demo

```bash
python3 inst/python/gnps_portable_demo.py
```

## Notes

- `gnps_join_core_api` returns 0-based indices and uses `-1` for unmatched side.
- The two output arrays from `gnps_join_core_api` are heap-allocated in C; free
  both with `gnps_free_ptr`.
- In aligned scoring API (`gnps_aligned_core_api`), use `NaN` in `x_mz` or
  `y_mz` to represent missing rows.
