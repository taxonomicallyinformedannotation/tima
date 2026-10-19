# AGENTS.md --- Tima Development Guide

TIMA (Taxonomically Informed Metabolite Annotation) is an R package with C
extensions, a targets-based pipeline, a Shiny app, and CLI scripts. Uses
Bioconductor.

## Quick Commands

| Task | Command |
|------|---------|
| Run all tests | `Rscript -e 'library(testthat); testthat::test_package("tima")'` |
| Run single test file | `Rscript -e 'library(pkgload); pkgload::load_all("."); library(testthat); test_file("tests/testthat/test-FILENAME.R")'` |
| Format R code | `air format .` |
| Check formatting | `air format . --check` |
| Lint R code | `Rscript -e 'library(lintr); lintr::lint_dir("R")'` |
| Roxygen docs | `Rscript -e 'roxygen2::roxygenise(".")'` |
| R CMD check | `R CMD check . --no-manual` |
| Regenerate code map | `graphify . . --no-commit` (requires graphify) |

## Setup: MCP & Knowledge Graph

Before using AI agents (Claude Code, opencode, etc.), set up the knowledge graph:

### 1. Install graphify and code-review-graph

```bash
pip install graphifyy code-review-graph
```

### 2. Build the code-review-graph (instant)

```bash
code-review-graph build
```

### 3. Create `.mcp.json` for MCP server configuration

```json
{
  "mcpServers": {
    "code-review-graph": {
      "command": "code-review-graph",
      "args": ["serve"],
      "type": "stdio"
    },
    "graphify": {
      "command": "python",
      "args": ["-m", "graphify.serve", "graphify-out/graph.json"],
      "type": "stdio"
    }
  }
}
```

### 4. Build the knowledge graph

In Claude Code: `/graphify .`

Or manually:
```bash
graphify . .
```

### 5. Install git hooks for auto-updates (optional)

```bash
graphify hook install
```

Then add to `.git/hooks/post-commit`:
```bash
if command -v code-review-graph >/dev/null 2>&1; then
    code-review-graph update 2>/dev/null || true
fi
```

## Project Structure

- `R/` --- R package source (144 files, ~54k LOC). Key entry points:
  `R/run_tima.R`, `R/run_app.R`
- `src/` --- C source. `gnps.c` implements spectral similarity; `init.c`
  registers symbols
- `tests/testthat/` --- 157 test files (testthat, 3rd edition). `setup.R` loads
  via pkgload
- `inst/scripts/` --- CLI scripts (docopt-based). Usage in
  `inst/scripts/docopt/*.txt`
- `inst/targets/` --- targets pipeline sections (sourced by `inst/_targets.R`)
- `inst/app/` --- Shiny app (`app_server.R`, `app_ui.R`)
- `inst/params/` --- YAML parameter templates
- `inst/contracts/` --- mzTab-m schema definitions
- `inst/python/` --- RDKit/ctypes Python integration for SMILES and GNPS
- `man/` --- roxygen-generated `.Rd` docs (507 files)
- `vignettes/` --- Quarto vignettes (I-gathering → IV-benchmarking)
- `graphify-out/` --- Knowledge graph (gitignored, regenerated as needed)

## Gotcha

- Tests MUST use `pkgload::load_all(".")` to access internal (non-exported)
  functions. `testthat::test_package("tima")` against an installed namespace
  will fail with "could not find function" errors for internal helpers.
- Bioconductor deps require `BiocManager::install()` --- not
  `install.packages()`
- C code is compiled via `src/init.c`; modify `.Call()` wrappers only in
  `R/c_wrappers.R`
- `inst/scripts/docopt/*.txt` define CLI usage --- update when adding CLI
  options
- Do NOT run `R CMD check` without Bioconductor deps installed (~10+ min
  otherwise)

## Definition of Done

1. `air format . --check` passes (exit 0)
2. Tests pass in `tests/testthat/` (requires `pkgload::load_all()` for local
   runs)
3. `Rscript -e 'roxygen2::roxygenise(".")'` passes with no errors
4. If new code affects R → run `R CMD check . --no-manual` locally before
   pushing

## Code Style

- Follow tidyverse style guide (<https://style.tidyverse.org>)
- R formatting via Air: `air format .`
- roxygen2 with Markdown syntax for documentation
- 80-char line limit (enforced by air)