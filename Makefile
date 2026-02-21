SHELL := /bin/bash

.PHONY: refactor render render-only example clean check build

# Run refactor on example dataset
example:
	cd example && R -q -e "source('../R/sampler_refactor.R'); run_sampler_refactor('sam2020.dat','bs_setup.dat','../results')"

# Run refactor with CONTROL and optional BS (defaults shown)
refactor:
	R -q -e "source('R/sampler_refactor.R'); run_sampler_refactor('${CONTROL:-example/sam2020.dat}','${BS:-example/bs_setup.dat}','${OUT:-results}','${OUT_PREFIX:-sampler}')"

# Render Quarto to index.html
render:
	@command -v quarto >/dev/null 2>&1 || { echo "quarto not found"; exit 1; }
	quarto render index.qmd

# Render only (no R run)
render-only:
	@command -v quarto >/dev/null 2>&1 || { echo "quarto not found"; exit 1; }
	quarto render index.qmd

# Remove generated outputs
clean:
	rm -f results/*.parquet

# Build package tarball
build:
	R CMD build pkg

# Basic package check
check:
	R CMD check pkg --no-manual
