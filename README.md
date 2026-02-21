# sampler refactor

R refactor of `sam.tpl` that preserves input file formats and produces tidy Parquet outputs.

## Quick start

```bash
make refactor CONTROL=example/sam2020.dat BS=example/bs_setup.dat OUT=results
make render-only
```

## Outputs

- `results/sampler_tidy.parquet`
- `results/sampler_bootstrap_<year>.parquet`
- `results/sampler_refactor.log`

## GitHub Pages

Render `index.qmd` to `index.html` and publish via `/docs` or `gh-pages`.

## Packaging

```bash
make build
R CMD check samplerrefactor_0.1.0.tar.gz --no-manual
```
