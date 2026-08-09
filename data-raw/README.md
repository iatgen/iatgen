# data-raw

Scripts that generate files used by the test suite. Not part of the built package
(excluded via `.Rbuildignore`).

## `make_demo_data.R`

Generates `tests/testthat/iat_demo_synthetic.csv`.

> **This is synthetic demo data. It does not come from human participants.**
> Every latency, error and response in that file was produced by the random number
> draws in this script. It must not be analysed, cited, or reported as though it were
> collected data.

### Why it exists

The recorded fixture `tests/testthat/iat_small.csv` contains two participants who both
saw the same counterbalancing permutation. That is too small and too narrow to test
reliability, internal consistency, or the logic that combines the four permutations.

### What it contains

40 synthetic participants, ten in each of the four permutations (RP, RN, LP, LN), in
the column layout and response encoding of a genuine iatgen Qualtrics export. Three
participants are deliberately awkward so the data-cleaning paths stay exercised: one
skipped the IAT, one responded too fast to be scored, and one has trials over the
timeout threshold.

Parameters were chosen to give psychometrics in the range reported for real IATs
rather than the implausibly clean numbers a naive noise model produces. As generated:
median reaction time around 850 ms, mean *D* about 0.42 (SD 0.35), roughly 8 per cent
of participants scoring below zero, and split-half reliability near 0.75.

### Replacing it with real data

This file is a placeholder. To swap in a real anonymised export, drop it in with the
same name and column headings; the tests should continue to pass. They are written to
assert only properties that any valid IAT dataset satisfies — that every participant is
either scored or accounted for, that *D* matches an independent implementation of the
Greenwald et al. (2003) algorithm, that reliability is bounded — and never specific
numeric values, precisely so that the swap does not break them.

Two things to keep if you do replace it: the `SYNTHETIC DEMO DATA` banner in row two
should be removed or replaced with a real provenance note, and the tests in
`tests/testthat/test-demo-data.R` that check for that banner and for `DEMO-` prefixed
IDs will need updating.

### Regenerating

```sh
Rscript data-raw/make_demo_data.R
```

Output is deterministic; the seed at the top of the script fixes every draw.
