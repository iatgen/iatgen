# iatgen 1.8.0

## Change to scores produced by `parcelIAT()`

`parcelIAT()` returned D-scores with the opposite sign to `cleanIAT()$D`. It
computed `block1 - block2` where `cleanIAT()` computes `block2 - block1`.

**Anyone who has used `parcelIAT()` before will find the scores from this version
reversed relative to earlier ones.** Because the parcels exist to serve as
indicators of the same construct as `clean$D`, a latent variable built from them
correlated negatively with the IAT score, reversing the sign of every correlation
with an external criterion. Analyses that used the parcels only as indicators of a
single latent factor are unaffected in fit, but any reported sign involving that
factor should be re-checked.

## Bug fixes

* `combineIATtwoblocks()` never issued its "contained no data" warning. The check
  for an entirely empty input ran after `NA` values had already been replaced with
  `""`, so it could not be true. A user of the two-permutation design who mistyped
  a variable name lost half of the data silently.
  `combineIATfourblocks()` was unaffected.

* `writeIATfull(qsf = TRUE)` deleted directories it had not created. It removed its
  four working folders with `unlink(recursive = TRUE)` without checking whether
  they already existed, so a same-named directory belonging to the user was
  destroyed along with its contents. Pre-existing folders are now left in place and
  a warning is issued.

* `writeIATfull(qsf = TRUE)` could read and then delete a user's file. It copied the
  QSF template into the working directory before parsing it, so a file already
  named `FullTemplate_-_For_Shiny_V11.qsf` was parsed as the template and then
  removed, failing with an unhelpful JSON parsing error. The template is now read
  directly from the installed package and never copied.

* `writeIATfull()` left the working directory changed if it failed partway through
  building a block. It is now restored via `on.exit()`.

## New features

* `writeIATfull()` gains an `outdir` argument controlling where the QSF file or the
  folders of HTML and JavaScript are written. It defaults to `getwd()`, so existing
  calls behave exactly as before.

## Documentation and messages

* `cleanIAT()`'s `inclusive.sd` argument was documented as "Unused parameter" but
  in fact switches the D-score denominator from the inclusive SD of Greenwald et
  al. (2003) to a pooled within-block SD, changing the scores. It is now documented
  accurately and raises a warning when disabled. The argument remains intended for
  testing and algorithm development only.

* `cleanIAT()` and `cleanIAT.noprac()` now report a block that is blank for every
  participant by name, instead of failing later with
  `argument is of length zero`. The usual cause is a mistyped variable name.

## Internal

* The test suite was rebuilt: 7 assertions to over 300, and statement coverage from
  84% to 100%. It now includes an implementation of the Greenwald et al. (2003)
  D-score written from the published description rather than from `cleanIAT()`, so
  that the two can be cross-checked; this is what identifies sign errors of the kind
  fixed above. Coverage is checked on every push and pull request.

* A synthetic 40-participant dataset covering all four counterbalancing
  permutations was added for testing. It contains no real participant data.


# iatgen 1.7.0

* Added tests and continuous integration.

# iatgen 1.6.0

* Added reliability testing; fixed `IATreliability()` ordering.

# iatgen 1.5.0

* Added explicit exports; removed SCIAT from production.
