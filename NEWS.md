# iatgen 1.9.0

## Trials are now timed with a monotonic clock

The survey JavaScript timed each trial by subtracting two readings of
`new Date().getTime()`, which reports the computer's wall clock. That clock is not
monotonic: it steps backwards when the machine corrects its time, so a trial spanning
such a correction recorded a negative reaction time.

Trials are now timed with `performance.now()`, which counts from page load and cannot
run backwards. Reaction times are rounded to whole milliseconds, since
`performance.now()` is fractional and a decimal point in the response string would be
read as a corrupted record.

`writeIATfull()` gains a `timing` argument. `timing="performance"` is the default;
`timing="date"` restores the previous wall-clock behaviour, for reproducing a survey
built with an earlier version or for a browser predating the Performance API.

This affects newly generated surveys only. Data already collected is unchanged, and
1.8.1 handles the negative latencies such data may contain.


# iatgen 1.8.1

## Negative reaction times no longer discard the participant

A reaction time below zero cannot occur physically. It appears when the clock on the
participant's computer steps backwards during a trial -- an NTP correction, a machine
waking from sleep, a daylight-saving adjustment -- because the survey JavaScript times
trials against the wall clock.

Previously the minus sign failed `cleanIAT()`'s data-integrity check, which treats
unexpected characters as evidence of a browser malfunction and discards *every* trial
that participant contributed. A single mistimed trial therefore cost all of their data;
in a one-participant file it cost the entire dataset.

Such a trial is now scored as missing on its own, exactly as an over-long trial is,
and the participant's remaining trials are analysed normally. The impossible value is
still visible in `clean$raw.latencies.*` for anyone auditing the data. New elements
report what happened: `num.negative.removed`, the same broken down per block, and
`negative.rate`. A warning is raised when any are found, since it points at a clock
problem on the respondent's machine.

The integrity check still rejects genuinely corrupt records; only the minus sign has
been added to the permitted characters.

Note that this addresses the symptom rather than the cause. Timing trials with
`performance.now()` instead of `new Date().getTime()` would prevent negative latencies
arising at all, but that is a change to the survey JavaScript and would only affect
newly generated surveys.


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
