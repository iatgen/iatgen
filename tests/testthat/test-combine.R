context("combining IAT permutations")

# Qualtrics spreads any one block of trials across the four counterbalanced
# permutations, so each participant has data in exactly one of the four columns and
# blanks in the rest. combineIATfourblocks() collapses them back into one variable.
#
# Every participant in iat_small.csv took the same permutation (LN), so the existing
# tests only ever exercised the case where the first three inputs are empty. These
# tests drive the selection logic across all four positions.

test_that("each participant's data is taken from whichever permutation they saw", {
  # a four-participant study in which each person saw a different permutation
  rp <- c("rp", "", "", "")
  lp <- c("", "lp", "", "")
  rn <- c("", "", "rn", "")
  ln <- c("", "", "", "ln")

  expect_equal(
    combineIATfourblocks(rp, lp, rn, ln),
    c("rp", "lp", "rn", "ln")
  )
})

test_that("earlier permutations take precedence when more than one is populated", {
  # Should not happen in real data, but the implementation defines a precedence and
  # it should stay stable: name1, then name2, then name3, then name4.
  name1 <- c("a", "", "", "")
  name2 <- c("b", "b", "", "")
  name3 <- c("c", "c", "c", "")
  name4 <- c("d", "d", "d", "d")

  expect_equal(
    combineIATfourblocks(name1, name2, name3, name4),
    c("a", "b", "c", "d")
  )
})

test_that("NA and whitespace-only cells count as absent", {
  # Qualtrics exports unanswered blocks as either an empty string, a single space or NA
  # depending on version and export settings.
  name1 <- c(NA, " ", "", NA)
  name2 <- c("second", "second", "second", NA)
  name3 <- c("third", "third", "third", NA)
  name4 <- c("fourth", "fourth", "fourth", "fourth")

  expect_equal(
    combineIATfourblocks(name1, name2, name3, name4),
    c("second", "second", "second", "fourth")
  )
})

test_that("a participant with no data anywhere yields an empty string", {
  # name2-name4 are wholly empty here, so the "contained no data" warning is expected.
  expect_equal(
    suppressWarnings(combineIATfourblocks(c("x", NA), c(NA, NA), c(NA, NA), c(NA, NA))),
    c("x", "")
  )
})

test_that("an entirely empty permutation warns the user", {
  # A wholly NA input usually means a mistyped variable name, which would otherwise
  # silently drop a quarter of the sample.
  expect_warning(
    combineIATfourblocks(c("a", "b"), c(NA, NA), c("c", "d"), c("e", "f")),
    "contained no data"
  )
})

test_that("factor columns are accepted as well as character columns", {
  # read.csv(stringsAsFactors = TRUE) was the default before R 4.0 and still appears
  # in older analysis scripts.
  name1 <- factor(c("rp", ""))
  name2 <- factor(c("", "lp"))

  expect_equal(
    suppressWarnings(combineIATfourblocks(name1, name2, c("", ""), c("", ""))),
    c("rp", "lp")
  )
})

test_that("combineIATtwoblocks collapses a two-permutation design", {
  # Retained for studies that fix one target or category to a single side.
  expect_equal(
    combineIATtwoblocks(c("first", "", NA), c("", "second", "third")),
    c("first", "second", "third")
  )
})

test_that("combineIATtwoblocks warns about an entirely empty permutation", {
  # Regression guard: the check used to sit below the NA-to-"" replacement, which made
  # it unreachable, so a mistyped variable name was collapsed away in silence.
  expect_warning(
    combineIATtwoblocks(c("a", "b"), c(NA, NA)),
    "contained no data"
  )
  expect_warning(
    combineIATtwoblocks(c(NA, NA), c("a", "b")),
    "contained no data"
  )
})

test_that("combineIATtwoblocks does not warn when both permutations carry data", {
  expect_silent(combineIATtwoblocks(c("a", ""), c("", "b")))
})

test_that("combined output feeds straight into cleanIAT", {
  # End-to-end: two participants on different permutations should both be scored.
  block <- make_iat_block(seq(450, 640, by = 10))
  slow <- make_iat_block(seq(550, 740, by = 10))

  prac1 <- combineIATfourblocks(c(block, ""), c("", block), c("", ""), c("", ""))
  crit1 <- combineIATfourblocks(c(block, ""), c("", block), c("", ""), c("", ""))
  prac2 <- combineIATfourblocks(c(slow, ""), c("", slow), c("", ""), c("", ""))
  crit2 <- combineIATfourblocks(c(slow, ""), c("", slow), c("", ""), c("", ""))

  clean <- cleanIAT(prac1, crit1, prac2, crit2)

  expect_false(any(clean$skipped))
  # same trials, different permutation column, so the two scores must agree
  expect_equal(as.numeric(clean$D[1]), as.numeric(clean$D[2]))
})
