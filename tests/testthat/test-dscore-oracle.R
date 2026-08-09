context("D-score algorithm")

# The tests elsewhere in this suite pin D scores to recorded numbers. Those catch
# accidental change, but they cannot tell you the recorded number was right in the
# first place - a wrong value simply gets frozen. These tests instead check cleanIAT()
# against an independent implementation of Greenwald et al. (2003) (see
# helper-iat.R:dscore_oracle) and against values that can be computed by hand.

test_that("cleanIAT reproduces an independent implementation of the D-score", {
  dat <- load_iat_fixture()

  clean <- suppressWarnings(cleanIAT(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit
  ))
  expected <- dscore_oracle(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit
  )

  expect_equal(as.numeric(clean$D), expected)
})

test_that("D is positive when the incompatible blocks are slower", {
  # This fixes the *meaning* of the sign rather than just its stability: a positive
  # D must indicate slower responding in the incompatible (second) pair of blocks.
  fast <- make_iat_block(rep(500, 20))
  slow <- make_iat_block(rep(700, 20))

  clean <- cleanIAT(fast, fast, slow, slow)
  expect_gt(clean$D, 0)

  # ...and reversing the roles must reverse the sign, symmetrically.
  reversed <- cleanIAT(slow, slow, fast, fast)
  expect_lt(reversed$D, 0)
  expect_equal(as.numeric(reversed$D), -as.numeric(clean$D))
})

test_that("D matches a hand-computed value on synthetic data", {
  # 20 compatible trials at 500 ms, 20 incompatible at 700 ms, in both prac and crit.
  # Block-mean difference is 200. The inclusive SD is the SD of all 40 trials.
  compat <- make_iat_block(rep(500, 20))
  incompat <- make_iat_block(rep(700, 20))

  inclusive.sd <- stats::sd(c(rep(500, 20), rep(700, 20)))
  expected <- 200 / inclusive.sd # identical for prac and crit, so their mean is the same

  clean <- cleanIAT(compat, compat, incompat, incompat)
  expect_equal(as.numeric(clean$D), expected)
})

test_that("D is invariant to adding a constant and scales inversely with spread", {
  # Two properties that follow from the definition and would break under most
  # algebraic slips in the scoring code.
  base.compat <- make_iat_block(seq(400, 780, by = 20))
  base.incompat <- make_iat_block(seq(600, 980, by = 20))

  d.base <- cleanIAT(base.compat, base.compat, base.incompat, base.incompat)$D

  # Shifting every latency by +100 ms changes both means and leaves the SD alone,
  # so D must be unchanged.
  shift.compat <- make_iat_block(seq(400, 780, by = 20) + 100)
  shift.incompat <- make_iat_block(seq(600, 980, by = 20) + 100)
  d.shift <- cleanIAT(shift.compat, shift.compat, shift.incompat, shift.incompat)$D

  expect_equal(as.numeric(d.shift), as.numeric(d.base))
})

test_that("cleanIAT.noprac matches the oracle restricted to critical blocks", {
  dat <- load_iat_fixture()

  clean <- suppressWarnings(cleanIAT.noprac(dat$compatible.crit, dat$incompatible.crit))

  # The no-practice variant is the same algorithm using only the critical blocks.
  expected <- vapply(seq_along(dat$compatible.crit), function(i) {
    a <- parse_iat_string(dat$compatible.crit[i])$latency
    b <- parse_iat_string(dat$incompatible.crit[i])$latency
    a <- a[a <= 10000]
    b <- b[b <= 10000]
    (mean(b) - mean(a)) / stats::sd(c(a, b))
  }, numeric(1))

  expect_equal(as.numeric(clean$D), expected)
})
