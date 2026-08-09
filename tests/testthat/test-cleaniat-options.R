context("cleanIAT data-cleaning options")

# None of cleanIAT()'s data-cleaning options were exercised anywhere in the suite:
# every test ran on defaults. These tests drive each option with synthetic data whose
# correct result can be worked out by hand.


## --- timeout dropping -------------------------------------------------------

test_that("timeout.drop removes trials over the threshold and reports the count", {
  # 19 trials at 500 ms plus one at 12000 ms, in the first practice block only.
  slowtrial <- make_iat_block(c(rep(500, 19), 12000))
  normal <- make_iat_block(rep(500, 20))

  clean <- cleanIAT(slowtrial, normal, normal, normal)

  expect_true(clean$timeout.drop)
  expect_equal(clean$num.timeout.removed, 1)
  expect_equal(clean$num.timeout.removed.prac1, 1)
  expect_equal(clean$num.timeout.removed.crit1, 0)
  # the surviving 19 trials all sit at 500 ms
  expect_equal(as.numeric(clean$clean.means.prac1), 500)
  expect_true(is.na(clean$clean.latencies.prac1[1, 20]))
})

test_that("timeout.drop = FALSE keeps long trials", {
  slowtrial <- make_iat_block(c(rep(500, 19), 12000))
  normal <- make_iat_block(rep(500, 20))

  clean <- cleanIAT(slowtrial, normal, normal, normal, timeout.drop = FALSE)

  expect_equal(clean$num.timeout.removed, 0)
  expect_equal(as.numeric(clean$clean.means.prac1), (19 * 500 + 12000) / 20)
})

test_that("timeout.ms is honoured when set to a custom threshold", {
  block <- make_iat_block(c(rep(500, 19), 3000))
  normal <- make_iat_block(rep(500, 20))

  # 3000 ms survives the default 10000 ms cut-off but not a 2000 ms one.
  expect_equal(cleanIAT(block, normal, normal, normal)$num.timeout.removed, 0)
  expect_equal(
    cleanIAT(block, normal, normal, normal, timeout.ms = 2000)$num.timeout.removed,
    1
  )
})


## --- fast-trial dropping ----------------------------------------------------

test_that("fasttrial.drop removes short trials only when enabled", {
  fasttrial <- make_iat_block(c(rep(500, 19), 200))
  normal <- make_iat_block(rep(500, 20))

  # disabled by default
  off <- cleanIAT(fasttrial, normal, normal, normal)
  expect_false(off$fasttrial.drop)
  expect_equal(off$num.fasttrial.removed, 0)

  on <- cleanIAT(fasttrial, normal, normal, normal, fasttrial.drop = TRUE, fasttrial.ms = 400)
  expect_true(on$fasttrial.drop)
  expect_equal(on$num.fasttrial.removed, 1)
  expect_equal(on$num.fasttrial.removed.prac1, 1)
  expect_equal(as.numeric(on$clean.means.prac1), 500)
})


## --- fast-participant dropping ----------------------------------------------

# These use blocks with genuine within-block spread rather than a constant latency.
# With a constant latency the inclusive SD is zero and D is NaN regardless of dropping,
# which would make "D is NA" pass for the wrong reason.
varied_block <- function(offset = 0) make_iat_block(seq(450, 640, by = 10) + offset)

test_that("fastprt.drop removes participants with too many fast responses", {
  # 80 trials in total; 10 of them (12.5%) are under 300 ms, over the .10 threshold.
  fastblock <- make_iat_block(c(rep(250, 10), rep(500, 10)))

  clean <- cleanIAT(fastblock, varied_block(), varied_block(50), varied_block(50))

  expect_true(clean$drop.participant)
  expect_equal(clean$fastprt.count, 1)
  expect_equal(clean$fastprt.rate, 1)
  expect_true(is.na(clean$D))
  # every block is blanked out for a dropped participant, not just the score
  expect_true(all(is.na(clean$clean.latencies.crit1[1, ])))
})

test_that("a participant just under the fastprt threshold is retained", {
  # 8 of 80 trials (10.0%) are fast; the test is strictly greater-than, so this stays.
  borderline <- make_iat_block(c(rep(250, 8), rep(500, 12)))

  clean <- cleanIAT(borderline, varied_block(), varied_block(50), varied_block(50))

  expect_false(clean$drop.participant)
  expect_equal(clean$fastprt.count, 0)
  expect_false(is.na(clean$D))
})

test_that("fastprt.drop = FALSE retains fast participants", {
  fastblock <- make_iat_block(c(rep(250, 10), rep(500, 10)))

  clean <- cleanIAT(fastblock, varied_block(), varied_block(50), varied_block(50),
    fastprt.drop = FALSE
  )

  expect_false(clean$drop.participant)
  expect_equal(clean$fastprt.count, 0)
  expect_false(is.na(clean$D))
})

test_that("a participant with no variance in latencies scores an undefined D", {
  # Every trial identical, so the inclusive SD is zero and D is 0/0. Pinned here
  # because it is easy to mistake this NaN for a data-cleaning drop.
  flat <- make_iat_block(rep(500, 20))

  clean <- cleanIAT(flat, flat, flat, flat)

  expect_false(clean$drop.participant)
  expect_false(clean$skipped)
  expect_equal(as.numeric(clean$inclusive.sd.crit), 0)
  expect_true(is.na(clean$D))
})


## --- error penalties --------------------------------------------------------

test_that("error.penalty = FALSE leaves incorrect latencies untouched", {
  # one error at 900 ms among nine correct trials at 500 ms
  witherror <- make_iat_block(c(rep(500, 9), 900), correct = c(rep(TRUE, 9), FALSE))
  normal <- make_iat_block(rep(500, 10))

  clean <- cleanIAT(witherror, normal, normal, normal)

  expect_false(clean$error.penalty)
  expect_equal(as.numeric(clean$clean.means.prac1), (9 * 500 + 900) / 10)
})

test_that("the D600 penalty replaces errors with the correct-trial mean plus 600", {
  witherror <- make_iat_block(c(rep(500, 9), 900), correct = c(rep(TRUE, 9), FALSE))
  normal <- make_iat_block(rep(500, 10))

  clean <- cleanIAT(witherror, normal, normal, normal,
    error.penalty = TRUE, error.penalty.ms = 600
  )

  expect_true(clean$error.penalty)
  # correct-trial mean is 500, so the error becomes 1100
  expect_equal(as.numeric(clean$clean.latencies.prac1[1, 10]), 1100)
  expect_equal(as.numeric(clean$clean.means.prac1), (9 * 500 + 1100) / 10)
})

test_that("the 2SD penalty replaces errors with the correct-trial mean plus 2 SD", {
  correct.latencies <- c(400, 450, 500, 550, 600)
  witherror <- make_iat_block(
    c(correct.latencies, 900),
    correct = c(rep(TRUE, 5), FALSE)
  )
  normal <- make_iat_block(rep(500, 6))

  clean <- cleanIAT(witherror, normal, normal, normal,
    error.penalty = TRUE, error.penalty.ms = "2SD"
  )

  expected <- mean(correct.latencies) + 2 * stats::sd(correct.latencies)
  expect_equal(as.numeric(clean$clean.latencies.prac1[1, 6]), expected)
})

test_that("error rates are reported per block and overall", {
  # 1 error in 10 trials in prac1, none elsewhere: 1 of 40 trials overall.
  witherror <- make_iat_block(c(rep(500, 9), 900), correct = c(rep(TRUE, 9), FALSE))
  normal <- make_iat_block(rep(500, 10))

  clean <- cleanIAT(witherror, normal, normal, normal)

  expect_equal(clean$error.rate.prac1, 0.1)
  expect_equal(clean$error.rate.crit1, 0)
  expect_equal(as.numeric(clean$error.num.prt), 1)
  expect_equal(clean$error.rate, 1 / 40)
})


## --- scoring denominator ----------------------------------------------------

test_that("inclusive.sd = FALSE warns and changes the D score", {
  dat <- load_iat_fixture()

  default <- suppressWarnings(cleanIAT(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit
  ))

  expect_warning(
    pooled <- cleanIAT(
      dat$compatible.prac, dat$compatible.crit,
      dat$incompatible.prac, dat$incompatible.crit,
      inclusive.sd = FALSE
    ),
    "pooled within-block SD"
  )

  # The option is documented as non-standard; confirm it really does change scoring,
  # so the warning is not merely decorative.
  expect_false(isTRUE(all.equal(as.numeric(default$D), as.numeric(pooled$D))))
})

test_that("the default scoring path does not warn about inclusive.sd", {
  normal <- make_iat_block(rep(500, 20))
  slow <- make_iat_block(rep(700, 20))

  expect_silent(cleanIAT(normal, normal, slow, slow))
})
