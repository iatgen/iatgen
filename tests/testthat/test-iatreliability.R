context("reliability")

# Builds a deterministic multi-participant IAT. Each participant responds a little
# faster or slower overall, and shows a different compatible/incompatible effect, so
# that D scores genuinely vary across people and a correlation is meaningful.
synthetic_sample <- function(n.participants = 12, n.trials = 20) {
  base <- seq(450, 450 + (n.trials - 1) * 10, by = 10)
  compat <- character(n.participants)
  incompat <- character(n.participants)
  for (i in seq_len(n.participants)) {
    speed <- (i - 1) * 15 # person-level differences in overall speed
    effect <- 40 + (i - 1) * 20 # person-level differences in IAT effect
    # alternate the sign of a small trial-level wobble so odd and even trials are not
    # identical, which would force the split-half correlation to exactly 1
    wobble <- rep(c(-12, 12), length.out = n.trials)
    compat[i] <- make_iat_block(base + speed + wobble)
    incompat[i] <- make_iat_block(base + speed + effect - wobble)
  }
  list(compat = compat, incompat = incompat)
}

test_that("IATreliability", {
  clean <- clean_iat_fixture()

  reliability <- IATreliability(clean)
  expect_equal(round(reliability$reliability, 4), 1)
})

test_that("reliability follows the Spearman-Brown correction of the split-half r", {
  # The n = 2 assertion above cannot fail: a correlation between two points is always
  # +/-1. This checks the relationship the function actually implements, on a sample
  # large enough for the correlation to carry information.
  s <- synthetic_sample()
  clean <- cleanIAT(s$compat, s$compat, s$incompat, s$incompat)

  rel <- IATreliability(clean)

  expect_equal(
    rel$reliability,
    (2 * rel$splithalfcorr) / (1 + rel$splithalfcorr)
  )
  expect_gt(rel$splithalfcorr, 0)
  expect_lte(rel$reliability, 1)
})

test_that("reliability returns one odd and one even D score per participant", {
  s <- synthetic_sample()
  clean <- cleanIAT(s$compat, s$compat, s$incompat, s$incompat)

  rel <- IATreliability(clean)

  expect_length(rel$D.odd, length(s$compat))
  expect_length(rel$D.even, length(s$compat))
  # the half-scores are averages of their practice and critical components
  expect_equal(as.numeric(rel$D.odd), as.numeric((rel$D.prac.odd + rel$D.crit.odd) / 2))
  expect_equal(as.numeric(rel$D.even), as.numeric((rel$D.prac.even + rel$D.crit.even) / 2))
})

test_that("both half-scores point the same way as the full D score", {
  # If either half were computed with the blocks the wrong way round, its scores would
  # oppose the full D score rather than track it.
  s <- synthetic_sample()
  clean <- cleanIAT(s$compat, s$compat, s$incompat, s$incompat)

  rel <- IATreliability(clean)

  expect_equal(sign(as.numeric(rel$D.odd)), sign(as.numeric(clean$D)))
  expect_equal(sign(as.numeric(rel$D.even)), sign(as.numeric(clean$D)))
})
