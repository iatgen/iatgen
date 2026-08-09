context("full-study analysis on the demo dataset")

# These tests run the whole pipeline over iat_demo_synthetic.csv, a 40-participant
# stand-in that covers all four counterbalancing permutations. The recorded fixture
# (iat_small.csv) has two participants who both saw the same permutation, which is too
# small for reliability or internal consistency to mean anything.
#
# IMPORTANT: the demo file is machine-generated, not real participant data, and is
# meant to be swapped for a real anonymised export later. So nothing here asserts a
# specific number. Every expectation is a property that any valid IAT dataset must
# satisfy, so that replacing the file does not break the suite.


test_that("the demo fixture is labelled as synthetic", {
  # The warning lives in the row Qualtrics uses for column descriptions, which every
  # loader discards - so it travels with the file without affecting parsing.
  header.rows <- readLines(DEMO_FIXTURE, n = 2, encoding = "UTF-8")

  expect_match(header.rows[2], "SYNTHETIC DEMO DATA")
  expect_match(header.rows[2], "NOT REAL PARTICIPANTS")

  dat <- load_demo_fixture()
  expect_true(all(grepl("^DEMO-", dat$ID)))
})

test_that("all four counterbalancing permutations carry data", {
  dat <- load_demo_fixture()

  block.columns <- grep("^Q[0-9]+\\.[RL][NP][0-9]$", names(dat), value = TRUE)
  populated <- block.columns[colSums(!is.na(dat[block.columns]) & dat[block.columns] != "") > 0]
  permutations <- unique(sub("^Q[0-9]+\\.([RL][NP])[0-9]$", "\\1", populated))

  expect_setequal(permutations, c("RP", "RN", "LP", "LN"))
})

test_that("every participant is scored or accounted for", {
  dat <- load_demo_fixture()
  clean <- clean_demo_fixture()

  expect_length(clean$D, nrow(dat))

  # A participant may legitimately end up without a score, but only for a stated
  # reason: they skipped the IAT, or they were dropped for responding too fast.
  unscored <- is.na(clean$D)
  accounted <- clean$skipped | (!is.na(clean$drop.participant) & clean$drop.participant)
  expect_true(all(unscored == accounted))
})

test_that("the cleaning rules all fire on this dataset", {
  # The fixture deliberately contains a participant who skipped, a button-masher, and
  # a participant with trials over the timeout, so these paths stay exercised.
  clean <- clean_demo_fixture()

  expect_gt(sum(clean$skipped), 0)
  expect_gt(clean$fastprt.count, 0)
  expect_gt(clean$num.timeout.removed, 0)
  expect_gt(clean$error.rate, 0)
  expect_lt(clean$error.rate, 0.5)
})

test_that("D scores match the independent implementation of the algorithm", {
  dat <- load_demo_fixture()
  clean <- clean_demo_fixture()

  expected <- dscore_oracle(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit
  )

  # The oracle has no notion of dropping fast participants, so compare only those the
  # cleaning rules retained.
  keep <- !clean$skipped & !clean$drop.participant
  expect_equal(as.numeric(clean$D[keep]), expected[keep])
})

test_that("D scores are in a plausible range and vary between people", {
  clean <- clean_demo_fixture()
  d <- clean$D[!is.na(clean$D)]

  # D is a standardised mean difference; values beyond +/-2 indicate something wrong.
  expect_true(all(abs(d) < 2))
  expect_gt(stats::sd(d), 0.1) # genuine individual differences, not a constant
})

test_that("reliability is estimable and bounded on a full-size sample", {
  clean <- clean_demo_fixture()

  rel <- IATreliability(clean)

  expect_false(is.na(rel$splithalfcorr))
  expect_gt(rel$splithalfcorr, 0)
  expect_lte(rel$reliability, 1)
  # Spearman-Brown applied to the split-half correlation
  expect_equal(rel$reliability, (2 * rel$splithalfcorr) / (1 + rel$splithalfcorr))
})

test_that("Cronbach's alpha is estimable on a full-size sample", {
  clean <- clean_demo_fixture()

  alpha <- quiet_IATalpha(clean)

  expect_true(is.finite(as.numeric(alpha$alpha.total)))
  expect_lte(as.numeric(alpha$alpha.total), 1)
  expect_equal(nrow(alpha$diffscores), length(clean$D))
})

test_that("parcel scores track the full D score", {
  clean <- clean_demo_fixture()

  parcel <- parcelIAT(clean)

  expect_equal(nrow(parcel), length(clean$D))

  # Parcels divide by the grand SD across all blocks, while D averages two separately
  # standardised halves, so the two are strongly correlated rather than identical. The
  # sign can differ for a participant whose D is essentially zero, which is why this
  # checks the correlation instead of asserting agreement participant by participant.
  parcel.mean <- rowMeans(parcel)
  expect_gt(stats::cor(parcel.mean, clean$D, use = "pairwise.complete.obs"), 0.9)
})

test_that("scoring is unaffected by which permutation a participant saw", {
  # Permutation assignment is a counterbalancing device and should not shift scores.
  # If the block-to-variable mapping were wrong for one permutation, its D scores would
  # be systematically reversed relative to the others.
  dat <- load_demo_fixture()
  clean <- clean_demo_fixture()

  block.columns <- grep("^Q[0-9]+\\.[RL][NP][0-9]$", names(dat), value = TRUE)
  permutation <- rep(NA_character_, nrow(dat))
  for (column in block.columns) {
    has.data <- !is.na(dat[[column]]) & dat[[column]] != ""
    permutation[has.data] <- sub("^Q[0-9]+\\.([RL][NP])[0-9]$", "\\1", column)
  }

  scored <- !is.na(clean$D) & !is.na(permutation)
  group.means <- tapply(clean$D[scored], permutation[scored], mean)

  expect_length(group.means, 4)
  # every permutation points the same way as the sample overall
  expect_true(all(sign(group.means) == sign(mean(clean$D, na.rm = TRUE))))
})
