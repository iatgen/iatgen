context("parcel")

test_that("parcelIAT", {
  clean <- clean_iat_fixture()

  parcel <- parcelIAT(clean)
  expect_equal(as.numeric(round(parcel[1, ], 4)), c(1.0826, 0.2992, 0.4367, 0.1931))
})

test_that("parcel scores are signed the same way as the full D score", {
  # parcelIAT() exists to supply indicators of the same latent variable as clean$D.
  # If the direction of subtraction were flipped, a latent factor built from these
  # parcels would correlate negatively with D and reverse every criterion correlation.
  clean <- clean_iat_fixture()

  parcel <- parcelIAT(clean)

  expect_equal(sign(rowMeans(parcel)), sign(clean$D))
})

test_that("parcelIAT returns four scores per participant", {
  clean <- clean_iat_fixture()

  parcel <- parcelIAT(clean)

  expect_equal(dim(parcel), c(2L, 4L))
  expect_named(parcel, c("D.1", "D.2", "D.3", "D.4"))
})

test_that("parcels are ordered by position in the block", {
  # Each parcel takes one quarter of the practice trials and the matching quarter of
  # the critical trials, in presentation order.
  clean <- clean_iat_fixture()

  parcel <- parcelIAT(clean)

  # rebuild the first parcel by hand: trials 1-5 of practice, 1-10 of critical
  prac.cols <- 1:5
  crit.cols <- 1:10
  compat <- cbind(
    clean$clean.latencies.prac1[, prac.cols],
    clean$clean.latencies.crit1[, crit.cols]
  )
  incompat <- cbind(
    clean$clean.latencies.prac2[, prac.cols],
    clean$clean.latencies.crit2[, crit.cols]
  )
  all.trials <- cbind(
    clean$clean.latencies.prac1, clean$clean.latencies.crit1,
    clean$clean.latencies.prac2, clean$clean.latencies.crit2
  )
  grand.sd <- apply(all.trials, 1, stats::sd, na.rm = TRUE)
  expected <- (rowMeans(incompat, na.rm = TRUE) - rowMeans(compat, na.rm = TRUE)) / grand.sd

  expect_equal(as.numeric(parcel$D.1), as.numeric(expected))
})

test_that("a block length not divisible by four is rejected, whichever block it is", {
  # Parcelling splits each block into quarters, so every trial count has to divide.
  # The check is written out once per block, so drive all four positions.
  ok <- make_iat_block(seq(450, 640, by = 10)) # 20 trials, divisible by four
  bad <- make_iat_block(seq(450, 630, by = 10)) # 19 trials, not divisible

  expected.message <- c(
    "practice block", "critical block",
    "practice block", "critical block"
  )

  for (position in 1:4) {
    args <- rep(list(ok), 4)
    args[[position]] <- bad
    clean <- do.call(cleanIAT, args)

    expect_error(parcelIAT(clean), expected.message[position],
      info = paste("undivisible block in position", position)
    )
  }
})
