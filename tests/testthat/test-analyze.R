context("analyze csv output")

test_that("Function to import and analyze csv", {
  dat <- load_iat_fixture()

  clean <- cleanIAT(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit
  )

  expect_equal(as.numeric(round(clean$D, 6)), c(0.536744, -0.520029))
})

test_that("cleaning the sample data reports no exclusions", {
  # The recorded D scores above are only meaningful if neither participant was dropped
  # by the default cleaning rules; assert that explicitly rather than assuming it.
  clean <- clean_iat_fixture()

  expect_equal(clean$skipped, c(FALSE, FALSE))
  expect_equal(as.logical(clean$drop.participant), c(FALSE, FALSE))
  expect_equal(clean$num.timeout.removed, 0)
  expect_equal(clean$num.fasttrial.removed, 0)
})

test_that("cleanIAT returns one score and one row of trials per participant", {
  clean <- clean_iat_fixture()

  expect_length(clean$D, 2)
  expect_equal(nrow(clean$clean.latencies.prac1), 2)
  expect_equal(nrow(clean$clean.latencies.crit1), 2)
  expect_equal(nrow(clean$clean.latencies.prac2), 2)
  expect_equal(nrow(clean$clean.latencies.crit2), 2)
  # 20 practice and 40 critical trials per block in this study
  expect_equal(ncol(clean$clean.latencies.prac1), 20)
  expect_equal(ncol(clean$clean.latencies.crit1), 40)
})

test_that("D is the mean of the practice and critical D scores", {
  clean <- clean_iat_fixture()

  expect_equal(
    as.numeric(clean$D),
    as.numeric((clean$D.prac + clean$D.crit) / 2)
  )
})
