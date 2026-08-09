context("analyze csv output without practice blocks")

test_that("Function to import and analyze csv", {
  dat <- load_iat_fixture()

  clean <- cleanIAT.noprac(dat$compatible.crit, dat$incompatible.crit)

  expect_equal(as.numeric(round(clean$D, 6)), c(0.210920, -0.249192))
})

test_that("the no-practice score equals the critical component of the full score", {
  # cleanIAT.noprac() is the same algorithm run on the critical blocks alone, so it
  # should reproduce clean$D.crit rather than clean$D.
  dat <- load_iat_fixture()

  full <- clean_iat_fixture()
  noprac <- cleanIAT.noprac(dat$compatible.crit, dat$incompatible.crit)

  expect_equal(as.numeric(noprac$D), as.numeric(full$D.crit))
})

test_that("cleanIAT.noprac applies the same cleaning options", {
  slowtrial <- make_iat_block(c(rep(500, 19), 12000))
  normal <- make_iat_block(seq(450, 640, by = 10))

  clean <- cleanIAT.noprac(slowtrial, normal)
  expect_equal(clean$num.timeout.removed, 1)

  kept <- cleanIAT.noprac(slowtrial, normal, timeout.drop = FALSE)
  expect_equal(kept$num.timeout.removed, 0)
})

test_that("cleanIAT.noprac rejects missing and empty inputs", {
  block <- make_iat_block(rep(500, 10))

  expect_error(cleanIAT.noprac(NULL, block), "does not exist")
  expect_error(cleanIAT.noprac(block, NULL), "does not exist")
  expect_error(cleanIAT.noprac(NA, block), "empty")
  expect_error(cleanIAT.noprac(block, NA), "empty")
})

test_that("cleanIAT.noprac reports a block that is blank for every participant", {
  block <- make_iat_block(rep(500, 10))

  expect_error(
    cleanIAT.noprac(c("", ""), c(block, block)),
    "No usable IAT data in: crit1"
  )
  expect_error(
    cleanIAT.noprac(c(block, block), c("", "")),
    "No usable IAT data in: crit2"
  )
})

test_that("D is positive when the incompatible block is slower", {
  fast <- make_iat_block(seq(450, 640, by = 10))
  slow <- make_iat_block(seq(550, 740, by = 10))

  expect_gt(cleanIAT.noprac(fast, slow)$D, 0)
  expect_lt(cleanIAT.noprac(slow, fast)$D, 0)
})
