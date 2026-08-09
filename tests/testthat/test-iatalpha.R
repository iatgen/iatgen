context("alpha")

test_that("IATalpha", {
  clean <- clean_iat_fixture()

  alpha <- quiet_IATalpha(clean)
  alpha_total <- as.numeric(alpha$alpha.total)
  expect_equal(round(alpha_total, 4), 0.9444)
})

test_that("IATalpha reports practice, critical and total alphas", {
  clean <- clean_iat_fixture()

  alpha <- quiet_IATalpha(clean)

  expect_named(alpha, c("alpha.prac", "alpha.crit", "alpha.total", "diffscores"))
  for (a in c(alpha$alpha.prac, alpha$alpha.crit, alpha$alpha.total)) {
    expect_true(is.finite(as.numeric(a)))
    expect_lte(as.numeric(a), 1)
  }
})

test_that("the difference-score matrix pairs every practice and critical trial", {
  clean <- clean_iat_fixture()

  alpha <- quiet_IATalpha(clean)

  # one row per participant, one column per paired trial (20 practice + 40 critical)
  expect_equal(nrow(alpha$diffscores), 2)
  expect_equal(ncol(alpha$diffscores), 60)
  expect_equal(colnames(alpha$diffscores), paste0("trial", 1:60))
})

test_that("difference scores are compatible minus incompatible latencies", {
  clean <- clean_iat_fixture()

  alpha <- quiet_IATalpha(clean)

  # the first 20 columns come from the practice blocks
  expect_equal(
    as.numeric(alpha$diffscores[1, 1:20]),
    as.numeric(clean$clean.latencies.prac1[1, ] - clean$clean.latencies.prac2[1, ])
  )
})
