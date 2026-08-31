context("cleaning rules apply identically to both blocks in cleanIAT.noprac")

# cleanIAT.noprac() carries its own copies of the same cleaning loops as cleanIAT(),
# one per critical block. Same reasoning as test-cleaniat-blocks.R: drive the anomaly
# through both positions so a typo in either copy shows up.

NOPRAC_BLOCKS <- c("crit1", "crit2")

noprac_blocks_with <- function(position, anomalous, normal) {
  args <- rep(list(normal), 2)
  args[[position]] <- anomalous
  args
}

noprac_counters <- function(clean, prefix) {
  vapply(NOPRAC_BLOCKS, function(b) as.numeric(clean[[paste0(prefix, ".", b)]]), numeric(1))
}

varied <- make_iat_block(seq(450, 640, by = 10))


test_that("timeout dropping is counted in whichever block the slow trial is in", {
  slow <- make_iat_block(c(seq(450, 630, by = 10), 12000))

  for (position in seq_along(NOPRAC_BLOCKS)) {
    clean <- do.call(cleanIAT.noprac, noprac_blocks_with(position, slow, varied))

    expected <- as.numeric(seq_along(NOPRAC_BLOCKS) == position)
    expect_equal(
      noprac_counters(clean, "num.timeout.removed"),
      setNames(expected, NOPRAC_BLOCKS),
      info = paste("slow trial placed in", NOPRAC_BLOCKS[position])
    )
    expect_equal(clean$num.timeout.removed, 1)
  }
})

test_that("fast-trial dropping is counted in whichever block the fast trial is in", {
  fast <- make_iat_block(c(seq(450, 630, by = 10), 200))

  for (position in seq_along(NOPRAC_BLOCKS)) {
    args <- noprac_blocks_with(position, fast, varied)
    clean <- do.call(cleanIAT.noprac, c(
      args,
      list(fasttrial.drop = TRUE, fasttrial.ms = 400)
    ))

    expected <- as.numeric(seq_along(NOPRAC_BLOCKS) == position)
    expect_equal(
      noprac_counters(clean, "num.fasttrial.removed"),
      setNames(expected, NOPRAC_BLOCKS),
      info = paste("fast trial placed in", NOPRAC_BLOCKS[position])
    )
  }
})

test_that("the D600 penalty is applied in whichever block the error is in", {
  witherror <- make_iat_block(c(rep(500, 9), 900), correct = c(rep(TRUE, 9), FALSE))
  normal <- make_iat_block(seq(450, 540, by = 10))

  for (position in seq_along(NOPRAC_BLOCKS)) {
    args <- noprac_blocks_with(position, witherror, normal)
    clean <- do.call(cleanIAT.noprac, c(
      args,
      list(error.penalty = TRUE, error.penalty.ms = 600)
    ))

    latencies <- clean[[paste0("clean.latencies.", NOPRAC_BLOCKS[position])]]
    expect_equal(as.numeric(latencies[1, 10]), 1100,
      info = paste("error placed in", NOPRAC_BLOCKS[position])
    )
  }
})

test_that("the 2SD penalty is applied in whichever block the error is in", {
  correct.latencies <- c(400, 450, 500, 550, 600)
  witherror <- make_iat_block(
    c(correct.latencies, 900),
    correct = c(rep(TRUE, 5), FALSE)
  )
  normal <- make_iat_block(seq(450, 500, by = 10))
  expected <- mean(correct.latencies) + 2 * stats::sd(correct.latencies)

  for (position in seq_along(NOPRAC_BLOCKS)) {
    args <- noprac_blocks_with(position, witherror, normal)
    clean <- do.call(cleanIAT.noprac, c(
      args,
      list(error.penalty = TRUE, error.penalty.ms = "2SD")
    ))

    latencies <- clean[[paste0("clean.latencies.", NOPRAC_BLOCKS[position])]]
    expect_equal(as.numeric(latencies[1, 6]), expected,
      info = paste("error placed in", NOPRAC_BLOCKS[position])
    )
  }
})


## --- participant-level exclusions -------------------------------------------

test_that("fast participants are dropped, and retained when the rule is disabled", {
  # 10 of 40 trials under 300 ms, above the .10 threshold
  fastblock <- make_iat_block(c(rep(250, 10), rep(500, 10)))

  dropped <- cleanIAT.noprac(fastblock, varied)
  expect_true(dropped$drop.participant)
  expect_equal(dropped$fastprt.count, 1)
  expect_true(is.na(dropped$D))

  kept <- cleanIAT.noprac(fastblock, varied, fastprt.drop = FALSE)
  expect_false(kept$drop.participant)
  expect_equal(kept$fastprt.count, 0)
  expect_false(is.na(kept$D))
})

test_that("participants who skipped are blanked in both blocks", {
  clean <- cleanIAT.noprac(c(varied, skipped_block()), c(varied, skipped_block()))

  expect_equal(clean$skipped, c(FALSE, TRUE))
  for (b in NOPRAC_BLOCKS) {
    expect_true(all(is.na(clean[[paste0("raw.latencies.", b)]][2, ])),
      info = paste("raw latencies for", b)
    )
    expect_true(all(is.na(clean[[paste0("clean.latencies.", b)]][2, ])),
      info = paste("clean latencies for", b)
    )
  }
  expect_true(is.na(clean$D[2]))
})

test_that("corrupted data is warned about and excluded", {
  corrupt <- "12C500,undefined,14C520,END"

  expect_warning(
    clean <- cleanIAT.noprac(c(varied, corrupt), c(varied, varied)),
    "web browser encountered an error"
  )

  expect_true(clean$skipped[2])
  expect_true(is.na(clean$D[2]))
  expect_false(is.na(clean$D[1]))
})


test_that("a negative latency is counted in whichever block it appears in", {
  negative <- make_iat_block(c(seq(450, 630, by = 10), -7124))

  for (position in seq_along(NOPRAC_BLOCKS)) {
    args <- noprac_blocks_with(position, negative, varied)
    clean <- suppressWarnings(do.call(cleanIAT.noprac, args))

    expected <- as.numeric(seq_along(NOPRAC_BLOCKS) == position)
    expect_equal(
      noprac_counters(clean, "num.negative.removed"),
      setNames(expected, NOPRAC_BLOCKS),
      info = paste("negative latency placed in", NOPRAC_BLOCKS[position])
    )
    expect_equal(clean$num.negative.removed, 1)
  }
})

test_that("cleanIAT.noprac keeps a participant who has a negative latency", {
  negative <- make_iat_block(c(seq(450, 630, by = 10), -7124))

  clean <- suppressWarnings(cleanIAT.noprac(negative, varied))

  expect_false(clean$skipped)
  expect_false(is.na(clean$D))
  expect_equal(clean$num.negative.removed, 1)
})
