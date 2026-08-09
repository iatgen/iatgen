context("cleaning rules apply identically to every block")

# cleanIAT() handles prac1, crit1, prac2 and crit2 with four near-identical copies of
# each cleaning loop. A typo in one copy would leave the other three correct and be
# invisible to any test that only ever puts its test data in the first block - which is
# what the rest of the suite did. These tests move the same anomaly through all four
# block positions and check the matching counter, and only that counter, responds.

BLOCKS <- c("prac1", "crit1", "prac2", "crit2")

# Build the four cleanIAT() arguments, placing `anomalous` in one position and
# `normal` in the other three.
blocks_with <- function(position, anomalous, normal) {
  args <- rep(list(normal), 4)
  args[[position]] <- anomalous
  args
}

# Pull the four per-block counters out of a cleaned IAT in prac1, crit1, prac2, crit2
# order, e.g. counters(clean, "num.timeout.removed").
counters <- function(clean, prefix) {
  vapply(BLOCKS, function(b) as.numeric(clean[[paste0(prefix, ".", b)]]), numeric(1))
}

varied <- make_iat_block(seq(450, 640, by = 10))


## --- timeouts ---------------------------------------------------------------

test_that("timeout dropping is counted in whichever block the slow trial is in", {
  slow <- make_iat_block(c(seq(450, 630, by = 10), 12000))

  for (position in seq_along(BLOCKS)) {
    clean <- do.call(cleanIAT, blocks_with(position, slow, varied))

    expected <- as.numeric(seq_along(BLOCKS) == position)
    expect_equal(counters(clean, "num.timeout.removed"), setNames(expected, BLOCKS),
      info = paste("slow trial placed in", BLOCKS[position])
    )
    expect_equal(clean$num.timeout.removed, 1)
    # the offending trial is blanked in that block only
    expect_true(is.na(clean[[paste0("clean.latencies.", BLOCKS[position])]][1, 20]))
  }
})


## --- fast trials ------------------------------------------------------------

test_that("fast-trial dropping is counted in whichever block the fast trial is in", {
  fast <- make_iat_block(c(seq(450, 630, by = 10), 200))

  for (position in seq_along(BLOCKS)) {
    args <- blocks_with(position, fast, varied)
    clean <- do.call(cleanIAT, c(args, list(fasttrial.drop = TRUE, fasttrial.ms = 400)))

    expected <- as.numeric(seq_along(BLOCKS) == position)
    expect_equal(counters(clean, "num.fasttrial.removed"), setNames(expected, BLOCKS),
      info = paste("fast trial placed in", BLOCKS[position])
    )
    expect_equal(clean$num.fasttrial.removed, 1)
  }
})


## --- error penalties --------------------------------------------------------

test_that("the D600 penalty is applied in whichever block the error is in", {
  # nine correct trials at 500 ms and one error, so the correct-trial mean is 500 and
  # the penalised latency must come out at 1100 in every block.
  witherror <- make_iat_block(c(rep(500, 9), 900), correct = c(rep(TRUE, 9), FALSE))
  normal <- make_iat_block(seq(450, 540, by = 10))

  for (position in seq_along(BLOCKS)) {
    args <- blocks_with(position, witherror, normal)
    clean <- do.call(cleanIAT, c(
      args,
      list(error.penalty = TRUE, error.penalty.ms = 600)
    ))

    latencies <- clean[[paste0("clean.latencies.", BLOCKS[position])]]
    expect_equal(as.numeric(latencies[1, 10]), 1100,
      info = paste("error placed in", BLOCKS[position])
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

  for (position in seq_along(BLOCKS)) {
    args <- blocks_with(position, witherror, normal)
    clean <- do.call(cleanIAT, c(
      args,
      list(error.penalty = TRUE, error.penalty.ms = "2SD")
    ))

    latencies <- clean[[paste0("clean.latencies.", BLOCKS[position])]]
    expect_equal(as.numeric(latencies[1, 6]), expected,
      info = paste("error placed in", BLOCKS[position])
    )
  }
})


## --- error rates ------------------------------------------------------------

test_that("error rates are attributed to the block the errors are in", {
  # 2 errors in 10 trials
  witherrors <- make_iat_block(
    rep(500, 10),
    correct = c(FALSE, TRUE, TRUE, FALSE, rep(TRUE, 6))
  )
  normal <- make_iat_block(seq(450, 540, by = 10))

  for (position in seq_along(BLOCKS)) {
    clean <- do.call(cleanIAT, blocks_with(position, witherrors, normal))

    rates <- vapply(BLOCKS, function(b) clean[[paste0("error.rate.", b)]], numeric(1))
    expect_equal(rates, setNames(as.numeric(seq_along(BLOCKS) == position) * 0.2, BLOCKS),
      info = paste("errors placed in", BLOCKS[position])
    )
  }
})


## --- skipped participants ---------------------------------------------------

test_that("a participant who skipped is blanked in every block", {
  clean <- cleanIAT(
    c(varied, skipped_block()),
    c(varied, skipped_block()),
    c(varied, skipped_block()),
    c(varied, skipped_block())
  )

  for (b in BLOCKS) {
    expect_true(all(is.na(clean[[paste0("raw.latencies.", b)]][2, ])),
      info = paste("raw latencies for", b)
    )
    expect_true(all(is.na(clean[[paste0("clean.latencies.", b)]][2, ])),
      info = paste("clean latencies for", b)
    )
  }
})
