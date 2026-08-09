context("cleanIAT input validation")

# cleanIAT() has guards for missing variables, empty variables and data corrupted by a
# browser malfunction mid-survey. None of them were reached by the existing tests.

test_that("a missing input variable is rejected", {
  block <- make_iat_block(rep(500, 10))

  # Passing dat$typo yields NULL; the message should point at variable names.
  expect_error(
    cleanIAT(NULL, block, block, block),
    "does not exist"
  )
  expect_error(
    cleanIAT(block, NULL, block, block),
    "does not exist"
  )
  expect_error(
    cleanIAT(block, block, NULL, block),
    "does not exist"
  )
  expect_error(
    cleanIAT(block, block, block, NULL),
    "does not exist"
  )
})

test_that("an all-missing input variable is rejected", {
  block <- make_iat_block(rep(500, 10))

  expect_error(cleanIAT(NA, block, block, block), "empty")
  expect_error(cleanIAT(block, NA, block, block), "empty")
  expect_error(cleanIAT(block, block, NA, block), "empty")
  expect_error(cleanIAT(block, block, block, NA), "empty")
})

test_that("a block that is blank for every participant is reported clearly", {
  # The commonest cause is a mistyped variable name, which yields a column of blanks
  # rather than NA. This used to fail deep inside the parsing loops with
  # "argument is of length zero", which gave no hint as to the cause.
  block <- make_iat_block(rep(500, 10))
  blank <- c("", "")

  expect_error(
    cleanIAT(blank, c(block, block), c(block, block), c(block, block)),
    "No usable IAT data in: prac1"
  )
  expect_error(
    cleanIAT(c(block, block), blank, c(block, block), c(block, block)),
    "No usable IAT data in: crit1"
  )
  expect_error(
    cleanIAT(c(block, block), c(block, block), blank, c(block, block)),
    "No usable IAT data in: prac2"
  )
  expect_error(
    cleanIAT(c(block, block), c(block, block), c(block, block), blank),
    "No usable IAT data in: crit2"
  )
})

test_that("every block being blank names all four blocks", {
  expect_error(
    cleanIAT(c("", ""), c("", ""), c("", ""), c("", "")),
    "prac1, crit1, prac2, crit2"
  )
})

test_that("a block blanked entirely by corrupt data is reported the same way", {
  # Corrupt responses are blanked out during cleaning, so a block can become empty
  # after the initial checks have already passed.
  block <- make_iat_block(rep(500, 10))
  corrupt <- c("12C500,undefined,END", "12C500,undefined,END")

  expect_error(
    suppressWarnings(
      cleanIAT(corrupt, c(block, block), c(block, block), c(block, block))
    ),
    "No usable IAT data in: prac1"
  )
})

test_that("participants who skipped the IAT are flagged and scored NA", {
  block <- make_iat_block(seq(450, 640, by = 10))

  clean <- cleanIAT(
    c(block, skipped_block()),
    c(block, skipped_block()),
    c(block, skipped_block()),
    c(block, skipped_block())
  )

  expect_equal(clean$skipped, c(FALSE, TRUE))
  expect_true(is.na(clean$D[2]))
  expect_false(is.na(clean$D[1]))
})

test_that("a response string without a terminating END counts as not completed", {
  # iatgen writes "END" once the block finishes; its absence means the participant
  # left partway through and the data cannot be trusted.
  block <- make_iat_block(seq(450, 640, by = 10))
  truncated <- sub(",END$", "", block)

  clean <- cleanIAT(
    c(block, truncated),
    c(block, block),
    c(block, block),
    c(block, block)
  )

  expect_true(clean$skipped[2])
  expect_true(is.na(clean$D[2]))
})

test_that("data corrupted by a browser malfunction is warned about and excluded", {
  # Only digits, commas, C, X and END are legal. Anything else means the JavaScript
  # malfunctioned on the participant's machine.
  block <- make_iat_block(seq(450, 640, by = 10))
  corrupt <- "12C500,undefined,14C520,END"

  expect_warning(
    clean <- cleanIAT(
      c(block, corrupt),
      c(block, block),
      c(block, block),
      c(block, block)
    ),
    "web browser encountered an error"
  )

  expect_true(clean$skipped[2])
  expect_true(is.na(clean$D[2]))
  # the valid participant is unaffected
  expect_false(is.na(clean$D[1]))
})

test_that("valid data produces no corruption warning", {
  block <- make_iat_block(seq(450, 640, by = 10))
  slow <- make_iat_block(seq(550, 740, by = 10))

  expect_silent(cleanIAT(block, block, slow, slow))
})

test_that("single-digit and double-digit stimulus numbers both parse", {
  # Stimulus numbers below 10 are written without a leading zero, and cleanIAT has to
  # add one back before it can slice the string by position.
  block <- make_iat_block(seq(450, 640, by = 10), stim = 0:19)

  clean <- cleanIAT(block, block, block, block)

  expect_equal(as.numeric(clean$raw.stim.number.prac1[1, ]), 0:19)
  expect_equal(as.numeric(clean$raw.latencies.prac1[1, ]), seq(450, 640, by = 10))
})

test_that("correct and incorrect trials are recorded per trial", {
  pattern <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  block <- make_iat_block(rep(500, 10), correct = pattern)

  clean <- cleanIAT(block, block, block, block)

  expect_equal(as.character(clean$raw.correct.prac1[1, ]), ifelse(pattern, "C", "X"))
  expect_equal(as.numeric(clean$error.num.prt), 4 * sum(!pattern))
})
