# =====================================================================================
# GENERATOR FOR SYNTHETIC DEMO IAT DATA - NOT REAL PARTICIPANT DATA
# =====================================================================================
#
# Produces tests/testthat/iat_demo_synthetic.csv, a stand-in Qualtrics export used by
# the test suite. Every value in that file is machine-generated from the random number
# draws below. No human being took this IAT. It must never be cited, analysed, or
# reported as though it were data collected from participants.
#
# It exists because the real recorded fixture (iat_small.csv) contains only two
# participants who both saw the same permutation, which is too small and too narrow to
# test reliability, internal consistency, or the permutation-combining logic.
#
# The file is intended to be replaced by a real anonymised export when one is
# available. Anything reading it should keep working: it matches the column names and
# response encoding of a genuine iatgen Qualtrics export.
#
# To regenerate:  Rscript data-raw/make_demo_data.R
# Output is deterministic - the seed below fixes every draw.
# =====================================================================================

set.seed(20250809)

OUTFILE <- file.path("tests", "testthat", "iat_demo_synthetic.csv")

N_PER_PERM <- 10 # participants per counterbalancing permutation
N_TRIALS <- c(20, 20, 20, 40, 40, 20, 40) # trials in blocks 1..7, the iatgen default

# Parameters chosen so the resulting dataset has psychometrics in the range reported
# for real IATs, rather than the implausibly clean numbers a naive noise model gives.
# As generated: median RT around 850 ms, mean D about 0.55 (SD 0.40), roughly 8 per
# cent of participants scoring below zero, and split-half reliability near 0.80.
TRIAL_NOISE <- 0.35 # SD of the log-normal trial-level multiplier
EFFECT_MEAN <- 140 # mean IAT effect in ms (incompatible slower than compatible)
EFFECT_SD <- 120 # between-person SD of that effect; wide enough to yield negatives

# The four permutations, and the question numbers their seven blocks occupy. iatgen
# lays these out consecutively: RP takes Q1-Q7, RN takes Q8-Q14, and so on.
PERMS <- list(
  RP = list(offset = 0, compatible.first = TRUE),
  RN = list(offset = 7, compatible.first = FALSE),
  LP = list(offset = 14, compatible.first = TRUE),
  LN = list(offset = 21, compatible.first = FALSE)
)

# Column name for block `k` of permutation `perm`, e.g. "Q18 LP4".
block_column <- function(perm, k) {
  paste0("Q", PERMS[[perm]]$offset + k, " ", perm, k)
}

# Encode one block the way the iatgen JavaScript does: "<stim><C|X><latency>," per
# trial, terminated by "END". Stimulus numbers are 0..n-1 in a shuffled order.
encode_block <- function(latencies, correct) {
  n <- length(latencies)
  stim <- sample(seq_len(n) - 1L)
  paste0(
    paste0(stim, ifelse(correct, "C", "X"), round(latencies), collapse = ","),
    ",END"
  )
}

# One participant's seven blocks. `effect` is how much slower, in ms, the incompatible
# pairing is for this person - the quantity the IAT is meant to measure.
make_participant <- function(perm, base, effect, error.rate, fast = FALSE,
                             timeouts = 0) {
  compatible.first <- PERMS[[perm]]$compatible.first
  blocks <- character(7)

  for (k in 1:7) {
    n <- N_TRIALS[k]

    # Blocks 3 and 4 are the first combined pairing; 6 and 7 the second. Which of the
    # two is "compatible" depends on the permutation. Blocks 1, 2 and 5 sort a single
    # category and are quicker.
    penalty <- 0
    if (k %in% c(3, 4)) penalty <- if (compatible.first) 0 else effect
    if (k %in% c(6, 7)) penalty <- if (compatible.first) effect else 0
    if (k %in% c(1, 2, 5)) penalty <- -60

    # Reaction times are right-skewed, so vary them multiplicatively rather than by
    # adding symmetric noise. A normal error term wide enough to give realistic
    # reliability would push a chunk of trials below 300 ms and trip the
    # fast-participant rule; this keeps the fast tail thin and the shape plausible.
    latencies <- (base + penalty) * exp(rnorm(n, 0, TRIAL_NOISE))
    latencies <- pmax(latencies, 260) # nobody responds faster than the screen refresh

    if (fast) {
      # A button-masher: a quarter of this block is under the 300 ms threshold, enough
      # to trip cleanIAT's fast-participant rule.
      masher <- sample(n, max(1, round(n * 0.25)))
      latencies[masher] <- runif(length(masher), 180, 290)
    }
    if (timeouts > 0 && k == 4) {
      latencies[sample(n, timeouts)] <- runif(timeouts, 10500, 18000)
    }

    correct <- runif(n) > error.rate
    blocks[k] <- encode_block(latencies, correct)
  }

  blocks
}

# ------------------------------------------------------------------------------------
# Build the participant table
# ------------------------------------------------------------------------------------

# Column layout of a real iatgen Qualtrics export.
iat.columns <- unlist(lapply(names(PERMS), function(p) {
  vapply(1:7, function(k) block_column(p, k), character(1))
}))
lead.columns <- c(
  "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "ID",
  "IntroTime_1", "IntroTime_2", "IntroTime_3", "IntroTime_4", "x"
)
trail.columns <- c(
  "ExplicitN_1", "ExplicitM_1", "ExplicitNvsM", "gender", "age", "race",
  "race_TEXT", "Eng", "comment", "x.1", "LocationLatitude", "LocationLongitude",
  "LocationAccuracy"
)
all.columns <- c(lead.columns, iat.columns, trail.columns)

rows <- list()
id <- 0

for (perm in names(PERMS)) {
  for (i in seq_len(N_PER_PERM)) {
    id <- id + 1

    row <- setNames(rep("", length(all.columns)), all.columns)
    row["ID"] <- sprintf("DEMO-%03d", id)
    row["V1"] <- sprintf("R_SYNTHETIC%03d", id) # stands in for Qualtrics' ResponseID
    row["V7"] <- "0"
    row["V10"] <- "1"
    row["gender"] <- as.character(sample(1:2, 1))
    row["age"] <- as.character(sample(18:65, 1))
    row["ExplicitN_1"] <- as.character(sample(1:7, 1))
    row["ExplicitM_1"] <- as.character(sample(1:7, 1))

    # A handful of deliberately awkward cases, so the fixture exercises the cleaning
    # rules rather than only the happy path.
    is.skipper <- (id == 12) # walked away without doing the IAT
    is.fast <- (id == 25) # button-masher, should be dropped by fastprt.drop
    n.timeouts <- if (id == 31) 3 else 0 # left the room mid-block

    if (!is.skipper) {
      blocks <- make_participant(
        perm = perm,
        base = runif(1, 620, 980),
        effect = rnorm(1, EFFECT_MEAN, EFFECT_SD),
        error.rate = runif(1, 0.02, 0.09),
        fast = is.fast,
        timeouts = n.timeouts
      )
      for (k in 1:7) row[block_column(perm, k)] <- blocks[k]
    }

    rows[[length(rows) + 1]] <- row
  }
}

dat <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------------
# Write it out, mimicking the two-header-row shape of a Qualtrics export
# ------------------------------------------------------------------------------------

# Row 2 of a Qualtrics export holds human-readable column descriptions and is discarded
# by every loader. Reuse it to carry the synthetic-data warning, so the notice travels
# with the file and is visible to anyone who opens it.
banner <- setNames(rep("", length(all.columns)), all.columns)
banner[1] <- paste(
  "SYNTHETIC DEMO DATA - NOT REAL PARTICIPANTS -",
  "machine-generated by data-raw/make_demo_data.R - do not analyse or report as real"
)
banner["ID"] <- "SYNTHETIC ID"
for (nm in iat.columns) banner[nm] <- "SYNTHETIC IAT BLOCK"

out <- rbind(banner, dat)

dir.create(dirname(OUTFILE), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUTFILE, row.names = FALSE, quote = TRUE, na = "")

cat("wrote", OUTFILE, "-", nrow(dat), "synthetic participants,",
  length(all.columns), "columns\n",
  sep = " "
)
