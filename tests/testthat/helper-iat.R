# Shared helpers for the iatgen test suite.
#
# Three things live here:
#   1. load_iat_fixture()  - loads the sample Qualtrics export used by most tests
#   2. make_iat_block()    - builds synthetic IAT response strings with known properties
#   3. dscore_oracle()     - an independent implementation of the Greenwald et al. (2003)
#                            D-score, written from the published definition rather than
#                            from cleanIAT(), so that the two can be cross-checked


## ---------------------------------------------------------------------------
## 1. Fixture loading
## ---------------------------------------------------------------------------

# Reads iat_small.csv (dropping the second row, which holds Qualtrics' column
# descriptions rather than data) and collapses the four permutations down into the
# four variables the analysis functions expect.
load_iat_fixture <- function(filename = "iat_small.csv") {
  allContent <- readLines(filename, encoding = "UTF-8")
  allContent <- allContent[-2]
  dat <- read.csv(textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)

  # Only one permutation carries data in this fixture, so combineIATfourblocks()
  # legitimately warns about the empty ones. See test-combine.R for tests that
  # exercise the combining logic itself.
  suppressWarnings({
    dat$compatible.prac <- combineIATfourblocks(dat$Q3.RP3, dat$Q17.LP3, dat$Q13.RN6, dat$Q27.LN6)
    dat$compatible.crit <- combineIATfourblocks(dat$Q4.RP4, dat$Q18.LP4, dat$Q14.RN7, dat$Q28.LN7)
    dat$incompatible.prac <- combineIATfourblocks(dat$Q6.RP6, dat$Q20.LP6, dat$Q10.RN3, dat$Q24.LN3)
    dat$incompatible.crit <- combineIATfourblocks(dat$Q7.RP7, dat$Q21.LP7, dat$Q11.RN4, dat$Q25.LN4)
  })

  dat
}

# iat_demo_synthetic.csv is MACHINE-GENERATED DEMO DATA, not real participants - see
# data-raw/make_demo_data.R. It is a placeholder for a real anonymised export and must
# not be treated as evidence about anything. It exists because iat_small.csv holds only
# two participants who both saw the same permutation, which cannot support tests of
# reliability, internal consistency, or the permutation-combining logic.
#
# Because the values are arbitrary, tests built on it should assert properties and
# relationships that any valid IAT dataset must satisfy, never specific numbers -
# otherwise swapping in real data will break them for no good reason.
DEMO_FIXTURE <- "iat_demo_synthetic.csv"

# Cleaning is the slow step and several tests need the same result, so cache it.
.clean_cache <- new.env(parent = emptyenv())

load_demo_fixture <- function() load_iat_fixture(DEMO_FIXTURE)

clean_demo_fixture <- function() {
  if (!is.null(.clean_cache$demo)) {
    return(.clean_cache$demo)
  }
  dat <- load_demo_fixture()
  clean <- suppressWarnings(cleanIAT(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit
  ))
  .clean_cache$demo <- clean
  clean
}

# Convenience: the cleaned IAT that most tests start from. The default-options result
# is cached, since many tests need it and re-parsing the CSV each time is pure cost.
clean_iat_fixture <- function(...) {
  cacheable <- length(list(...)) == 0L
  if (cacheable && !is.null(.clean_cache$default)) {
    return(.clean_cache$default)
  }

  dat <- load_iat_fixture()
  clean <- suppressWarnings(cleanIAT(
    dat$compatible.prac, dat$compatible.crit,
    dat$incompatible.prac, dat$incompatible.crit,
    ...
  ))

  if (cacheable) .clean_cache$default <- clean
  clean
}

# psych::alpha() prints its "negatively correlated" diagnostic with cat(), which the
# suppressMessages() inside IATalpha() cannot intercept. Swallow it so it does not
# bury the test output.
quiet_IATalpha <- function(clean) {
  result <- NULL
  invisible(utils::capture.output(result <- IATalpha(clean)))
  result
}


## ---------------------------------------------------------------------------
## 2. Synthetic response strings
## ---------------------------------------------------------------------------

# iatgen encodes a block of trials as "<stim><C|X><latency>," repeated, then "END".
# Building these directly lets tests state exactly what went in and assert exactly
# what should come out, instead of relying on opaque numbers from a recorded session.
#
#   make_iat_block(c(500, 600))            -> "0C500,1C600,END"
#   make_iat_block(c(500, 600), c(TRUE, FALSE)) -> "0C500,1X600,END"
make_iat_block <- function(latencies, correct = TRUE, stim = NULL) {
  n <- length(latencies)
  if (is.null(stim)) stim <- seq_len(n) - 1L
  correct <- rep_len(correct, n)
  stim <- rep_len(stim, n)
  paste0(
    paste0(stim, ifelse(correct, "C", "X"), latencies, collapse = ","),
    ",END"
  )
}

# A participant who skipped the IAT entirely.
skipped_block <- function() ""


## ---------------------------------------------------------------------------
## 2b. Survey-building fixtures
## ---------------------------------------------------------------------------

# The flowers/insects IAT used by the QSF tests. These arguments must stay exactly as
# they are: expected_iat-flowins.qsf is a byte-for-byte record of this recipe's output.
canned_iat_args <- function() {
  list(
    IATname = "flowins",
    posname = "Pleasant",
    negname = "Unpleasant",
    Aname = "Flowers",
    Bname = "Insects",
    catType = "words",
    poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
    negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
    tgtType = "words",
    Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
    Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),

    # advanced options with recommended IAT settings
    n = c(20, 20, 20, 40, 40, 20, 40),
    qsf = T,
    note = T,
    correct.error = T,
    pause = 250,
    tgtCol = "black",
    catCol = "green"
  )
}

# Named arguments passed here *replace* the canned ones rather than being appended,
# so that tests can vary a single setting without triggering an argument-matching
# error that could be mistaken for the validation error under test.
writeIATfull_canned_recipe <- function(...) {
  do.call(writeIATfull, utils::modifyList(canned_iat_args(), list(...)))
}

# Runs `code` with the working directory set to a fresh empty temporary directory,
# restoring the original directory afterwards even if `code` fails. Keeps the
# file-writing tests from leaving anything behind in tests/testthat.
in_temp_dir <- function(code) {
  dir <- file.path(tempdir(), paste0("iatgen-test-", as.integer(runif(1, 1, 1e9))))
  dir.create(dir, recursive = TRUE)
  old <- setwd(dir)
  on.exit(
    {
      setwd(old)
      unlink(dir, recursive = TRUE)
    },
    add = TRUE
  )
  force(code)
}


## ---------------------------------------------------------------------------
## 3. Independent D-score oracle
## ---------------------------------------------------------------------------

# Parses a response string without reusing any of cleanIAT()'s parsing logic.
# Returns a data frame of stim number, correctness and latency.
parse_iat_string <- function(s) {
  empty <- data.frame(stim = integer(0), correct = logical(0), latency = numeric(0))
  if (is.na(s) || !nzchar(s)) {
    return(empty)
  }
  body <- sub(",?END$", "", s)
  if (!nzchar(body)) {
    return(empty)
  }
  trials <- strsplit(body, ",", fixed = TRUE)[[1]]
  trials <- trials[nzchar(trials)]
  m <- regmatches(trials, regexec("^([0-9]+)([CX])([0-9]+)$", trials))
  ok <- vapply(m, length, integer(1)) == 4L
  if (!all(ok)) {
    stop("parse_iat_string() could not parse: ", paste(trials[!ok], collapse = ", "))
  }
  data.frame(
    stim = as.integer(vapply(m, `[`, character(1), 2)),
    correct = vapply(m, `[`, character(1), 3) == "C",
    latency = as.numeric(vapply(m, `[`, character(1), 4))
  )
}

# The D-score exactly as described by Greenwald, Nosek & Banaji (2003, p. 214,
# centre column), for the forced-error-correction variant iatgen produces by
# default: drop trials over the timeout, take the difference of block means, and
# divide by the *inclusive* SD of all remaining trials in the two blocks combined.
#
# Deliberately written from the paper rather than derived from cleanIAT(), so that
# agreement between the two is real evidence rather than a tautology.
dscore_oracle <- function(prac1, crit1, prac2, crit2, timeout.ms = 10000) {
  one <- function(a, b) {
    ta <- parse_iat_string(a)$latency
    tb <- parse_iat_string(b)$latency
    ta <- ta[ta <= timeout.ms]
    tb <- tb[tb <= timeout.ms]
    if (!length(ta) || !length(tb)) {
      return(NA_real_)
    }
    (mean(tb) - mean(ta)) / stats::sd(c(ta, tb))
  }
  vapply(seq_along(prac1), function(i) {
    d.prac <- one(prac1[i], prac2[i])
    d.crit <- one(crit1[i], crit2[i])
    (d.prac + d.crit) / 2
  }, numeric(1))
}
