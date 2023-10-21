context("analyze csv output")

test_that("Function to import and analyze csv", {
  filename <- "iat_small.csv"
  allContent <- readLines(filename, encoding = "UTF-8")
  allContent <- allContent[-2]
  dat <- read.csv(textConnection(allContent), header = TRUE, stringsAsFactors = FALSE)


  suppressWarnings(
    dat$compatible.crit <- combineIATfourblocks(dat$Q4.RP4, dat$Q18.LP4, dat$Q14.RN7, dat$Q28.LN7)
  )
  suppressWarnings(
    dat$incompatible.crit <- combineIATfourblocks(dat$Q7.RP7, dat$Q21.LP7, dat$Q11.RN4, dat$Q25.LN4)
  )
  suppressWarnings(
    dat$compatible.prac <- combineIATfourblocks(dat$Q3.RP3, dat$Q17.LP3, dat$Q13.RN6, dat$Q27.LN6)
  )
  suppressWarnings(
    dat$incompatible.prac <- combineIATfourblocks(dat$Q6.RP6, dat$Q20.LP6, dat$Q10.RN3, dat$Q24.LN3)
  )

  clean <- cleanIAT.noprac(dat$compatible.crit, dat$incompatible.crit)

  expect_equal(as.numeric(round(clean$D, 6)), c(0.210920, -0.249192))
})

