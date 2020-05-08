########## STEP SIX: RELIABILITY ANALYSIS
#' Data analysis function: Cronbach's Alpha
#' @description  An alternative way of assessing the internal consistency of the IAT is to use Cronbach's Alpha. This method, used by Schnabel, Asendorpf, & Greenwald (2008) creates pairs of reaction times from compatible/incompatible blocks, calculates their difference scores, and enters those into a Cronbach's alpha analysis.
#' @param data an object created by \code{cleanIAT()} representing a cleaned IAT.
#' @return Returns a list with alphas based on the practice trials, the critical trials, and the entire IAT.
#' @references Schnabel, K., Asendorpf, J. B., & Greenwald, A. G. (2008). Using Implicit Association Tests for the Assessment of Implicit Personality Self-Concept. In The SAGE Handbook of Personality Theory and Assessment: Volume 2—Personality Measurement and Testing (pp. 508–528). SAGE Publications Ltd. https://doi.org/10.4135/9781849200479.n24
#' @examples \dontrun{
#' ### RELIABILITY ANALYSIS - ESTIMATE ONLY ###
#' IATalpha(clean)$alpha.total
#'
#' ### RELIABILITY ANALYSIS - ENTIRE RELIABILITY OUTPUT ###
#' IATalpha(clean)
#' }

IATalpha <- function(data){
  require(psych)
  prac1 <- data$clean.latencies.prac1
  prac2 <- data$clean.latencies.prac2
  crit1 <- data$clean.latencies.crit1
  crit2 <- data$clean.latencies.crit2

  #matricies must have exact same dimensions.
  # Any weird or extra trials due to error will cause issues. id correct dimensions and coerce.
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  prac1.cols <- getmode(rowSums(!is.na(prac1[!data$skipped & !data$drop.participant,])))
  prac2.cols <- getmode(rowSums(!is.na(prac2[!data$skipped & !data$drop.participant,])))
  crit1.cols <- getmode(rowSums(!is.na(crit1[!data$skipped & !data$drop.participant,])))
  crit2.cols <- getmode(rowSums(!is.na(crit2[!data$skipped & !data$drop.participant,])))

  prac1 <- prac1[,1:prac1.cols]
  prac2 <- prac2[,1:prac2.cols]
  crit1 <- crit1[,1:crit1.cols]
  crit2 <- crit2[,1:crit2.cols]

  prac <- prac1-prac2
  crit <- crit1-crit2

  df <- cbind(prac, crit, make.row.names = FALSE)

  alpha.prac <- suppressMessages(suppressWarnings(psych::alpha(prac)$total[1]))
  alpha.crit <- suppressMessages(suppressWarnings(psych::alpha(crit)$total[1]))
  alpha.total <- suppressMessages(suppressWarnings(psych::alpha(df)$total[1]))

  return(list(alpha.prac=alpha.prac, alpha.crit=alpha.crit, alpha.total=alpha.total))
}
