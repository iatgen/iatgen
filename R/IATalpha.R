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

  prac1.cols <- getmode(rowSums(!is.na(prac1)))
  prac2.cols <- getmode(rowSums(!is.na(prac2)))
  crit1.cols <- getmode(rowSums(!is.na(crit1)))
  crit2.cols <- getmode(rowSums(!is.na(crit2)))

  prac1 <- prac1[,1:prac1.cols]
  prac2 <- prac2[,1:prac2.cols]
  crit1 <- crit1[,1:crit1.cols]
  crit2 <- crit2[,1:crit2.cols]

  prac <- prac1-prac2
  crit <- crit1-crit2

  df <- cbind(prac, crit, make.row.names = FALSE)

  alpha.prac <- psych::alpha(prac)$total[1]
  alpha.crit <- psych::alpha(crit)$total[1]
  alpha.total <- psych::alpha(df)$total[1]

  return(list(alpha.prac=alpha.prac, alpha.crit=alpha.crit, alpha.total=alpha.total))
}
