library(stringr)


########## STEP SIX: RELIABILITY ANALYSIS
#' Data analysis function: Estimate reliability of IAT
#' @description  One can easily estimate the IAT reliability by scoring the IAT separately based on odd and even trials and compute a split-half reliability. This is accomplished using \code{IATreliability()}, which sorts trials in order by type (positive, negative, target A, target B), takes alternating trials in order of presentation, scores and correlates the IAT, and applies a split-half spearman-brown correction (De Houwer & De Bruycker, 2007). This ensures an even distribution of targets and categories in odd/even trialsets. 
#' @param data An object created by \code{cleanIAT()} representing a cleaned IAT.
#' @return Returns reliability estimate and split-half correlation.
#' @references De Houwer, J., & De Bruycker, E. (2007). The Implicit Association Test outperforms the extrinsic affective Simon task as an implicit measure of inter-individual differences in attitudes. \emph{British Journal of Social Psychology, 46}, 401–421. https://doi.org/10.1348/014466606X130346
#' @examples \dontrun{
#' ### RELIABILITY ANALYSIS - ESTIMATE ONLY ###
#' IATreliability(clean)$reliability
#' 
#' ### RELIABILITY ANALYSIS - ENTIRE RELIABILITY OUTPUT ###
#' IATreliability(clean)
#' }
IATreliability <- function(data){
  
  #put clean latencies in order by stim number, which sorts by pos, neg, A, and B trials and within that, order of presentation    
  b1.prac <-c()
  for (i in 1:nrow(data$clean.latencies.prac1)){
    temp <- data$clean.latencies.prac1[i,]
    temp <- temp[order(data$raw.stim.number.prac1[i,])]
    b1.prac <- rbind(b1.prac, temp)
  }
  
  b2.prac <-c()
  for (i in 1:nrow(data$clean.latencies.prac2)){
    temp <- data$clean.latencies.prac2[i,]
    temp <- temp[order(data$raw.stim.number.prac2[i,])]
    b2.prac <- rbind(b2.prac, temp)
  }
  
  b1.crit <-c()
  for (i in 1:nrow(data$clean.latencies.crit1)){
    temp <- data$clean.latencies.crit1[i,]
    temp <- temp[order(data$raw.stim.number.crit1[i,])]
    b1.crit <- rbind(b1.crit, temp)
  }
  
  b2.crit <-c()
  for (i in 1:nrow(data$clean.latencies.crit2)){
    temp <- data$clean.latencies.crit2[i,]
    temp <- temp[order(data$raw.stim.number.crit2[i,])]
    b2.crit <- rbind(b2.crit, temp)
  }
  
  b1 <- cbind(b1.prac, b1.crit)
  b2 <- cbind(b2.prac, b2.crit)
  
  
  ## BLOCK 1
  odd1 <- seq(1, ncol(b1), by=2)
  even1 <- seq(2, ncol(b1), by=2)
  
  # save latencies and means for block 1
  odd.latencies1 <- b1[,odd1]
  even.latencies1 <- b1[,even1]
  oddmeans1 <- rowMeans(odd.latencies1, na.rm=T)
  evenmeans1 <- rowMeans(even.latencies1, na.rm=T)
  oddmeans1[is.nan(oddmeans1)] <- NA
  evenmeans1[is.nan(evenmeans1)] <- NA
  
  # get number of clean trials by row for variance calculation
  
  
  num.clean.odd1 <- rowSums(!is.na(odd.latencies1))
  num.clean.odd1[data$skipped] <- NA
  num.clean.even1 <- rowSums(!is.na(even.latencies1))
  num.clean.even1[data$skipped] <- NA
  
  # odd sd for block 1
  oddsd1 <- numeric()
  for(i in 1:nrow(odd.latencies1)){
    row <- odd.latencies1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.odd1[i]
    oddsd1[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.odd1[i]-1))
  }
  oddsd1[data$skipped] <- NA
  oddsd1[oddsd1==0] <- NA
  
  evensd1 <- numeric()
  for(i in 1:nrow(even.latencies1)){
    row <- even.latencies1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.even1[i]
    evensd1[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.even1[i]-1))
  }
  evensd1[data$skipped] <- NA
  evensd1[evensd1==0] <- NA
  
  
  ## BLOCK 2
  odd2 <- seq(1, ncol(b2), by=2)
  even2 <- seq(2, ncol(b2), by=2)
  
  # save latencies and means for block 2
  odd.latencies2 <- b2[,odd2]
  even.latencies2 <- b2[,even2]
  oddmeans2 <- rowMeans(odd.latencies2, na.rm=T)
  evenmeans2 <- rowMeans(even.latencies2, na.rm=T)
  oddmeans2[is.nan(oddmeans2)] <- NA
  evenmeans2[is.nan(evenmeans2)] <- NA
  
  # get number of clean trials by row for sd calculation
  num.clean.odd2 <- rowSums(!is.na(odd.latencies2))
  num.clean.odd2[data$skipped] <- NA
  num.clean.even2 <- rowSums(!is.na(even.latencies2))
  num.clean.even2[data$skipped] <- NA
  
  # odd sd for block 2
  oddsd2 <- numeric()
  for(i in 1:nrow(odd.latencies2)){
    row <- odd.latencies2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.odd2[i]
    oddsd2[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.odd2[i]-1))
  }
  oddsd2[data$skipped] <- NA
  oddsd2[oddsd2==0] <- NA
  
  evensd2 <- numeric()
  for(i in 1:nrow(even.latencies2)){
    row <- even.latencies2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.even2[i]
    evensd2[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.even2[i]-1))
  }
  evensd2[data$skipped] <- NA
  evensd2[evensd2==0] <- NA
  
  diff.odd <- oddmeans2 - oddmeans1 
  pool.sd.odd<- sqrt((oddsd1^2 * (num.clean.odd1-1) + oddsd2^2 * (num.clean.odd2-1)) / (num.clean.odd1-1 + num.clean.odd2-1))
  D1 <- diff.odd / pool.sd.odd
  
  diff.even <- evenmeans2 - evenmeans1 
  pool.sd.even<- sqrt((evensd1^2 * (num.clean.even1-1) + evensd2^2 * (num.clean.even2-1)) / (num.clean.even1-1 + num.clean.even2-1))
  D2 <- diff.even / pool.sd.even
  
  splithalfcorr <- cor(D1, D2, use="pairwise.complete.obs")
  reliability <- (2*splithalfcorr) / (1 + splithalfcorr)
  return(list(reliability=reliability, splithalfcorr=splithalfcorr, D.odd = D1, D.even = D2))
}


