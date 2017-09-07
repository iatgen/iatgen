library(stringr)


########## STEP SIX: RELIABILITY ANALYSIS
#' Data analysis function: Estimate reliability of IAT
#' @description  One can easily estimate the IAT reliability by scoring the IAT separately based on odd and even trials and compute a split-half reliability. This is accomplished using \code{IATreliability()}, which sorts trials in order by type (positive, negative, target A, target B), takes alternating trials in order of presentation, scores and correlates the IAT, and applies a split-half spearman-brown correction (De Houwer & De Bruycker, 2007). This ensures an even distribution of targets and categories in odd/even trialsets. 
#' @param data An object created by \code{cleanIAT()} representing a cleaned IAT.
#' @return Returns reliability estimate and split-half correlation. In addition, D scores for odd and even trials can be viewed, as can the component practice and critical D scores.
#' @references De Houwer, J., & De Bruycker, E. (2007). The Implicit Association Test outperforms the extrinsic affective Simon task as an implicit measure of inter-individual differences in attitudes. \emph{British Journal of Social Psychology, 46}, 401–421. https://doi.org/10.1348/014466606X130346
#' @examples \dontrun{
#' ### RELIABILITY ANALYSIS - ESTIMATE ONLY ###
#' IATreliability(clean)$reliability
#' 
#' ### RELIABILITY ANALYSIS - ENTIRE RELIABILITY OUTPUT ###
#' IATreliability(clean)
#' }
IATreliability <- function(data, inclusive.sd=TRUE){
  
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
  

  
  
  ## BLOCK 1
  odd.prac.1 <- seq(1, ncol(b1.prac), by=2)
  even.prac.1 <- seq(2, ncol(b1.prac), by=2)
  
  odd.crit.1 <- seq(1, ncol(b1.crit), by=2)
  even.crit.1 <- seq(2, ncol(b1.crit), by=2)
  
  # save latencies and means for block 1
  odd.prac.latencies1 <- b1.prac[,odd.prac.1]
  even.prac.latencies1 <- b1.prac[,even.prac.1]
  odd.prac.means1 <- rowMeans(odd.prac.latencies1, na.rm=T)
  even.prac.means1 <- rowMeans(even.prac.latencies1, na.rm=T)
  odd.prac.means1[is.nan(odd.prac.means1)] <- NA
  even.prac.means1[is.nan(even.prac.means1)] <- NA
  
  odd.crit.latencies1 <- b1.crit[,odd.crit.1]
  even.crit.latencies1 <- b1.crit[,even.crit.1]
  odd.crit.means1 <- rowMeans(odd.crit.latencies1, na.rm=T)
  even.crit.means1 <- rowMeans(even.crit.latencies1, na.rm=T)
  odd.crit.means1[is.nan(odd.crit.means1)] <- NA
  even.crit.means1[is.nan(even.crit.means1)] <- NA
  
  ## BLOCK 2
  odd.prac.2 <- seq(1, ncol(b2.prac), by=2)
  even.prac.2 <- seq(2, ncol(b2.prac), by=2)
  
  odd.crit.2 <- seq(1, ncol(b2.crit), by=2)
  even.crit.2 <- seq(2, ncol(b2.crit), by=2)
  
  # save latencies and means for block 2
  odd.prac.latencies2 <- b2.prac[,odd.prac.2]
  even.prac.latencies2 <- b2.prac[,even.prac.2]
  odd.prac.means2 <- rowMeans(odd.prac.latencies2, na.rm=T)
  even.prac.means2 <- rowMeans(even.prac.latencies2, na.rm=T)
  odd.prac.means2[is.nan(odd.prac.means2)] <- NA
  even.prac.means2[is.nan(even.prac.means2)] <- NA
  
  odd.crit.latencies2 <- b2.crit[,odd.crit.2]
  even.crit.latencies2 <- b2.crit[,even.crit.2]
  odd.crit.means2 <- rowMeans(odd.crit.latencies2, na.rm=T)
  even.crit.means2 <- rowMeans(even.crit.latencies2, na.rm=T)
  odd.crit.means2[is.nan(odd.crit.means2)] <- NA
  even.crit.means2[is.nan(even.crit.means2)] <- NA
  
  
  diff.prac.odd <- odd.prac.means2 - odd.prac.means1 
  inclusive.sd.prac.odd <- sd(cbind(as.matrix(odd.prac.latencies1), as.matrix(odd.prac.latencies2)), na.rm=T)
  D1.prac <- diff.prac.odd / inclusive.sd.prac.odd
  
  diff.crit.odd <- odd.crit.means2 - odd.crit.means1 
  inclusive.sd.crit.odd <- sd(cbind(as.matrix(odd.crit.latencies1), as.matrix(odd.crit.latencies2)), na.rm=T)
  D1.crit <- diff.crit.odd / inclusive.sd.crit.odd
  
  diff.prac.even <- even.prac.means2 - even.prac.means1 
  inclusive.sd.prac.even <- sd(cbind(as.matrix(even.prac.latencies1), as.matrix(even.prac.latencies2)), na.rm=T)
  D2.prac <- diff.prac.even / inclusive.sd.prac.even
  
  diff.crit.even <- even.crit.means2 - even.crit.means1 
  inclusive.sd.crit.even <- sd(cbind(as.matrix(even.crit.latencies1), as.matrix(even.crit.latencies2)), na.rm=T)
  D2.crit <- diff.crit.even / inclusive.sd.crit.even
  
  
  D1 <- ((D1.prac + D1.crit) / 2)
  D2 <- ((D2.prac + D2.crit) / 2)
  
  splithalfcorr <- cor(D1, D2, use="pairwise.complete.obs")
  reliability <- (2*splithalfcorr) / (1 + splithalfcorr)
  return(list(reliability=reliability, splithalfcorr=splithalfcorr, D.odd = D1, D.even = D2, D.prac.odd=D1.prac, D.crit.odd=D1.crit, D.prac.even=D2.prac, D.crit.even=D2.crit))
}


