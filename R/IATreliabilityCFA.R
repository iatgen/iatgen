#### THREE FACTOR CFS ###

IATreliabilityCFA <- function(data){
  
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
  q1 <- seq(1, ncol(b1), by=3)
  q2 <- seq(2, ncol(b1), by=3)
  q3 <- seq(3, ncol(b1), by=3)
  
  
  
  # save latencies and means for block 1
  q1.latencies1 <- b1[,q1]
  q2.latencies1 <- b1[,q2]
  q3.latencies1 <- b1[,q3]
  
  
  q1means1 <- rowMeans(q1.latencies1, na.rm=T)
  q2means1 <- rowMeans(q2.latencies1, na.rm=T)
  q3means1 <- rowMeans(q3.latencies1, na.rm=T)
  
  q1means1[is.nan(q1means1)] <- NA
  q2means1[is.nan(q2means1)] <- NA
  q3means1[is.nan(q3means1)] <- NA
  
  
  
  # get number of clean trials by row for variance calculation
  
  num.clean.q1.1 <- rowSums(!is.na(q1.latencies1))
  num.clean.q1.1[data$skipped] <- NA
  
  num.clean.q2.1 <- rowSums(!is.na(q2.latencies1))
  num.clean.q2.1[data$skipped] <- NA
  
  num.clean.q3.1 <- rowSums(!is.na(q3.latencies1))
  num.clean.q3.1[data$skipped] <- NA
  
  
  # sd for block 1
  q1sd1 <- numeric()
  for(i in 1:nrow(q1.latencies1)){
    row <- q1.latencies1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.q1.1[i]
    q1sd1[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.q1.1[i]-1))
  }
  q1sd1[data$skipped] <- NA
  q1sd1[q1sd1==0] <- NA
  
  q2sd1 <- numeric()
  for(i in 1:nrow(q2.latencies1)){
    row <- q2.latencies1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.q2.1[i]
    q2sd1[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.q2.1[i]-1))
  }
  q2sd1[data$skipped] <- NA
  q2sd1[q2sd1==0] <- NA
  
  q3sd1 <- numeric()
  for(i in 1:nrow(q3.latencies1)){
    row <- q3.latencies1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.q3.1[i]
    q3sd1[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.q3.1[i]-1))
  }
  q3sd1[data$skipped] <- NA
  q3sd1[q3sd1==0] <- NA
  
  
  # block 2
  q1.latencies2 <- b2[,q1]
  q2.latencies2 <- b2[,q2]
  q3.latencies2 <- b2[,q3]
  
  
  q1means2 <- rowMeans(q1.latencies2, na.rm=T)
  q2means2 <- rowMeans(q2.latencies2, na.rm=T)
  q3means2 <- rowMeans(q3.latencies2, na.rm=T)
  
  q1means2[is.nan(q1means2)] <- NA
  q2means2[is.nan(q2means2)] <- NA
  q3means2[is.nan(q3means2)] <- NA
  
  
  
  # get number of clean trials by row for variance calculation
  num.clean.q1.2 <- rowSums(!is.na(q1.latencies2))
  num.clean.q1.2[data$skipped] <- NA
  
  num.clean.q2.2 <- rowSums(!is.na(q2.latencies2))
  num.clean.q2.2[data$skipped] <- NA
  
  num.clean.q3.2 <- rowSums(!is.na(q3.latencies2))
  num.clean.q3.2[data$skipped] <- NA
  
  
  # sd for block 2
  q1sd2 <- numeric()
  for(i in 1:nrow(q1.latencies2)){
    row <- q1.latencies2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.q1.2[i]
    q1sd2[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.q1.2[i]-1))
  }
  q1sd2[data$skipped] <- NA
  q1sd2[q1sd2==0] <- NA
  
  q2sd2 <- numeric()
  for(i in 1:nrow(q2.latencies2)){
    row <- q2.latencies2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.q2.2[i]
    q2sd2[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.q2.2[i]-1))
  }
  q2sd2[data$skipped] <- NA
  q2sd2[q2sd2==0] <- NA
  
  q3sd2 <- numeric()
  for(i in 1:nrow(q3.latencies2)){
    row <- q3.latencies2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.q3.2[i]
    q3sd2[i] <- sqrt(sum((row - avg)^2, na.rm=T) / (num.clean.q3.2[i]-1))
  }
  q3sd2[data$skipped] <- NA
  q3sd2[q3sd2==0] <- NA
  
  
  # diff scores
  
  diff.q1 <- q1means2 - q1means1 
  pool.sd.q1<- sqrt((q1sd1^2 * (num.clean.q1.1-1) + q1sd2^2 * (num.clean.q1.2-1)) / (num.clean.q1.1-1 + num.clean.q1.2-1))
  D1 <- diff.q1 / pool.sd.q1
  
  diff.q2 <- q2means2 - q2means1 
  pool.sd.q2<- sqrt((q2sd1^2 * (num.clean.q2.1-1) + q2sd2^2 * (num.clean.q2.2-1)) / (num.clean.q2.1-1 + num.clean.q2.2-1))
  D2 <- diff.q2 / pool.sd.q2
  
  diff.q3 <- q3means2 - q3means1 
  pool.sd.q3<- sqrt((q3sd1^2 * (num.clean.q3.1-1) + q3sd2^2 * (num.clean.q3.2-1)) / (num.clean.q3.1-1 + num.clean.q3.2-1))
  D3 <- diff.q3 / pool.sd.q3
  
  
  library(lavaan)
  library(semTools)
  
  dat3 <- data.frame(D1, D2, D3)
  mod.cfa <- 'iat1 =~ D1 + D2 + D3'
  fit.cfa <- cfa(mod.cfa, data=dat3)
  #summary(fit.cfa, standardized=TRUE, fit.measures=TRUE)
  rel <- semTools::reliability(fit.cfa)
  
  return(list(reliability=rel, model=mod.cfa, model.fit=fit.cfa, D1=D1, D2=D2, D3=D3))
}
