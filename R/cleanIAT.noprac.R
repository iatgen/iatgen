#' Data analysis function: Cleans the IAT without Practice Blocks
#' @description A variant of \code{cleanIAT()}. Inputs and outputs are identical, except that this function accepts only two blocks. This can be used when practice blocks are omitted from the IAT.
#' @seealso See the help for \code{cleanIAT()}.
#' @examples \dontrun{
#' ### Collapse  IAT critical blocks  down ####
#' clean <- cleanIAT.noprac(dat$compatible.crit, dat$incompatible.crit)
#'}
cleanIAT.noprac <- function(crit1, crit2, timeout.drop=TRUE, timeout.ms=10000, fasttrial.drop=FALSE, fasttrial.ms=400, fastprt.drop=TRUE, fastprt.percent=.10, fastprt.ms=300, error.penalty=FALSE, error.penalty.ms=600, inclusive.sd=TRUE) {

  if (is.null(crit1)){stop("One of your input variables does not exist. Please check your data / variable names and try again.")}
  if (is.null(crit2)){stop("One of your input variables does not exist. Please check your data / variable names and try again.")}

  if (all(is.na(crit1))){stop("One of your input variables is empty")}
  if (all(is.na(crit2))){stop("One of your input variables is empty")}


  ## Declare local function to add leading zeros. Needed if the first two characters contain C or X
  add.leading.zeros <- function(temp) {
    if (stringr::str_count(stringr::str_sub(temp,1,2),"C") == 1 | stringr::str_count(stringr::str_sub(temp,1,2),"X") == 1){
      temp <- paste("0", temp, sep="")
    }
    return(temp)
  }

  #Check for people who skipped IAT or who have nonvalid data
  #Are there 3 "END" characters at end of string? If not, did not complete IAT; mark skipped
  p.crit1 <- substring(crit1, (stringr::str_length(crit1)-2), stringr::str_length(crit1)) != "END"
  p.crit2 <- substring(crit2, (stringr::str_length(crit2)-2), stringr::str_length(crit2)) != "END"
  crit1[p.crit1] <- ""
  crit2[p.crit2] <- ""
  #mark people who skipped IAT as such
  skipped.crit1 <- crit1 == ""
  skipped.crit2 <- crit2 == ""
  #check integrity of people who completed IAT
  p.crit1 <- (p.crit1 & !skipped.crit1)
  p.crit2 <- (p.crit2 & !skipped.crit2)

  check.me <- function(temp){
    temp <- stringr::str_replace_all(temp, "END", "")
    temp <- stringr::str_replace_all(temp, ",", "")
    temp <- stringr::str_replace_all(temp, "C", "")
    temp <- stringr::str_replace_all(temp, "X", "")
    temp <- stringr::str_replace_all(temp, "0", "")
    temp <- stringr::str_replace_all(temp, "1", "")
    temp <- stringr::str_replace_all(temp, "2", "")
    temp <- stringr::str_replace_all(temp, "3", "")
    temp <- stringr::str_replace_all(temp, "4", "")
    temp <- stringr::str_replace_all(temp, "5", "")
    temp <- stringr::str_replace_all(temp, "6", "")
    temp <- stringr::str_replace_all(temp, "7", "")
    temp <- stringr::str_replace_all(temp, "8", "")
    temp <- stringr::str_replace_all(temp, "9", "")
    return(!temp=="")
  }
  p.crit1 <- as.logical(p.crit1 + check.me(crit1))
  p.crit2 <- as.logical(p.crit2 + check.me(crit2))

  p.prt <- as.logical(p.crit1 + p.crit2)
  rm(p.crit1);rm(p.crit2)
  index.prt <- 1:length(p.prt)
  flag<-index.prt[p.prt==TRUE]; rm(index.prt); rm(p.prt)
  crit1[flag] <- ""
  crit2[flag] <- ""
  if(length(flag) > 0){
    for(i in 1:length(flag)){
      warning(paste("Participant ",flag[i],"'s web browser encountered an error during the survey. Their IAT data are not usable and not included in analysis.", sep=""))
    }
  }
  #update skip counts to reflect number of validly completed IATs
  skipped.crit1 <- crit1 == ""
  skipped.crit2 <- crit2 == ""


  ## BUILD data frames
  raw.crit1 <- data.frame()
  raw.crit2 <- data.frame()
  num.raw.trials.crit1 <- numeric() # make a vector. Everyone has same number NOW but will overwrite later
  num.raw.trials.crit2 <- numeric() # make a vector. Everyone has same number NOW but will overwrite later

  ## POPULATE data frames. Make all NA if task skipped.

  #crit1
  for(i in 1:length(crit1)){
    source <- toString(crit1[i])
    num.raw.trials.crit1[i] <- stringr::str_count(source,",")
    if (skipped.crit1[i]) {raw.crit1[i,] <- NA} else {
      for(j in 1:num.raw.trials.crit1[i]) {
        comma.location <- stringr::str_locate(source,",")[1]
        raw.crit1[i,j] <- stringr::str_sub(source, 1, comma.location - 1)
        source <- stringr::str_sub(source, comma.location+1, stringr::str_length(source))
      }
    }
  }

  #crit2
  for(i in 1:length(crit2)){
    source <- toString(crit2[i])
    num.raw.trials.crit2[i] <- stringr::str_count(source,",")
    if (skipped.crit2[i]) {raw.crit2[i,] <- NA} else {
      for(j in 1:num.raw.trials.crit2[i]) {
        comma.location <- stringr::str_locate(source,",")[1]
        raw.crit2[i,j] <- stringr::str_sub(source, 1, comma.location - 1)
        source <- stringr::str_sub(source, comma.location+1, stringr::str_length(source))
      }
    }
  }

  ## ADD leading zeros on non-empty cells. Do for non-empty cells only.

  #crit1
  for (i in 1:nrow(raw.crit1)){
    for (j in 1:ncol(raw.crit1)){
      if (!is.na(raw.crit1[i,j])) {raw.crit1[i,j] <- add.leading.zeros(raw.crit1[i,j])}
    }
  }

  #crit2
  for (i in 1:nrow(raw.crit2)){
    for (j in 1:ncol(raw.crit2)){
      if (!is.na(raw.crit2[i,j])) {raw.crit2[i,j] <- add.leading.zeros(raw.crit2[i,j])}
    }
  }


  temp.crit1 <- median(num.raw.trials.crit1[num.raw.trials.crit1 != 0])
  temp.crit2 <- median(num.raw.trials.crit2[num.raw.trials.crit2 != 0])
  raw.crit1 <- raw.crit1[,1:temp.crit1]
  raw.crit2 <- raw.crit2[,1:temp.crit2]
  num.raw.trials.crit1[num.raw.trials.crit1 > temp.crit1] <- temp.crit1
  num.raw.trials.crit2[num.raw.trials.crit2 > temp.crit2] <- temp.crit2
  rm(temp.crit1); rm(temp.crit2)

  ## SAVE stimuli numbers as a data frame. NA handled naturally.

  #crit1
  raw.stim.number.crit1 <- raw.crit1
  for (i in 1:nrow(raw.stim.number.crit1)){
    for (j in 1:ncol(raw.stim.number.crit1)){
      raw.stim.number.crit1[i,j] <- as.numeric(stringr::str_sub(raw.stim.number.crit1[i,j], 1, 2))
    }
  } #not returning numeric -- this fixes it
  for (j in 1:ncol(raw.stim.number.crit1)){
    raw.stim.number.crit1[,j] <- as.numeric(raw.stim.number.crit1[,j])
  }

  #crit2
  raw.stim.number.crit2 <- raw.crit2
  for (i in 1:nrow(raw.stim.number.crit2)){
    for (j in 1:ncol(raw.stim.number.crit2)){
      raw.stim.number.crit2[i,j] <- as.numeric(stringr::str_sub(raw.stim.number.crit2[i,j], 1, 2))
    }
  } #not returning numeric -- this fixes it
  for (j in 1:ncol(raw.stim.number.crit2)){
    raw.stim.number.crit2[,j] <- as.numeric(raw.stim.number.crit2[,j])
  }

  ## SAVE trial status (correct v incorrect) as a data frame. NA handled naturally.

  #crit1
  raw.correct.crit1 <- raw.crit1
  for (i in 1:nrow(raw.correct.crit1)){
    for (j in 1:ncol(raw.correct.crit1)){
      raw.correct.crit1[i,j] <- stringr::str_sub(raw.correct.crit1[i,j], 3, 3)
    }
  }

  #crit2
  raw.correct.crit2 <- raw.crit2
  for (i in 1:nrow(raw.correct.crit2)){
    for (j in 1:ncol(raw.correct.crit2)){
      raw.correct.crit2[i,j] <- stringr::str_sub(raw.correct.crit2[i,j], 3, 3)
    }
  }

  ## SAVE latencies as a data frame, convert to numeric. NA handled naturally


  #crit1
  raw.latencies.crit1 <- raw.crit1
  for (i in 1:nrow(raw.latencies.crit1)){
    for (j in 1:ncol(raw.latencies.crit1)){
      end <- nchar(raw.latencies.crit1[i,j])
      raw.latencies.crit1[i,j] <- stringr::str_sub(raw.latencies.crit1[i,j], 4, end)
    }
  }
  for (j in 1:ncol(raw.latencies.crit1)){
    raw.latencies.crit1[,j] <- as.numeric(raw.latencies.crit1[,j])
  }

  #crit2
  raw.latencies.crit2 <- raw.crit2
  for (i in 1:nrow(raw.latencies.crit2)){
    for (j in 1:ncol(raw.latencies.crit2)){
      end <- nchar(raw.latencies.crit2[i,j])
      raw.latencies.crit2[i,j] <- stringr::str_sub(raw.latencies.crit2[i,j], 4, end)
    }
  }
  for (j in 1:ncol(raw.latencies.crit2)){
    raw.latencies.crit2[,j] <- as.numeric(raw.latencies.crit2[,j])
  }


  ## CREATE containers for clean versions
  clean.latencies.crit1 <-raw.latencies.crit1
  clean.correct.crit1 <- raw.correct.crit1
  clean.stim.number.crit1 <- raw.stim.number.crit1

  clean.latencies.crit2 <-raw.latencies.crit2
  clean.correct.crit2 <- raw.correct.crit2
  clean.stim.number.crit2 <- raw.stim.number.crit2


  ## DROP trials that are too long
  num.timeout.removed.crit1 <- 0 #create a count of timeout responses removed
  num.timeout.removed.crit2 <- 0 #create a count of timeout responses removed

  if (timeout.drop==TRUE){
    # if enabled, removes trials over 10k ms
    # NA handling: only performs comparison logic if not NA

    #crit1
    for (i in 1:nrow(clean.latencies.crit1)){
      for (j in 1:ncol(clean.latencies.crit1)){
        if (!is.na(clean.latencies.crit1[i,j])){
          if(clean.latencies.crit1[i,j] > timeout.ms) {
            clean.latencies.crit1[i,j] <- NA
            clean.correct.crit1[i,j] <- NA
            clean.stim.number.crit1[i,j] <- NA
            num.timeout.removed.crit1 <- num.timeout.removed.crit1 + 1 #counter of total removals for sample for rate analysis
          }
        }
      }
    }

    #crit2
    for (i in 1:nrow(clean.latencies.crit2)){
      for (j in 1:ncol(clean.latencies.crit2)){
        if (!is.na(clean.latencies.crit2[i,j])){
          if(clean.latencies.crit2[i,j] > timeout.ms) {
            clean.latencies.crit2[i,j] <- NA
            clean.correct.crit2[i,j] <- NA
            clean.stim.number.crit2[i,j] <- NA
            num.timeout.removed.crit2 <- num.timeout.removed.crit2 + 1 #counter of total removals for sample for rate analysis
          }
        }
      }
    }
  }
  #SUM total for final reporting
  num.timeout.removed <- sum(c(num.timeout.removed.crit1, num.timeout.removed.crit2), na.rm=T)


  ## DROP trials that are too short (for some algorithms)
  num.fasttrial.removed.crit1 <- 0
  num.fasttrial.removed.crit2 <- 0

  if (fasttrial.drop == T){
    # removes trials under a given threshold
    # NA handling: only performs comparison logic if not NA

    #crit1
    for (i in 1:nrow(clean.latencies.crit1)){
      for (j in 1:ncol(clean.latencies.crit1)){
        if (!is.na(clean.latencies.crit1[i,j])){
          if(clean.latencies.crit1[i,j] < fasttrial.ms) {
            clean.latencies.crit1[i,j] <- NA
            clean.correct.crit1[i,j] <- NA
            clean.stim.number.crit1[i,j] <- NA
            num.fasttrial.removed.crit1 <- num.fasttrial.removed.crit1 + 1 #counter of total removals for sample for rate analysis
          }
        }
      }
    }

    #crit2
    for (i in 1:nrow(clean.latencies.crit2)){
      for (j in 1:ncol(clean.latencies.crit2)){
        if (!is.na(clean.latencies.crit2[i,j])){
          if(clean.latencies.crit2[i,j] < fasttrial.ms) {
            clean.latencies.crit2[i,j] <- NA
            clean.correct.crit2[i,j] <- NA
            clean.stim.number.crit2[i,j] <- NA
            num.fasttrial.removed.crit2 <- num.fasttrial.removed.crit2 + 1 #counter of total removals for sample for rate analysis
          }
        }
      }
    }

  }
  #SUM total for final reporting
  num.fasttrial.removed <- sum(c(num.fasttrial.removed.crit1, num.fasttrial.removed.crit2), na.rm=T)


  ## DROP PARTICIPANT IF OVERLY FAST

  # generate large combo dataset
  fastprt.trials <- cbind(clean.latencies.crit1, clean.latencies.crit2)
  raw.latencies.combo <- cbind(raw.latencies.crit1, raw.latencies.crit2) #fast trials may already be dropped. This compares against raw latencies to ensure we don't keep someone because we have already dropped their fast trials.

  #also need a num.raw.trials for this dataset
  num.raw.trials <- num.raw.trials.crit1 + num.raw.trials.crit2

  #create a counter variable for overly fast trials
  fastprt.trials[is.na(fastprt.trials)] <- 0 # convert all NAs (e.g., for skips, dropped trials) to zeros // else comparison fails
  raw.latencies.combo[is.na(raw.latencies.combo)] <- 0 # convert all NAs (e.g., for skips) to zeros // else comparison fails
  for (i in 1:nrow(fastprt.trials)){
    for (j in 1:ncol(fastprt.trials)){
      if(raw.latencies.combo[i,j] < fastprt.ms){fastprt.trials[i,j] = 1} else {fastprt.trials[i,j] = 0}
    }
  }
  rm(raw.latencies.combo)


  if (fastprt.drop == TRUE){
    number.fastprt <- rowSums(fastprt.trials, na.rm=TRUE)
    drop.participant <- (number.fastprt > (num.raw.trials * fastprt.percent))

    #If we are goign to drop, drop from both practice and critical blocks

    clean.latencies.crit1[drop.participant,] <- NA
    clean.correct.crit1[drop.participant,] <- NA
    clean.stim.number.crit1[drop.participant,] <- NA
    clean.latencies.crit2[drop.participant,] <- NA
    clean.correct.crit2[drop.participant,] <- NA
    clean.stim.number.crit2[drop.participant,] <- NA

  } else {
    drop.participant <- rep(FALSE,nrow(fastprt.trials)) # say we're not dropping anyone
  }

  # the above would flag anyone as dropped who simply skipped the task. Adjust.
  skipped <- skipped.crit1 | skipped.crit2  #if any block is skipped, flag as skipped
  fastprt.trials[skipped,] <- NA
  if(fastprt.drop==T) {number.fastprt[skipped] <- NA}
  drop.participant[skipped] <- NA

  # calculate rates of  dropping
  timeout.rate <- num.timeout.removed / sum(num.raw.trials.crit1, num.raw.trials.crit2, na.rm=T)
  fasttrial.rate <- num.fasttrial.removed / sum(num.raw.trials.crit1, num.raw.trials.crit2, na.rm=T)
  fastprt.count <- sum(drop.participant, na.rm=T)
  fastprt.rate <- sum(drop.participant, na.rm=T) / sum(!skipped, na.rm=T)

  ##########

  # Now that all trials / prts that needed dropping are dropped:
  #1) grab SD without error penalty applied
  #2) save a correct latencies files from correct index file
  #3) add error penalty into incorrect trials and add into clean latencies
  #4) calculate means and SDs
  ## this replicates order done in greenwald et al. syntax


  ## grab SD for all clean trials (without error penalty applied) for use in some penalty algorithms

  #same for crit1
  num.clean.trials.crit1 <- clean.latencies.crit1   # skip handling: make NA
  num.clean.trials.crit1[!is.na(num.clean.trials.crit1)] <- 1
  num.clean.trials.crit1 <- rowSums(num.clean.trials.crit1, na.rm=TRUE)
  num.clean.trials.crit1[skipped.crit1] <- NA
  std.nopenalty.crit1 <- numeric()
  for(i in 1:nrow(clean.latencies.crit1)){
    row <- clean.latencies.crit1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.trials.crit1[i]
    std.nopenalty.crit1[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.crit1[i]-1))
  }
  std.nopenalty.crit1[std.nopenalty.crit1==0] <- NA # anyone who has all trials cut will have a zero SD.
  std.nopenalty.crit1[std.nopenalty.crit1==Inf] <- NA
  std.nopenalty.crit1[is.nan(std.nopenalty.crit1)] <- NA

  #crit2
  num.clean.trials.crit2 <- clean.latencies.crit2   # skip handling: make NA
  num.clean.trials.crit2[!is.na(num.clean.trials.crit2)] <- 1
  num.clean.trials.crit2 <- rowSums(num.clean.trials.crit2, na.rm=TRUE)
  num.clean.trials.crit2[skipped.crit2] <- NA
  std.nopenalty.crit2 <- numeric()
  for(i in 1:nrow(clean.latencies.crit2)){
    row <- clean.latencies.crit2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.trials.crit2[i]
    std.nopenalty.crit2[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.crit2[i]-1))
  }
  std.nopenalty.crit2[std.nopenalty.crit2==0] <- NA # anyone who has all trials cut will have a zero SD.
  std.nopenalty.crit2[std.nopenalty.crit2==Inf] <- NA
  std.nopenalty.crit2[is.nan(std.nopenalty.crit2)] <- NA


  ## grab correct latencies, but ignore NA
  #crit1
  clean.correct.latencies.crit1 <-clean.latencies.crit1
  for (i in 1:nrow(clean.correct.latencies.crit1)){
    for (j in 1:ncol(clean.correct.latencies.crit1)){
      if (!is.na(clean.correct.latencies.crit1[i,j])){ #cannot have NA in comparisons
        if(raw.correct.crit1[i,j] == "X") {clean.correct.latencies.crit1[i,j] <- NA}
      }
    }
  }

  #crit2
  clean.correct.latencies.crit2 <-clean.latencies.crit2
  for (i in 1:nrow(clean.correct.latencies.crit2)){
    for (j in 1:ncol(clean.correct.latencies.crit2)){
      if (!is.na(clean.correct.latencies.crit2[i,j])){ #cannot have NA in comparisons
        if(raw.correct.crit2[i,j] == "X") {clean.correct.latencies.crit2[i,j] <- NA}
      }
    }
  }


  ## GRAB SD of pre-penalty correct responses for imposing error the 2-SD error penalty
  #crit1
  num.clean.correct.crit1 <- clean.correct.latencies.crit1   # skip handling: make NA
  num.clean.correct.crit1[!is.na(num.clean.correct.crit1)] <- 1
  num.clean.correct.crit1 <- rowSums(num.clean.correct.crit1, na.rm=TRUE)
  num.clean.correct.crit1[skipped.crit1] <- NA
  clean.std.correct.crit1 <- numeric()
  for(i in 1:nrow(clean.correct.latencies.crit1)){
    row <- clean.correct.latencies.crit1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.correct.crit1[i]
    clean.std.correct.crit1[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.correct.crit1[i]-1))
  }
  clean.std.correct.crit1[clean.std.correct.crit1==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
  clean.std.correct.crit1[clean.std.correct.crit1==Inf] <- NA
  clean.std.correct.crit1[is.nan(clean.std.correct.crit1)] <- NA

  #crit2
  num.clean.correct.crit2 <- clean.correct.latencies.crit2   # skip handling: make NA
  num.clean.correct.crit2[!is.na(num.clean.correct.crit2)] <- 1
  num.clean.correct.crit2 <- rowSums(num.clean.correct.crit2, na.rm=TRUE)
  num.clean.correct.crit2[skipped.crit2] <- NA
  clean.std.correct.crit2 <- numeric()
  for(i in 1:nrow(clean.correct.latencies.crit2)){
    row <- clean.correct.latencies.crit2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.correct.crit2[i]
    clean.std.correct.crit2[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.correct.crit2[i]-1))
  }
  clean.std.correct.crit2[clean.std.correct.crit2==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
  clean.std.correct.crit2[clean.std.correct.crit2==Inf] <- NA
  clean.std.correct.crit2[is.nan(clean.std.correct.crit2)] <- NA

  ## grab means for correct, cleaned values and add error penalty
  # NA handling: only performs calculations for non-missing values
  # NOTE: replacing values with means and penalties may accidentally resurrect dropped trials. logic now only replaces for nonmissing values.

  #crit1
  clean.correct.means.crit1 <- rowMeans(clean.correct.latencies.crit1, na.rm=TRUE)
  clean.correct.means.crit1[is.nan(clean.correct.means.crit1)] <- NA
  for (i in 1:nrow(clean.latencies.crit1)){
    for (j in 1:ncol(clean.latencies.crit1)){
      if(!is.na(raw.correct.crit1[i,j])){
        if(error.penalty==TRUE && is.numeric(error.penalty.ms)){
          if(raw.correct.crit1[i,j] == "X" && !is.na(clean.latencies.crit1[i,j])) {clean.latencies.crit1[i,j] = clean.correct.means.crit1[i] + error.penalty.ms}
        } else if (error.penalty==TRUE && error.penalty.ms=="2SD"){
          if(raw.correct.crit1[i,j] == "X" && !is.na(clean.latencies.crit1[i,j])) {clean.latencies.crit1[i,j] = clean.correct.means.crit1[i] + 2*clean.std.correct.crit1[i]}
        } else if (error.penalty==FALSE){
          if(raw.correct.crit1[i,j] == "X" && !is.na(clean.latencies.crit1[i,j])) {clean.latencies.crit1[i,j] = clean.latencies.crit1[i,j]}
        }
        if(raw.correct.crit1[i,j] == "C") {clean.latencies.crit1[i,j] = clean.correct.latencies.crit1[i,j]}
      }
      if(is.na(raw.correct.crit1[i,j])){clean.latencies.crit1[i,j] <- NA} #should already be NA because they were dropped or missing but just to be safe
    }
  }

  #crit2
  clean.correct.means.crit2 <- rowMeans(clean.correct.latencies.crit2, na.rm=TRUE)
  clean.correct.means.crit2[is.nan(clean.correct.means.crit2)] <- NA
  for (i in 1:nrow(clean.latencies.crit2)){
    for (j in 1:ncol(clean.latencies.crit2)){
      if(!is.na(raw.correct.crit2[i,j])){
        if(error.penalty==TRUE && is.numeric(error.penalty.ms)){
          if(raw.correct.crit2[i,j] == "X" && !is.na(clean.latencies.crit2[i,j])) {clean.latencies.crit2[i,j] = clean.correct.means.crit2[i] + error.penalty.ms}
        } else if (error.penalty==TRUE && error.penalty.ms=="2SD"){
          if(raw.correct.crit2[i,j] == "X" && !is.na(clean.latencies.crit2[i,j])) {clean.latencies.crit2[i,j] = clean.correct.means.crit2[i] + 2*clean.std.correct.crit2[i]}
        } else if (error.penalty==FALSE){
          if(raw.correct.crit2[i,j] == "X" && !is.na(clean.latencies.crit2[i,j])) {clean.latencies.crit2[i,j] = clean.latencies.crit2[i,j]}
        }
        if(raw.correct.crit2[i,j] == "C") {clean.latencies.crit2[i,j] = clean.correct.latencies.crit2[i,j]}
      }
      if(is.na(raw.correct.crit2[i,j])){clean.latencies.crit2[i,j] <- NA} #should already be NA because they were dropped or missing but just to be safe
    }
  }



  ## saves clean blocks means
  clean.means.crit1 <- rowMeans(clean.latencies.crit1, na.rm=TRUE)
  clean.means.crit1[is.nan(clean.means.crit1)] <- NA
  clean.means.crit2 <- rowMeans(clean.latencies.crit2, na.rm=TRUE)
  clean.means.crit2[is.nan(clean.means.crit2)] <- NA

  ## generate inclusive SD for D score; use apply for rowSDs. Get a grand SD for psychometrics testing
  inclusive.sd.crit <-  apply(cbind(clean.latencies.crit1, clean.latencies.crit2), 1, sd, na.rm=T)
  grand.sd <- apply(cbind(clean.latencies.crit1, clean.latencies.crit2), 1, sd, na.rm=T)

  ## final total for calculations
  num.clean.trials <- num.clean.trials.crit1 + num.clean.trials.crit2

  # save error rate on non-eliminated trials
  error.rate <- cbind(clean.correct.crit1, clean.correct.crit2)
  error.rate[error.rate=="C"] <- 0
  error.rate[error.rate=="X"] <- 1
  for (j in 1:ncol(error.rate)){
    error.rate[,j] <- as.numeric(error.rate[,j])
  }
  error.num <- rowSums(error.rate, na.rm=T)
  error.num[skipped] <- NA # drop skips!
  error.rate <- error.num/ num.clean.trials
  error.rate[error.num == 0] <- 0   # make zero for people with no errors
  error.num[drop.participant==TRUE] <- NA
  error.rate[drop.participant==TRUE] <- NA
  error.rate[error.rate == Inf] <- NA

  #rename for use as prt variable
  error.num.prt <- error.num
  error.rate.prt <- error.rate

  #calcualte for whole sample
  error.rate <- sum(error.num, na.rm=T) / sum(num.clean.trials, na.rm=T)


  # save error rate on non-eliminated trials - crit1
  error.rate.crit1 <- cbind(clean.correct.crit1)
  error.rate.crit1[error.rate.crit1=="C"] <- 0
  error.rate.crit1[error.rate.crit1=="X"] <- 1
  for (j in 1:ncol(error.rate.crit1)){
    error.rate.crit1[,j] <- as.numeric(error.rate.crit1[,j])
  }
  error.num.crit1 <- rowSums(error.rate.crit1, na.rm=T)
  error.num.crit1[skipped] <- NA # drop skips!
  error.rate.crit1 <- error.num.crit1/ num.clean.trials.crit1
  error.rate.crit1[error.num.crit1 == 0] <- 0   # make zero for people with no errors
  error.num.crit1[drop.participant==TRUE] <- NA
  error.rate.crit1[drop.participant==TRUE] <- NA
  error.rate.crit1[error.rate.crit1 == Inf] <- NA
  error.rate.crit1 <- sum(error.num.crit1, na.rm=T) / sum(num.clean.trials.crit1, na.rm=T)

  # save error rate on non-eliminated trials - crit2
  error.rate.crit2 <- cbind(clean.correct.crit2)
  error.rate.crit2[error.rate.crit2=="C"] <- 0
  error.rate.crit2[error.rate.crit2=="X"] <- 1
  for (j in 1:ncol(error.rate.crit2)){
    error.rate.crit2[,j] <- as.numeric(error.rate.crit2[,j])
  }
  error.num.crit2 <- rowSums(error.rate.crit2, na.rm=T)
  error.num.crit2[skipped] <- NA # drop skips!
  error.rate.crit2 <- error.num.crit2/ num.clean.trials.crit2
  error.rate.crit2[error.num.crit2 == 0] <- 0   # make zero for people with no errors
  error.num.crit2[drop.participant==TRUE] <- NA
  error.rate.crit2[drop.participant==TRUE] <- NA
  error.rate.crit2[error.rate.crit2 == Inf] <- NA
  error.rate.crit2 <- sum(error.num.crit2, na.rm=T) / sum(num.clean.trials.crit2, na.rm=T)

  ## Dscore

  diff.crit <- clean.means.crit2-clean.means.crit1

  D <- diff.crit / inclusive.sd.crit


  return(list(
    skipped=skipped,
    raw.latencies.crit1=raw.latencies.crit1,
    raw.latencies.crit2=raw.latencies.crit2,
    raw.stim.number.crit1=raw.stim.number.crit1,
    raw.stim.number.crit2=raw.stim.number.crit2,
    raw.correct.crit1=raw.correct.crit1,
    raw.correct.crit2=raw.correct.crit2,
    timeout.drop=timeout.drop,
    timeout.ms=timeout.ms,
    num.timeout.removed=num.timeout.removed,
    timeout.rate=timeout.rate,
    num.timeout.removed.crit1=num.timeout.removed.crit1,
    num.timeout.removed.crit2=num.timeout.removed.crit2,
    fasttrial.drop=fasttrial.drop,
    fasttrial.ms=fasttrial.ms,
    num.fasttrial.removed=num.fasttrial.removed,
    fasttrial.rate=fasttrial.rate,
    num.fasttrial.removed.crit1=num.fasttrial.removed.crit1,
    num.fasttrial.removed.crit2=num.fasttrial.removed.crit2,
    fastprt.drop=fastprt.drop,
    fastprt.ms=fastprt.ms,
    fastprt.percent=fastprt.percent,
    drop.participant=drop.participant,
    fastprt.count=fastprt.count,
    fastprt.rate=fastprt.rate,
    error.penalty=error.penalty,
    error.num.prt=error.num.prt,
    error.rate.prt=error.rate.prt,
    error.rate=error.rate,
    error.rate.crit1=error.rate.crit1,
    error.rate.crit2=error.rate.crit2,
    clean.latencies.crit1=clean.latencies.crit1,
    clean.latencies.crit2=clean.latencies.crit2,
    clean.stim.number.crit1=clean.stim.number.crit1,
    clean.stim.number.crit2=clean.stim.number.crit2,
    clean.correct.crit1=clean.correct.crit1,
    clean.correct.crit2=clean.correct.crit2,
    clean.means.crit1=clean.means.crit1,
    clean.means.crit2=clean.means.crit2,
    diff.crit=diff.crit,
    inclusive.sd.crit=inclusive.sd.crit,
    grand.sd=grand.sd,
    D=D
  ))
}
