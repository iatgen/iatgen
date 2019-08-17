requireNamespace("stringr")

#' Data analysis function: Processes and cleans raw IAT data
#' @description Prior to running, please see \code{combineIATfourblocks()}. This function processes, cleans, and scores the combined IAT data. In addition, it returns diagnostics (see examples, below). By default, the function implements the D-score algorithm (Greenwald et al., 2003, p 214, center column). Because it assumes users were forced to correct errors, no error penalty is imposed (unless the user requests it; see below). The function can be easily configured to do other scoring procedures as well. The function accepts as an input four vectors of IAT responses (see \code{prac1}, \code{crit1}, \code{prac2}, and \code{crit2}, below). It returns a list containing a variety of IAT variables, including matrices of clean latencies and other information (see below). The most important is \code{clean$D}, which is the final D scores for the analysis. Users can also extract clean block means for each participant using \code{clean$clean.means.prac1}, \code{clean$clean.means.crit1}, \code{clean$clean.means.prac2}, and \code{clean$clean.means.crit2}. Users can extract matrices of clean latencies using \code{clean$clean.latencies.prac1}, \code{clean$clean.latencies.crit1}, etc. Raw latencies can be requested with \code{clean$raw.latencies.prac1}, etc. Users can request to know whether a trial was correct with \code{clean$clean.correct.prac1}, etc. and precisely which stimulus was used on a given trial with \code{clean$clean.stim.number.prac1}, etc. (Stimuli are numbered based on their order entered within each category and following the sequence "positive, negative, tgtA, tgtB". For example, stimulus 1 is the first positive stimulus). See below for more information on what is returned from this function. The data cleaning function adheres to Greenwald et al. (2003; see also Lane et al., 2005, p. 92 for a simplified table of data cleaning steps). There are four main data cleaning options. First, long responses are usually dealt with by setting \code{timeout.drop=TRUE} (enabled by default), which drops individual trials over a given threshold (\code{timeout.ms}, which is 10000 ms by default). Next, overly short responses (i.e., button mashing) are dealt with by setting \code{fastprt.drop=TRUE} (enabled by default), which drops participants who have too many fast responses (more than a \code{fastprt.percent} proportion [default = .10] of responses faster than \code{fastprt.ms} [default = 300 ms]). Alternatively, one can remove individual fast trials by setting \code{fasttrial.drop=TRUE} (disabled by default), which uses a default threshold of \code{fasttrial.ms=400} ms. (This is seldom used but enables users to use alternative scoring methods [e.g., Greenwald et al., 2003, p 214, right column]). Finally, an error penalty is imposed on incorrect responses in some variants. If the IAT forces participants to correct errors, then no error penalty should be imposed (\code{error.penalty=FALSE}, the default setting). However, if participants are not forced to correct errors, one is added. Most common is a 600 ms penalty above the clean block mean (Greenwald et al., 2003), which is done by setting \code{error.penalty.ms=600}, sometimes known as the D600 scoring procedure. Greenwald et al. (2003) also suggested one could use two standard deviations instead of 600 ms, which is done by setting \code{error.penalty.ms="2SD"}. Finally, the function ensures that the data are not corrupted (i.e., JavaScript malfunction on participant's computer when completing the survey) by requiring that only appropriate characters (numbers, commas, "C", "X", and "END) are in the raw data.
#' @param prac1 A vector of one kind of practice responses (e.g., compatible practice), one per participant.
#' @param crit1 A vector of that same kind of critical responses (e.g., compatible critical), one per participant.
#' @param prac2 A vector of the other kind of practice responses (e.g., incompatible practice), one per participant.
#' @param crit2 A vector of that same kind of critical responses (e.g., incompatible critical), one per participant.
#' @param timeout.drop (Required, set \code{TRUE} by default). Tells the procedure to drop trials over a certain duration; recommended by Greenwald et al. (2003).
#' @param timeout.ms (Required if \code{timeout.drop=TRUE}; set to 10000 by default). Following the Greenwald et al. (2003), individual trials over 10000 ms are dropped (scored as missing). Ignored if \code{timeout.drop=FALSE}.
#' @param fastprt.drop (Required, set \code{TRUE} by default). If enabled, follows Greenwald et al. (2003) in which participants who have more than 10 percent of responses (\code{fastprt.percent = .10}) faster than 300 ms (\code{fastprt.ms=300}) are dropped entirely.
#' @param fastprt.percent (Required if \code{fastprt.drop=TRUE}; set to .10 by default). Set the proportion threshold for \code{fastprt.drop}, above. Ignored if \code{fastprt.drop=FALSE}.
#' @param fastprt.ms (Required if \code{fastprt.drop=TRUE}; set to 300 ms by default). Sets the time threshold for for \code{fastprt.drop}, above. Ignored if \code{fastprt.drop=FALSE}.
#' @param fasttrial.drop (Required, set \code{FALSE} by default). Tells the procedure to drop trials under a certain duration. Not recommended but was validated by Greenwald et al. (2003) as an alternative to dropping fast participants.
#' @param fasttrial.ms (Required if \code{fasttrial.drop=TRUE}; set to 400 ms by default). The threshold for \code{fastprt.drop}, above. Ignored if \code{fastprt.drop=FALSE}.
#' @param error.penalty (Required, set \code{FALSE} by default). Logical value stating whether an error penalty is added. This should be disabled if forced error correction was used in the IAT and enabled otherwise (Greenwald et al., 2003).
#' @param error.penalty.ms (Required if \code{error.penalty=TRUE}; set to \code{error.penalty.ms=600} by default). Following the D600 procedure, IAT errors are scored as the correct-trial block mean plus an error penalty of 600 ms. Can be manually set to any desired value. One can also use the 2SD penalty [Greenwald et al., 2003, p 214, right column] by setting  \code{error.penalty.ms="2SD"}. Ignored if \code{error.penalty=FALSE}.
#' @return Returns a list containing several important elements.
#' \code{skipped} is a vector indicating whether the participant completed the IAT or skipped it. They are dropped from analysis if the IAT was skipped.
#' \code{raw.latencies.prac1} is a matrix of the raw latencies in the first practice block prior to any data cleaning.
#' \code{raw.latencies.crit1} is a matrix of the raw latencies in the first critical block prior to any data cleaning.
#' \code{raw.latencies.prac2} is a matrix of the raw latencies in the second practice block prior to any data cleaning.
#' \code{raw.latencies.crit2} is a matrix of the raw latencies in the second critical block prior to any data cleaning.
#' \code{raw.stim.number.prac1} is a matrix of the raw stimuli ID numbers in the first practice block prior to any data cleaning.
#' \code{raw.stim.number.crit1} is a matrix of the raw stimuli ID numbers in the first critical block prior to any data cleaning.
#' \code{raw.stim.number.prac2} is a matrix of the raw stimuli ID numbers in the second practice block prior to any data cleaning.
#' \code{raw.stim.number.crit2} is a matrix of the raw stimuli ID numbers in the second critical block prior to any data cleaning.
#' \code{raw.correct.prac1} is a matrix stating whether each trial was correct (logical) in the first practice block prior to cleaning.
#' \code{raw.correct.crit1} is a matrix stating whether each trial was correct (logical) in the first critical block prior to cleaning.
#' \code{raw.correct.prac2} is a matrix stating whether each trial was correct (logical) in the second practice block prior to cleaning.
#' \code{raw.correct.crit2} is a matrix stating whether each trial was correct (logical) in the second critical block prior to cleaning.
#' \code{timeout.drop} is the logical value stating whether this feature was enabled in the function call (see above).
#' \code{timeout.ms} is the timeout threshold specified in the function call (see above), used if timeout.drop is enabled.
#' \code{num.timeout.removed} is the grand total number of trials removed because they exceeded the timeout threshold in timeout.ms.
#' \code{timeout.rate} is a vector indicating the proportion of responses per participant that were scored as missing due to timeouts.
#' \code{num.timeout.removed.prac1} is the number of trials removed in the first practice block because they exceeded the timeout threshold in timeout.ms.
#' \code{num.timeout.removed.crit1} is the number of trials removed in the first critical block because they exceeded the timeout threshold in timeout.ms.
#' \code{num.timeout.removed.prac2} is the number of trials removed in the second practice block because they exceeded the timeout threshold in timeout.ms.
#' \code{num.timeout.removed.crit2} is the number of trials removed in the second critical block because they exceeded the timeout threshold in timeout.ms.
#' \code{fasttrial.drop} is the logical value stating whether this feature was enabled in the function call (see above).
#' \code{fasttrial.ms} is the time threshold specified in the function call (see above), used if fasttrial.drop is enabled.
#' \code{num.fasttrial.removed} is the grand total number of trials removed because they exceeded the fasttrial threshold in fasttrial.ms.
#' \code{fasttrial.rate} is a vector indicating the percentage of responses per participant that were scored as missing due to rapid speeds.
#' \code{num.fasttrial.removed.prac1} is the number of trials removed in the first practice block because they exceeded the fasttrial threshold in fasttrial.ms.
#' \code{num.fasttrial.removed.crit1} is the number of trials removed in the first critical block because they exceeded the fasttrial threshold in fasttrial.ms.
#' \code{num.fasttrial.removed.prac2} is the number of trials removed in the second practice block because they exceeded the fasttrial threshold in fasttrial.ms.
#' \code{num.fasttrial.removed.crit2} is the number of trials removed in the second critical block because they exceeded the fasttrial threshold in fasttrial.ms.
#' \code{fastprt.drop} is the logical value as specified by the function call (see above).
#' \code{fastprt.ms} is the threshold for as  specified in the function call (see above), used if fastprt.drop is enabled.
#' \code{fastprt.percent} is the proportion of trials specified in the function call (see above).
#' \code{drop.participant} is a logical vector indicating whether the participant's responses have been dropped due to excessive fast responses (if fastprt.drop is enabled).
#' \code{fastprt.count} is the number of participants dropped for excessive fast responding (if fastprt.drop is enabled).
#' \code{fastprt.rate} is the proportion of participants dropped for excessive fast responding (if fastprt.drop is enabled).
#' \code{error.penalty} is a logical value stating whether an error penalty is enabled.
#' \code{error.num.prt} is a vector of the number of erroneous trials per participant (after data cleaning is complete).
#' \code{error.rate.prt} is a vector of the proportion of erroneous trials per participant (after data cleaning is complete).
#' \code{error.rate} is the proportion of the entire set of clean trials which are erroneous trials.
#' \code{error.rate.prac1} is the proportion of the prac1 block set of clean trials which are erroneous trials.
#' \code{error.rate.crit1} is the proportion of the crit1 block set of clean trials which are erroneous trials.
#' \code{error.rate.prac2} is the proportion of the prac2 block set of clean trials which are erroneous trials.
#' \code{error.rate.crit2} is the proportion of the crit2 block set of clean trials which are erroneous trials.
#' \code{clean.latencies.prac1} is a matrix of the clean latencies in the first practice block.
#' \code{clean.latencies.crit1} is a matrix of the clean latencies in the first critical block.
#' \code{clean.latencies.prac2} is a matrix of the clean latencies in the second practice block.
#' \code{clean.latencies.crit2} is a matrix of the clean latencies in the second critical block.
#' \code{clean.stim.number.prac1} is a matrix of the clean stimuli ID numbers in the first practice block.
#' \code{clean.stim.number.crit1} is a matrix of the clean stimuli ID numbers in the first critical block.
#' \code{clean.stim.number.prac2} is a matrix of the clean stimuli ID numbers in the second practice block.
#' \code{clean.stim.number.crit2} is a matrix of the clean stimuli ID numbers in the second critical block.
#' \code{clean.correct.prac1} is a matrix stating whether each trial was correct (logical) in the first practice block.
#' \code{clean.correct.crit1} is a matrix stating whether each trial was correct (logical) in the first critical block.
#' \code{clean.correct.prac2} is a matrix stating whether each trial was correct (logical) in the second practice block.
#' \code{clean.correct.crit2} is a matrix stating whether each trial was correct (logical) in the second critical block.
#' \code{clean.means.prac1} is a vector of clean block mean of latencies in the first practice block, one per participant.
#' \code{clean.means.crit1} is a vector of clean block mean of latencies in the first critical block, one per participant.
#' \code{clean.means.prac2} is a vector of clean block mean of latencies in the second practice block, one per participant.
#' \code{clean.means.crit2} is a vector of clean block mean of latencies in the second critical block, one per participant.
#' \code{diff.prac} is a vector (one per person) of the difference between mean latencies compatible and incompatible (practice) blocks.
#' \code{diff.crit} is a vector (one per person) of the difference between mean latencies compatible and incompatible (critical) blocks.
#' \code{inclusive.sd.prac} is a vector (one per person) of the inclusive SD for the practice trials, per Greenwald et al. (2003).
#' \code{inclusive.sd.crit} is a vector (one per person) of the inclusive SD for the critical trials, per Greenwald et al. (2003).
#' \code{D} is a vector (one per person) of the final D scores (i.e., IAT scores).
#' @references Greenwald, A. G., McGhee, D. E., & Schwartz, J. L. K. (1998). Measuring individual differences in implicit cognition: The Implicit Association Test. \emph{Journal of Personality and Social Psychology, 74}, 1464–1480. https://doi.org/10.1037/0022-3514.74.6.1464
#' @references Greenwald, A. G., Nosek, B. A., & Banaji, M. R. (2003). Understanding and using the Implicit Association Test: I. An improved scoring algorithm. \emph{Journal of Personality and Social Psychology, 85}, 197–216. https://doi.org/10.1037/0022-3514.85.2.197
#' @references Lane, K. A., Banaji, M. R., Nosek, B. A., & Greenwald, A. G. (2007). Understanding and using the Implicit Association Test: IV: What we know (so far) about the method. In B. Wittenbrink & N. Schwarz (Eds.), \emph{Implicit measures of attitudes}. (pp. 59–102). New York, NY: Guilford Press.
#' @references Nosek, B. A., Greenwald, A. G., & Banaji, M. R. (2005). Understanding and using the implicit association test: II. Method variables and construct validity. \emph{Personality and Social Psychology Bulletin, 31}, 166–180. https://doi.org/10.1177/0146167204271418
#' @examples \dontrun{
#'
#' ### CLEAN THE IAT USING THE BUILT IN ERROR PENALTY FOR FORCED-ERROR CORRECTION ###
#' clean <- cleanIAT(dat$compatible.prac, dat$compatible.crit, dat$incompatible.prac, dat$incompatible.crit)
#'
#' ### CLEAN THE IAT USING THE D600 PROCEDURE ###
#' clean <- cleanIAT(dat$compatible.prac, dat$compatible.crit, dat$incompatible.prac, dat$incompatible.crit, error.penalty=TRUE, error.penalty.ms=600)
#'
#' ### CLEAN THE IAT USING THE D2SD PROCEDURE###
#' clean <- cleanIAT(dat$compatible.prac, dat$compatible.crit, dat$incompatible.prac, dat$incompatible.crit, error.penalty=TRUE, error.penalty.ms = "2SD")
#'
#' ### CLEAN THE IAT USING THE D2SD PROCEDURE WITH TRIALS UNDER 400 MS DROPPED ###
#' clean <- cleanIAT(dat$compatible.prac, dat$compatible.crit, dat$incompatible.prac, dat$incompatible.crit, fastprt.drop=FALSE, fasttrial.drop=TRUE, fasttrial.ms=400, error.penalty=TRUE, error.penalty.ms = "2SD")
#'
#' ### EXAMINE CLEAN IAT SCORES
#' clean$D
#'
#' ### EXAMINE IAT DIAGNOSTICS ###
#' # TIMEOUT DROP RATE (% of TRIALS) #
#' clean$timeout.rate
#'
#' # LOWER TAIL DROP RATE (% of TRIALS) - NOTE: DISABLED BY DEFAULT #
#' clean$fasttrial.rate
#'
#' # FAST PARTICIPANT DROP COUNT AND RATE (% of SAMPLE) #
#' clean$fastprt.count
#' clean$fastprt.rate
#'
#' # ERROR RATE #
#' clean$error.rate
#' }


cleanIAT <- function(prac1, crit1, prac2, crit2, timeout.drop=TRUE, timeout.ms=10000, fasttrial.drop=FALSE, fasttrial.ms=400, fastprt.drop=TRUE, fastprt.percent=.10, fastprt.ms=300, error.penalty=FALSE, error.penalty.ms=600, inclusive.sd=TRUE) {

  if (is.null(prac1)){stop("One of your input variables does not exist. Please check your data / variable names and try again.")}
  if (is.null(prac2)){stop("One of your input variables does not exist. Please check your data / variable names and try again.")}
  if (is.null(crit1)){stop("One of your input variables does not exist. Please check your data / variable names and try again.")}
  if (is.null(crit2)){stop("One of your input variables does not exist. Please check your data / variable names and try again.")}


  if (all(is.na(prac1))){stop("One of your input variables is empty")}
  if (all(is.na(prac2))){stop("One of your input variables is empty")}
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
  p.prac1 <- substring(prac1, (stringr::str_length(prac1)-2), stringr::str_length(prac1)) != "END"
  p.crit1 <- substring(crit1, (stringr::str_length(crit1)-2), stringr::str_length(crit1)) != "END"
  p.prac2 <- substring(prac2, (stringr::str_length(prac2)-2), stringr::str_length(prac2)) != "END"
  p.crit2 <- substring(crit2, (stringr::str_length(crit2)-2), stringr::str_length(crit2)) != "END"
  prac1[p.prac1] <- ""
  crit1[p.crit1] <- ""
  prac2[p.prac2] <- ""
  crit2[p.crit2] <- ""
  #mark people who skipped IAT as such
  skipped.prac1 <- prac1 == ""
  skipped.crit1 <- crit1 == ""
  skipped.prac2 <- prac2 == ""
  skipped.crit2 <- crit2 == ""
  #check integrity of people who completed IAT
  p.prac1 <- (p.prac1 & !skipped.prac1)
  p.crit1 <- (p.crit1 & !skipped.crit1)
  p.prac2 <- (p.prac2 & !skipped.prac2)
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
  p.prac1 <- as.logical(p.prac1 + check.me(prac1))
  p.crit1 <- as.logical(p.crit1 + check.me(crit1))
  p.prac2 <- as.logical(p.prac2 + check.me(prac2))
  p.crit2 <- as.logical(p.crit2 + check.me(crit2))

  p.prt <- as.logical(p.prac1 + p.crit1 + p.prac2 + p.crit2)
  rm(p.prac1);rm(p.crit1);rm(p.prac2);rm(p.crit2)
  index.prt <- 1:length(p.prt)
  flag<-index.prt[p.prt==TRUE]; rm(index.prt); rm(p.prt)
  prac1[flag] <- ""
  crit1[flag] <- ""
  prac2[flag] <- ""
  crit2[flag] <- ""
  if(length(flag) > 0){
    for(i in 1:length(flag)){
      warning(paste("Participant ",flag[i],"'s web browser encountered an error during the survey. Their IAT data are not usable and not included in analysis.", sep=""))
    }
  }
  #update skip counts to reflect number of validly completed IATs
  skipped.prac1 <- prac1 == ""
  skipped.crit1 <- crit1 == ""
  skipped.prac2 <- prac2 == ""
  skipped.crit2 <- crit2 == ""


  ## BUILD data frames
  raw.prac1 <- data.frame()
  raw.crit1 <- data.frame()
  raw.prac2 <- data.frame()
  raw.crit2 <- data.frame()
  num.raw.trials.prac1 <- numeric() # make a vector. Everyone has same number NOW but will overwrite later
  num.raw.trials.crit1 <- numeric() # make a vector. Everyone has same number NOW but will overwrite later
  num.raw.trials.prac2 <- numeric() # make a vector. Everyone has same number NOW but will overwrite later
  num.raw.trials.crit2 <- numeric() # make a vector. Everyone has same number NOW but will overwrite later

  ## POPULATE data frames. Make all NA if task skipped.

  #prac1
  for(i in 1:length(prac1)){
    source <- toString(prac1[i])
    num.raw.trials.prac1[i] <- stringr::str_count(source,",")
    if (skipped.prac1[i]) {raw.prac1[i,] <- NA} else {
      for(j in 1:num.raw.trials.prac1[i]) {
        comma.location <- stringr::str_locate(source,",")[1]
        raw.prac1[i,j] <- stringr::str_sub(source, 1, comma.location - 1)
        source <- stringr::str_sub(source, comma.location+1, stringr::str_length(source))
      }
    }
  }

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

  #prac2
  for(i in 1:length(prac2)){
    source <- toString(prac2[i])
    num.raw.trials.prac2[i] <- stringr::str_count(source,",")
    if (skipped.prac2[i]) {raw.prac2[i,] <- NA} else {
      for(j in 1:num.raw.trials.prac2[i]) {
        comma.location <- stringr::str_locate(source,",")[1]
        raw.prac2[i,j] <- stringr::str_sub(source, 1, comma.location - 1)
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

  #prac1
  for (i in 1:nrow(raw.prac1)){
    for (j in 1:ncol(raw.prac1)){
      if (!is.na(raw.prac1[i,j])) {raw.prac1[i,j] <- add.leading.zeros(raw.prac1[i,j])}
    }
  }

  #crit1
  for (i in 1:nrow(raw.crit1)){
    for (j in 1:ncol(raw.crit1)){
      if (!is.na(raw.crit1[i,j])) {raw.crit1[i,j] <- add.leading.zeros(raw.crit1[i,j])}
    }
  }

  #prac2
  for (i in 1:nrow(raw.prac2)){
    for (j in 1:ncol(raw.prac2)){
      if (!is.na(raw.prac2[i,j])) {raw.prac2[i,j] <- add.leading.zeros(raw.prac2[i,j])}
    }
  }

  #crit2
  for (i in 1:nrow(raw.crit2)){
    for (j in 1:ncol(raw.crit2)){
      if (!is.na(raw.crit2[i,j])) {raw.crit2[i,j] <- add.leading.zeros(raw.crit2[i,j])}
    }
  }


  temp.prac1 <- median(num.raw.trials.prac1[num.raw.trials.prac1 != 0])
  temp.crit1 <- median(num.raw.trials.crit1[num.raw.trials.crit1 != 0])
  temp.prac2 <- median(num.raw.trials.prac2[num.raw.trials.prac2 != 0])
  temp.crit2 <- median(num.raw.trials.crit2[num.raw.trials.crit2 != 0])
  raw.prac1 <- raw.prac1[,1:temp.prac1]
  raw.crit1 <- raw.crit1[,1:temp.crit1]
  raw.prac2 <- raw.prac2[,1:temp.prac2]
  raw.crit2 <- raw.crit2[,1:temp.crit2]
  num.raw.trials.prac1[num.raw.trials.prac1 > temp.prac1] <- temp.prac1
  num.raw.trials.crit1[num.raw.trials.crit1 > temp.crit1] <- temp.crit1
  num.raw.trials.prac2[num.raw.trials.prac2 > temp.prac2] <- temp.prac2
  num.raw.trials.crit2[num.raw.trials.crit2 > temp.crit2] <- temp.crit2
  rm(temp.crit1); rm(temp.crit2); rm(temp.prac1); rm(temp.prac2)

  ## SAVE stimuli numbers as a data frame. NA handled naturally.
  #prac1
  raw.stim.number.prac1 <- raw.prac1
  for (i in 1:nrow(raw.stim.number.prac1)){
    for (j in 1:ncol(raw.stim.number.prac1)){
      raw.stim.number.prac1[i,j] <- as.numeric(stringr::str_sub(raw.stim.number.prac1[i,j], 1, 2))
    }
  } #not returning numeric -- this fixes it
  for (j in 1:ncol(raw.stim.number.prac1)){
    raw.stim.number.prac1[,j] <- as.numeric(raw.stim.number.prac1[,j])
  }

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


  #prac2
  raw.stim.number.prac2 <- raw.prac2
  for (i in 1:nrow(raw.stim.number.prac2)){
    for (j in 1:ncol(raw.stim.number.prac2)){
      raw.stim.number.prac2[i,j] <- as.numeric(stringr::str_sub(raw.stim.number.prac2[i,j], 1, 2))
    }
  } #not returning numeric -- this fixes it
  for (j in 1:ncol(raw.stim.number.prac2)){
    raw.stim.number.prac2[,j] <- as.numeric(raw.stim.number.prac2[,j])
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

  #prac1
  raw.correct.prac1 <- raw.prac1
  for (i in 1:nrow(raw.correct.prac1)){
    for (j in 1:ncol(raw.correct.prac1)){
      raw.correct.prac1[i,j] <- stringr::str_sub(raw.correct.prac1[i,j], 3, 3)
    }
  }

  #crit1
  raw.correct.crit1 <- raw.crit1
  for (i in 1:nrow(raw.correct.crit1)){
    for (j in 1:ncol(raw.correct.crit1)){
      raw.correct.crit1[i,j] <- stringr::str_sub(raw.correct.crit1[i,j], 3, 3)
    }
  }

  #prac2
  raw.correct.prac2 <- raw.prac2
  for (i in 1:nrow(raw.correct.prac2)){
    for (j in 1:ncol(raw.correct.prac2)){
      raw.correct.prac2[i,j] <- stringr::str_sub(raw.correct.prac2[i,j], 3, 3)
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
  #prac1
  raw.latencies.prac1 <- raw.prac1
  for (i in 1:nrow(raw.latencies.prac1)){
    for (j in 1:ncol(raw.latencies.prac1)){
      end <- nchar(raw.latencies.prac1[i,j])
      raw.latencies.prac1[i,j] <- stringr::str_sub(raw.latencies.prac1[i,j], 4, end)
    }
  }
  for (j in 1:ncol(raw.latencies.prac1)){
    raw.latencies.prac1[,j] <- as.numeric(raw.latencies.prac1[,j])
  }

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

  #prac2
  raw.latencies.prac2 <- raw.prac2
  for (i in 1:nrow(raw.latencies.prac2)){
    for (j in 1:ncol(raw.latencies.prac2)){
      end <- nchar(raw.latencies.prac2[i,j])
      raw.latencies.prac2[i,j] <- stringr::str_sub(raw.latencies.prac2[i,j], 4, end)
    }
  }
  for (j in 1:ncol(raw.latencies.prac2)){
    raw.latencies.prac2[,j] <- as.numeric(raw.latencies.prac2[,j])
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
  clean.latencies.prac1 <-raw.latencies.prac1
  clean.correct.prac1 <- raw.correct.prac1
  clean.stim.number.prac1 <- raw.stim.number.prac1

  clean.latencies.crit1 <-raw.latencies.crit1
  clean.correct.crit1 <- raw.correct.crit1
  clean.stim.number.crit1 <- raw.stim.number.crit1

  clean.latencies.prac2 <-raw.latencies.prac2
  clean.correct.prac2 <- raw.correct.prac2
  clean.stim.number.prac2 <- raw.stim.number.prac2

  clean.latencies.crit2 <-raw.latencies.crit2
  clean.correct.crit2 <- raw.correct.crit2
  clean.stim.number.crit2 <- raw.stim.number.crit2


  ## DROP trials that are too long
  num.timeout.removed.prac1 <- 0 #create a count of timeout responses removed
  num.timeout.removed.crit1 <- 0 #create a count of timeout responses removed
  num.timeout.removed.prac2 <- 0 #create a count of timeout responses removed
  num.timeout.removed.crit2 <- 0 #create a count of timeout responses removed

  if (timeout.drop==TRUE){
    # if enabled, removes trials over 10k ms
    # NA handling: only performs comparison logic if not NA

    #prac1
    for (i in 1:nrow(clean.latencies.prac1)){
      for (j in 1:ncol(clean.latencies.prac1)){
        if (!is.na(clean.latencies.prac1[i,j])){
          if(clean.latencies.prac1[i,j] > timeout.ms) {
            clean.latencies.prac1[i,j] <- NA
            clean.correct.prac1[i,j] <- NA
            clean.stim.number.prac1[i,j] <- NA
            num.timeout.removed.prac1 <- num.timeout.removed.prac1 + 1 #counter of total removals for sample for rate analysis
          }
        }
      }
    }

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

    #prac2
    for (i in 1:nrow(clean.latencies.prac2)){
      for (j in 1:ncol(clean.latencies.prac2)){
        if (!is.na(clean.latencies.prac2[i,j])){
          if(clean.latencies.prac2[i,j] > timeout.ms) {
            clean.latencies.prac2[i,j] <- NA
            clean.correct.prac2[i,j] <- NA
            clean.stim.number.prac2[i,j] <- NA
            num.timeout.removed.prac2 <- num.timeout.removed.prac2 + 1 #counter of total removals for sample for rate analysis
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
  num.timeout.removed <- sum(c(num.timeout.removed.prac1, num.timeout.removed.crit1,
                               num.timeout.removed.prac2, num.timeout.removed.crit2), na.rm=T)


  ## DROP trials that are too short (for some algorithms)
  num.fasttrial.removed.prac1 <- 0
  num.fasttrial.removed.crit1 <- 0
  num.fasttrial.removed.prac2 <- 0
  num.fasttrial.removed.crit2 <- 0

  if (fasttrial.drop == T){
    # removes trials under a given threshold
    # NA handling: only performs comparison logic if not NA

    #prac1
    for (i in 1:nrow(clean.latencies.prac1)){
      for (j in 1:ncol(clean.latencies.prac1)){
        if (!is.na(clean.latencies.prac1[i,j])){
          if(clean.latencies.prac1[i,j] < fasttrial.ms) {
            clean.latencies.prac1[i,j] <- NA
            clean.correct.prac1[i,j] <- NA
            clean.stim.number.prac1[i,j] <- NA
            num.fasttrial.removed.prac1 <- num.fasttrial.removed.prac1 + 1 #counter of total removals for sample for rate analysis
          }
        }
      }
    }

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

    #prac2
    for (i in 1:nrow(clean.latencies.prac2)){
      for (j in 1:ncol(clean.latencies.prac2)){
        if (!is.na(clean.latencies.prac2[i,j])){
          if(clean.latencies.prac2[i,j] < fasttrial.ms) {
            clean.latencies.prac2[i,j] <- NA
            clean.correct.prac2[i,j] <- NA
            clean.stim.number.prac2[i,j] <- NA
            num.fasttrial.removed.prac2 <- num.fasttrial.removed.prac2 + 1 #counter of total removals for sample for rate analysis
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
  num.fasttrial.removed <- sum(c(num.fasttrial.removed.prac1, num.fasttrial.removed.crit1,
                                 num.fasttrial.removed.prac2, num.fasttrial.removed.crit2), na.rm=T)


  ## DROP PARTICIPANT IF OVERLY FAST

  # generate large combo dataset
  fastprt.trials <- cbind(clean.latencies.prac1, clean.latencies.crit1, clean.latencies.prac2, clean.latencies.crit2)
  raw.latencies.combo <- cbind(raw.latencies.prac1, raw.latencies.crit1, raw.latencies.prac2, raw.latencies.crit2) #fast trials may already be dropped. This compares against raw latencies to ensure we don't keep someone because we have already dropped their fast trials.

  #also need a num.raw.trials for this dataset
  num.raw.trials <- num.raw.trials.prac1 + num.raw.trials.crit1 + num.raw.trials.prac2 + num.raw.trials.crit2

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

    clean.latencies.prac1[drop.participant,] <- NA
    clean.latencies.crit1[drop.participant,] <- NA
    clean.correct.prac1[drop.participant,] <- NA
    clean.correct.crit1[drop.participant,] <- NA
    clean.stim.number.prac1[drop.participant,] <- NA
    clean.stim.number.crit1[drop.participant,] <- NA
    clean.latencies.prac2[drop.participant,] <- NA
    clean.latencies.crit2[drop.participant,] <- NA
    clean.correct.prac2[drop.participant,] <- NA
    clean.correct.crit2[drop.participant,] <- NA
    clean.stim.number.prac2[drop.participant,] <- NA
    clean.stim.number.crit2[drop.participant,] <- NA

  } else {
    drop.participant <- rep(FALSE,nrow(fastprt.trials)) # say we're not dropping anyone
  }

  # the above would flag anyone as dropped who simply skipped the task. Adjust.
  skipped <- skipped.prac1 | skipped.crit1 | skipped.prac2 | skipped.crit2  #if any block is skipped, flag as skipped
  fastprt.trials[skipped,] <- NA
  if(fastprt.drop==T) {number.fastprt[skipped] <- NA}
  drop.participant[skipped] <- NA

  # calculate rates of  dropping
  timeout.rate <- num.timeout.removed / sum(num.raw.trials.prac1, num.raw.trials.crit1, num.raw.trials.prac2, num.raw.trials.crit2, na.rm=T)
  fasttrial.rate <- num.fasttrial.removed / sum(num.raw.trials.prac1, num.raw.trials.crit1, num.raw.trials.prac2, num.raw.trials.crit2, na.rm=T)
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
  #prac1
  num.clean.trials.prac1 <- clean.latencies.prac1   # skip handling: make NA
  num.clean.trials.prac1[!is.na(num.clean.trials.prac1)] <- 1
  num.clean.trials.prac1 <- rowSums(num.clean.trials.prac1, na.rm=TRUE)
  num.clean.trials.prac1[skipped.prac1] <- NA
  std.nopenalty.prac1 <- numeric()
  for(i in 1:nrow(clean.latencies.prac1)){
    row <- clean.latencies.prac1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.trials.prac1[i]
    std.nopenalty.prac1[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.prac1[i]-1))
  }
  std.nopenalty.prac1[std.nopenalty.prac1==0] <- NA # anyone who has all trials cut will have a zero SD.
  std.nopenalty.prac1[std.nopenalty.prac1==Inf] <- NA
  std.nopenalty.prac1[is.nan(std.nopenalty.prac1)] <- NA

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

  #prac2
  num.clean.trials.prac2 <- clean.latencies.prac2   # skip handling: make NA
  num.clean.trials.prac2[!is.na(num.clean.trials.prac2)] <- 1
  num.clean.trials.prac2 <- rowSums(num.clean.trials.prac2, na.rm=TRUE)
  num.clean.trials.prac2[skipped.prac2] <- NA
  std.nopenalty.prac2 <- numeric()
  for(i in 1:nrow(clean.latencies.prac2)){
    row <- clean.latencies.prac2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.trials.prac2[i]
    std.nopenalty.prac2[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.prac2[i]-1))
  }
  std.nopenalty.prac2[std.nopenalty.prac2==0] <- NA # anyone who has all trials cut will have a zero SD.
  std.nopenalty.prac2[std.nopenalty.prac2==Inf] <- NA
  std.nopenalty.prac2[is.nan(std.nopenalty.prac2)] <- NA

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
  #prac1
  clean.correct.latencies.prac1 <-clean.latencies.prac1
  for (i in 1:nrow(clean.correct.latencies.prac1)){
    for (j in 1:ncol(clean.correct.latencies.prac1)){
      if (!is.na(clean.correct.latencies.prac1[i,j])){ #cannot have NA in comparisons
        if(raw.correct.prac1[i,j] == "X") {clean.correct.latencies.prac1[i,j] <- NA}
      }
    }
  }

  #crit1
  clean.correct.latencies.crit1 <-clean.latencies.crit1
  for (i in 1:nrow(clean.correct.latencies.crit1)){
    for (j in 1:ncol(clean.correct.latencies.crit1)){
      if (!is.na(clean.correct.latencies.crit1[i,j])){ #cannot have NA in comparisons
        if(raw.correct.crit1[i,j] == "X") {clean.correct.latencies.crit1[i,j] <- NA}
      }
    }
  }

  #prac2
  clean.correct.latencies.prac2 <-clean.latencies.prac2
  for (i in 1:nrow(clean.correct.latencies.prac2)){
    for (j in 1:ncol(clean.correct.latencies.prac2)){
      if (!is.na(clean.correct.latencies.prac2[i,j])){ #cannot have NA in comparisons
        if(raw.correct.prac2[i,j] == "X") {clean.correct.latencies.prac2[i,j] <- NA}
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
  #prac1
  num.clean.correct.prac1 <- clean.correct.latencies.prac1   # skip handling: make NA
  num.clean.correct.prac1[!is.na(num.clean.correct.prac1)] <- 1
  num.clean.correct.prac1 <- rowSums(num.clean.correct.prac1, na.rm=TRUE)
  num.clean.correct.prac1[skipped.prac1] <- NA
  clean.std.correct.prac1 <- numeric()
  for(i in 1:nrow(clean.correct.latencies.prac1)){
    row <- clean.correct.latencies.prac1[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.correct.prac1[i]
    clean.std.correct.prac1[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.correct.prac1[i]-1))
  }
  clean.std.correct.prac1[clean.std.correct.prac1==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
  clean.std.correct.prac1[clean.std.correct.prac1==Inf] <- NA
  clean.std.correct.prac1[is.nan(clean.std.correct.prac1)] <- NA

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

  #prac2
  num.clean.correct.prac2 <- clean.correct.latencies.prac2   # skip handling: make NA
  num.clean.correct.prac2[!is.na(num.clean.correct.prac2)] <- 1
  num.clean.correct.prac2 <- rowSums(num.clean.correct.prac2, na.rm=TRUE)
  num.clean.correct.prac2[skipped.prac2] <- NA
  clean.std.correct.prac2 <- numeric()
  for(i in 1:nrow(clean.correct.latencies.prac2)){
    row <- clean.correct.latencies.prac2[i,]
    avg <- sum(row, na.rm=TRUE) / num.clean.correct.prac2[i]
    clean.std.correct.prac2[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.correct.prac2[i]-1))
  }
  clean.std.correct.prac2[clean.std.correct.prac2==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
  clean.std.correct.prac2[clean.std.correct.prac2==Inf] <- NA
  clean.std.correct.prac2[is.nan(clean.std.correct.prac2)] <- NA

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

  #prac1
  clean.correct.means.prac1 <- rowMeans(clean.correct.latencies.prac1, na.rm=TRUE)
  clean.correct.means.prac1[is.nan(clean.correct.means.prac1)] <- NA
  for (i in 1:nrow(clean.latencies.prac1)){
    for (j in 1:ncol(clean.latencies.prac1)){
      if(!is.na(raw.correct.prac1[i,j])){
        if(error.penalty==TRUE && is.numeric(error.penalty.ms)){
          if(raw.correct.prac1[i,j] == "X" && !is.na(clean.latencies.prac1[i,j])) {clean.latencies.prac1[i,j] = clean.correct.means.prac1[i] + error.penalty.ms}
        } else if (error.penalty==TRUE && error.penalty.ms=="2SD"){
          if(raw.correct.prac1[i,j] == "X" && !is.na(clean.latencies.prac1[i,j])) {clean.latencies.prac1[i,j] = clean.correct.means.prac1[i] + 2*clean.std.correct.prac1[i]}
        } else if (error.penalty==FALSE){
          if(raw.correct.prac1[i,j] == "X" && !is.na(clean.latencies.prac1[i,j])) {clean.latencies.prac1[i,j] = clean.latencies.prac1[i,j]}
        }
        if(raw.correct.prac1[i,j] == "C") {clean.latencies.prac1[i,j] = clean.correct.latencies.prac1[i,j]}
      }
      if(is.na(raw.correct.prac1[i,j])){clean.latencies.prac1[i,j] <- NA} #should already be NA because they were dropped or missing but just to be safe
    }
  }

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

  #prac2
  clean.correct.means.prac2 <- rowMeans(clean.correct.latencies.prac2, na.rm=TRUE)
  clean.correct.means.prac2[is.nan(clean.correct.means.prac2)] <- NA
  for (i in 1:nrow(clean.latencies.prac2)){
    for (j in 1:ncol(clean.latencies.prac2)){
      if(!is.na(raw.correct.prac2[i,j])){
        if(error.penalty==TRUE && is.numeric(error.penalty.ms)){
          if(raw.correct.prac2[i,j] == "X" && !is.na(clean.latencies.prac2[i,j])) {clean.latencies.prac2[i,j] = clean.correct.means.prac2[i] + error.penalty.ms}
        } else if (error.penalty==TRUE && error.penalty.ms=="2SD"){
          if(raw.correct.prac2[i,j] == "X" && !is.na(clean.latencies.prac2[i,j])) {clean.latencies.prac2[i,j] = clean.correct.means.prac2[i] + 2*clean.std.correct.prac2[i]}
        } else if (error.penalty==FALSE){
          if(raw.correct.prac2[i,j] == "X" && !is.na(clean.latencies.prac2[i,j])) {clean.latencies.prac2[i,j] = clean.latencies.prac2[i,j]}
        }
        if(raw.correct.prac2[i,j] == "C") {clean.latencies.prac2[i,j] = clean.correct.latencies.prac2[i,j]}
      }
      if(is.na(raw.correct.prac2[i,j])){clean.latencies.prac2[i,j] <- NA} #should already be NA because they were dropped or missing but just to be safe
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
  clean.means.prac1 <- rowMeans(clean.latencies.prac1, na.rm=TRUE)
  clean.means.prac1[is.nan(clean.means.prac1)] <- NA
  clean.means.crit1 <- rowMeans(clean.latencies.crit1, na.rm=TRUE)
  clean.means.crit1[is.nan(clean.means.crit1)] <- NA
  clean.means.prac2 <- rowMeans(clean.latencies.prac2, na.rm=TRUE)
  clean.means.prac2[is.nan(clean.means.prac2)] <- NA
  clean.means.crit2 <- rowMeans(clean.latencies.crit2, na.rm=TRUE)
  clean.means.crit2[is.nan(clean.means.crit2)] <- NA

  ## generate inclusive SD for D score
  inclusive.sd.prac <- numeric()
  inclusive.trials <- cbind(clean.latencies.prac1, clean.latencies.prac2)
  inclusive.num <- num.clean.trials.prac1 + num.clean.trials.prac2
  for(i in 1:nrow(inclusive.trials)){
    row <- inclusive.trials[i,]
    avg <- sum(row, na.rm=TRUE) / (inclusive.num[i])
    inclusive.sd.prac[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (inclusive.num[i]-1))
  }

  inclusive.sd.crit <- numeric()
  inclusive.trials <- cbind(clean.latencies.crit1, clean.latencies.crit2)
  inclusive.num <- num.clean.trials.crit1 + num.clean.trials.crit2
  for(i in 1:nrow(inclusive.trials)){
    row <- inclusive.trials[i,]
    avg <- sum(row, na.rm=TRUE) / (inclusive.num[i])
    inclusive.sd.crit[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (inclusive.num[i]-1))
  }

  ## final total for calculations
  num.clean.trials <- num.clean.trials.prac1 + num.clean.trials.crit1 + num.clean.trials.prac2 + num.clean.trials.crit2


  # save error rate on non-eliminated trials
  error.rate <- cbind(clean.correct.prac1, clean.correct.crit1, clean.correct.prac2, clean.correct.crit2)
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


  # save error rate on non-eliminated trials - prac1
  error.rate.prac1 <- cbind(clean.correct.prac1)
  error.rate.prac1[error.rate.prac1=="C"] <- 0
  error.rate.prac1[error.rate.prac1=="X"] <- 1
  for (j in 1:ncol(error.rate.prac1)){
    error.rate.prac1[,j] <- as.numeric(error.rate.prac1[,j])
  }
  error.num.prac1 <- rowSums(error.rate.prac1, na.rm=T)
  error.num.prac1[skipped] <- NA # drop skips!
  error.rate.prac1 <- error.num.prac1/ num.clean.trials.prac1
  error.rate.prac1[error.num.prac1 == 0] <- 0   # make zero for people with no errors
  error.num.prac1[drop.participant==TRUE] <- NA
  error.rate.prac1[drop.participant==TRUE] <- NA
  error.rate.prac1[error.rate.prac1 == Inf] <- NA
  error.rate.prac1 <- sum(error.num.prac1, na.rm=T) / sum(num.clean.trials.prac1, na.rm=T)

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

  # save error rate on non-eliminated trials - prac2
  error.rate.prac2 <- cbind(clean.correct.prac2)
  error.rate.prac2[error.rate.prac2=="C"] <- 0
  error.rate.prac2[error.rate.prac2=="X"] <- 1
  for (j in 1:ncol(error.rate.prac2)){
    error.rate.prac2[,j] <- as.numeric(error.rate.prac2[,j])
  }
  error.num.prac2 <- rowSums(error.rate.prac2, na.rm=T)
  error.num.prac2[skipped] <- NA # drop skips!
  error.rate.prac2 <- error.num.prac2/ num.clean.trials.prac2
  error.rate.prac2[error.num.prac2 == 0] <- 0   # make zero for people with no errors
  error.num.prac2[drop.participant==TRUE] <- NA
  error.rate.prac2[drop.participant==TRUE] <- NA
  error.rate.prac2[error.rate.prac2 == Inf] <- NA
  error.rate.prac2 <- sum(error.num.prac2, na.rm=T) / sum(num.clean.trials.prac2, na.rm=T)

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

  diff.prac <- clean.means.prac2-clean.means.prac1
  diff.crit <- clean.means.crit2-clean.means.crit1

  D.prac <- diff.prac / inclusive.sd.prac
  D.crit <- diff.crit / inclusive.sd.crit
  D <- (D.prac + D.crit) / 2

  #### FOR TESTING ONLY--DO NOT USE IN ACTUAL ANALYSEES ###
  if(inclusive.sd==FALSE){

    ## generate within-block SDs for pooling based on final clean data
    #prac1
    num.clean.trials.prac1 <- clean.latencies.prac1   # skip handling: make NA
    num.clean.trials.prac1[!is.na(num.clean.trials.prac1)] <- 1
    num.clean.trials.prac1 <- rowSums(num.clean.trials.prac1, na.rm=TRUE)
    num.clean.trials.prac1[skipped.prac1] <- NA
    clean.std.prac1 <- numeric()
    for(i in 1:nrow(clean.latencies.prac1)){
      row <- clean.latencies.prac1[i,]
      avg <- sum(row, na.rm=TRUE) / num.clean.trials.prac1[i]
      clean.std.prac1[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.prac1[i]-1))
    }
    clean.std.prac1[clean.std.prac1==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
    clean.std.prac1[clean.std.prac1==Inf] <- NA
    clean.std.prac1[is.nan(clean.std.prac1)] <- NA
    #crit1
    num.clean.trials.crit1 <- clean.latencies.crit1   # skip handling: make NA
    num.clean.trials.crit1[!is.na(num.clean.trials.crit1)] <- 1
    num.clean.trials.crit1 <- rowSums(num.clean.trials.crit1, na.rm=TRUE)
    num.clean.trials.crit1[skipped.crit1] <- NA
    clean.std.crit1 <- numeric()
    for(i in 1:nrow(clean.latencies.crit1)){
      row <- clean.latencies.crit1[i,]
      avg <- sum(row, na.rm=TRUE) / num.clean.trials.crit1[i]
      clean.std.crit1[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.crit1[i]-1))
    }
    clean.std.crit1[clean.std.crit1==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
    clean.std.crit1[clean.std.crit1==Inf] <- NA
    clean.std.crit1[is.nan(clean.std.crit1)] <- NA
    #prac2
    num.clean.trials.prac2 <- clean.latencies.prac2   # skip handling: make NA
    num.clean.trials.prac2[!is.na(num.clean.trials.prac2)] <- 1
    num.clean.trials.prac2 <- rowSums(num.clean.trials.prac2, na.rm=TRUE)
    num.clean.trials.prac2[skipped.prac2] <- NA
    clean.std.prac2 <- numeric()
    for(i in 1:nrow(clean.latencies.prac2)){
      row <- clean.latencies.prac2[i,]
      avg <- sum(row, na.rm=TRUE) / num.clean.trials.prac2[i]
      clean.std.prac2[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.prac2[i]-1))
    }
    clean.std.prac2[clean.std.prac2==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
    clean.std.prac2[clean.std.prac2==Inf] <- NA
    clean.std.prac2[is.nan(clean.std.prac2)] <- NA
    #crit2
    num.clean.trials.crit2 <- clean.latencies.crit2   # skip handling: make NA
    num.clean.trials.crit2[!is.na(num.clean.trials.crit2)] <- 1
    num.clean.trials.crit2 <- rowSums(num.clean.trials.crit2, na.rm=TRUE)
    num.clean.trials.crit2[skipped.crit2] <- NA
    clean.std.crit2 <- numeric()
    for(i in 1:nrow(clean.latencies.crit2)){
      row <- clean.latencies.crit2[i,]
      avg <- sum(row, na.rm=TRUE) / num.clean.trials.crit2[i]
      clean.std.crit2[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (num.clean.trials.crit2[i]-1))
    }
    clean.std.crit2[clean.std.crit2==0] <- NA # when fastprt.drop is off and fasttrial.drop is on, this may happen b/c all trials are cut
    clean.std.crit2[clean.std.crit2==Inf] <- NA
    clean.std.crit2[is.nan(clean.std.crit2)] <- NA

    pool.sd.prac <- sqrt((clean.std.prac1^2 * (num.clean.trials.prac1-1) + clean.std.prac2^2 * (num.clean.trials.prac2-1)) / (num.clean.trials.prac1-1 + num.clean.trials.prac2-1))
    pool.sd.crit <- sqrt((clean.std.crit1^2 * (num.clean.trials.crit1-1) + clean.std.crit2^2 * (num.clean.trials.crit2-1)) / (num.clean.trials.crit1-1 + num.clean.trials.crit2-1))
    D.prac <- diff.prac / pool.sd.prac
    D.crit <- diff.crit / pool.sd.crit
    D <- (D.prac + D.crit) / 2
  }

  return(list(
    skipped=skipped,
    raw.latencies.prac1=raw.latencies.prac1,
    raw.latencies.crit1=raw.latencies.crit1,
    raw.latencies.prac2=raw.latencies.prac2,
    raw.latencies.crit2=raw.latencies.crit2,
    raw.stim.number.prac1=raw.stim.number.prac1,
    raw.stim.number.crit1=raw.stim.number.crit1,
    raw.stim.number.prac2=raw.stim.number.prac2,
    raw.stim.number.crit2=raw.stim.number.crit2,
    raw.correct.prac1=raw.correct.prac1,
    raw.correct.crit1=raw.correct.crit1,
    raw.correct.prac2=raw.correct.prac2,
    raw.correct.crit2=raw.correct.crit2,
    timeout.drop=timeout.drop,
    timeout.ms=timeout.ms,
    num.timeout.removed=num.timeout.removed,
    timeout.rate=timeout.rate,
    num.timeout.removed.prac1=num.timeout.removed.prac1,
    num.timeout.removed.crit1=num.timeout.removed.crit1,
    num.timeout.removed.prac2=num.timeout.removed.prac2,
    num.timeout.removed.crit2=num.timeout.removed.crit2,
    fasttrial.drop=fasttrial.drop,
    fasttrial.ms=fasttrial.ms,
    num.fasttrial.removed=num.fasttrial.removed,
    fasttrial.rate=fasttrial.rate,
    num.fasttrial.removed.prac1=num.fasttrial.removed.prac1,
    num.fasttrial.removed.crit1=num.fasttrial.removed.crit1,
    num.fasttrial.removed.prac2=num.fasttrial.removed.prac2,
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
    error.rate.prac1=error.rate.prac1,
    error.rate.crit1=error.rate.crit1,
    error.rate.prac2=error.rate.prac2,
    error.rate.crit2=error.rate.crit2,
    clean.latencies.prac1=clean.latencies.prac1,
    clean.latencies.crit1=clean.latencies.crit1,
    clean.latencies.prac2=clean.latencies.prac2,
    clean.latencies.crit2=clean.latencies.crit2,
    clean.stim.number.prac1=clean.stim.number.prac1,
    clean.stim.number.crit1=clean.stim.number.crit1,
    clean.stim.number.prac2=clean.stim.number.prac2,
    clean.stim.number.crit2=clean.stim.number.crit2,
    clean.correct.prac1=clean.correct.prac1,
    clean.correct.crit1=clean.correct.crit1,
    clean.correct.prac2=clean.correct.prac2,
    clean.correct.crit2=clean.correct.crit2,
    clean.means.prac1=clean.means.prac1,
    clean.means.crit1=clean.means.crit1,
    clean.means.prac2=clean.means.prac2,
    clean.means.crit2=clean.means.crit2,
    diff.prac=diff.prac,
    diff.crit=diff.crit,
    inclulsive.sd.prac=inclusive.sd.prac,
    inclusive.sd.crit=inclusive.sd.crit,
    D=D
  ))
}
