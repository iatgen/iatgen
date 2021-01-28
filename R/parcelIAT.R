#' Data analysis function: Splits an IAT into Four Parcels
#' @description Used for latent-variable modeling, this is run on an IAT object, the output from \code{cleanIAT()}. It outputs a list of four D-scores, each representing one 1/4 of the IAT. All four combined blocks are split into four parcels; corresponding portions of practice and critical blocks are then combined into two sets of trials (compatible, incompatible). These are then divided by the person-level SD of all trials across all combined blocks. The resulting four D-scores reflect four parcels of the IAT, with all combined blocks reflected in each parcel.
#' @param input A cleaned IAT, the output from \code{cleanIAT()}.
#' @return Returns a dataframe of D-scores, reflecting 1/4 of the IAT each.
#' @seealso See www.iatgen.wordpress.com for tutorials and files.
#' @references Greenwald, A. G., McGhee, D. E., & Schwartz, J. L. K. (1998). Measuring individual differences in implicit cognition: The Implicit Association Test. \emph{Journal of Personality and Social Psychology, 74}, 1464–1480. https://doi.org/10.1037/0022-3514.74.6.1464
#' @references Buttrick, N., Axt, J., Ebersole, C. R., & Huband, J. (2020). Re-assessing the incremental predictive validity of Implicit Association Tests. Journal of Experimental Social Psychology, 88, 103941. https://doi.org/10.1016/j.jesp.2019.103941
#' @examples \dontrun{
#' ### Collapse  IAT critical blocks  down ####
#' parcel <- parcelIAT(clean)
#'}
parcelIAT <- function(input){
  if(ncol(input$clean.latencies.prac1) %% 4 != 0){
    stop("The number of trials in the practice block must be divisible by four")
  }

  if(ncol(input$clean.latencies.crit1) %% 4 != 0){
    stop("The number of trials in the critical block must be divisible by four")
  }

  if(ncol(input$clean.latencies.prac2) %% 4 != 0){
    stop("The number of trials in the practice block must be divisible by four")
  }

  if(ncol(input$clean.latencies.crit2) %% 4 != 0){
    stop("The number of trials in the critical block must be divisible by four")
  }

  #break number of trials in 4, generate vectors of columns for each parcel
  # start with practice trials
  trials <- ncol(input$clean.latencies.prac1)
  p1.trials <- 1:(trials/4)
  p2.trials <- (1:(trials/4))+max(p1.trials)
  p3.trials <- (1:(trials/4))+max(p2.trials)
  p4.trials <- (1:(trials/4))+max(p3.trials)

  #grab trials for each parcel from compatible and incompatible blocks
  p1.1 <- input$clean.latencies.prac1[,p1.trials]
  p1.2 <- input$clean.latencies.prac2[,p1.trials]

  p2.1 <- input$clean.latencies.prac1[,p2.trials]
  p2.2 <- input$clean.latencies.prac2[,p2.trials]

  p3.1 <- input$clean.latencies.prac1[,p3.trials]
  p3.2 <- input$clean.latencies.prac2[,p3.trials]

  p4.1 <- input$clean.latencies.prac1[,p4.trials]
  p4.2 <- input$clean.latencies.prac2[,p4.trials]

  # now critical trials
  trials <- ncol(input$clean.latencies.crit1)
  c1.trials <- 1:(trials/4)
  c2.trials <- (1:(trials/4))+max(c1.trials)
  c3.trials <- (1:(trials/4))+max(c2.trials)
  c4.trials <- (1:(trials/4))+max(c3.trials)

  #grab trials for each parcel from compatible and incompatible blocks
  c1.1 <- input$clean.latencies.crit1[,c1.trials]
  c1.2 <- input$clean.latencies.crit2[,c1.trials]

  c2.1 <- input$clean.latencies.crit1[,c2.trials]
  c2.2 <- input$clean.latencies.crit2[,c2.trials]

  c3.1 <- input$clean.latencies.crit1[,c3.trials]
  c3.2 <- input$clean.latencies.crit2[,c3.trials]

  c4.1 <- input$clean.latencies.crit1[,c4.trials]
  c4.2 <- input$clean.latencies.crit2[,c4.trials]

  #combine them: each parcel-half should typically hvae 5 prac and 10 crit trials
  parcel1.1 <- cbind(p1.1, c1.1)
  parcel1.2 <- cbind(p1.2, c1.2)

  parcel2.1 <- cbind(p2.1, c2.1)
  parcel2.2 <- cbind(p2.2, c2.2)

  parcel3.1 <- cbind(p3.1, c3.1)
  parcel3.2 <- cbind(p3.2, c3.2)

  parcel4.1 <- cbind(p4.1, c4.1)
  parcel4.2 <- cbind(p4.2, c4.2)

  clean.means <- function(x){
    out <- rowMeans(x, na.rm=TRUE)
    out[is.nan(out)] <- NA
    return(out)
  }

  clean.means.1.1 <- clean.means(parcel1.1)
  clean.means.1.2 <- clean.means(parcel1.2)
  clean.means.2.1 <- clean.means(parcel2.1)
  clean.means.2.2 <- clean.means(parcel2.2)
  clean.means.3.1 <- clean.means(parcel3.1)
  clean.means.3.2 <- clean.means(parcel3.2)
  clean.means.4.1 <- clean.means(parcel4.1)
  clean.means.4.2 <- clean.means(parcel4.2)

  #get inclusive SD across all combined blocks
  inclusive.trials <- cbind(input$clean.latencies.prac1, input$clean.latencies.crit1,
                          input$clean.latencies.prac2, input$clean.latencies.crit2)
  inclusive.sd.x <- numeric()
  inclusive.num <- rowSums(!is.na(inclusive.trials))
  inclusive.num[inclusive.num==0] <- NA
  for(i in 1:nrow(inclusive.trials)){
    row <- inclusive.trials[i,]
    avg <- sum(row, na.rm=TRUE) / (inclusive.num[i])
    inclusive.sd.x[i] <- sqrt(sum((row - avg)^2, na.rm=TRUE) / (inclusive.num[i]-1))
  }

  D.1 <- (clean.means.1.1-clean.means.1.2) / inclusive.sd.x
  D.2 <- (clean.means.2.1-clean.means.2.2) / inclusive.sd.x
  D.3 <- (clean.means.3.1-clean.means.3.2) / inclusive.sd.x
  D.4 <- (clean.means.4.1-clean.means.4.2) / inclusive.sd.x

  return(data.frame(D.1=D.1,
              D.2=D.2,
              D.3=D.3,
              D.4=D.4))
}
