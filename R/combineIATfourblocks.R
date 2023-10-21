#' Data analysis function: Collapses IAT permutations down prior to cleaning and analysis
#' @description The first step after importing IAT data is collapsing the IAT data into two variables, typically labeled “compatible” and “incompatible.” Data analysis for the IAT requires comparing responses in the "compatible" blocks against performance in the "incompatible" blocks. However, the Qualtrics IAT runs four permutations of the IAT, counterbalancing left/right starting positions of both targets and categories (although research has consistently failed to find any left/right bias on the IAT; e.g., Greenwald et al., 1998; Nosek et al., 2005). As a result of these four permutations, any one block of trials (e.g., the compatible critical block) is distributed across four variables. Thus, it must be combined back together. This is done with the \code{combineIATfourblocks} function. Typically, four variables are of interest (compatible practice, incompatible practice, compatible critical, incompatible critical; see Greenwald et al., 2003; Nosek et al., 2005). Thus, we must create these four variables. The positioning of this information varies depending on the permutation. For example, if Target A starts on the right initially paired with the positive category (RP permutation), then the compatible block comes first and block 3 (RP.3) and block 4 (RP.4) contain compatible practice and critical blocks. However, if Target A starts on the right initially paired with negative (RN permutation), then the incompatible block comes first and this same information comes in blocks 6 and 7 (RN.6 and RN.7). The IAT analysis script (and examples below) have done the work of locating this information for you and are ready to run (so long as users do not alter variable names in the Qualtrics survey).
#' @param name1 A vector of responses representing a critical block (either compatible or incompatible) of trials for one of the four IAT permutations.
#' @param name2 A vector of responses representing a critical block (either compatible or incompatible) of trials for another of the four IAT permutations.
#' @param name3 A vector of responses representing a critical block (either compatible or incompatible) of trials for another of the four IAT permutations.
#' @param name4 A vector of responses representing a critical block (either compatible or incompatible) of trials for another of the four IAT permutations.
#' @return Returns a single vector of responses that contains all four permutations collapsed into one vector.
#' @export
#' @seealso See www.iatgen.wordpress.com for tutorials and files.
#' @references Greenwald, A. G., McGhee, D. E., & Schwartz, J. L. K. (1998). Measuring individual differences in implicit cognition: The Implicit Association Test. \emph{Journal of Personality and Social Psychology, 74}, 1464–1480. https://doi.org/10.1037/0022-3514.74.6.1464
#' @references Greenwald, A. G., Nosek, B. A., & Banaji, M. R. (2003). Understanding and using the Implicit Association Test: I. An improved scoring algorithm. \emph{Journal of Personality and Social Psychology, 85}, 197–216. https://doi.org/10.1037/0022-3514.85.2.197
#' @references Nosek, B. A., Greenwald, A. G., & Banaji, M. R. (2005). Understanding and using the implicit association test: II. Method variables and construct validity. \emph{Personality and Social Psychology Bulletin, 31}, 166–180. https://doi.org/10.1177/0146167204271418
#' @examples \dontrun{
#' ### Collapse  IAT critical blocks  down ####
#' dat$compatible.crit <- combineIATfourblocks(dat$Q4.RP4, dat$Q18.LP4, dat$Q14.RN7, dat$Q28.LN7)
#' dat$incompatible.crit <- combineIATfourblocks(dat$Q7.RP7, dat$Q21.LP7, dat$Q11.RN4, dat$Q25.LN4)
#'
#' ### Collapse  IAT practice blocks ####
#' dat$compatible.prac <- combineIATfourblocks(dat$Q3.RP3, dat$Q17.LP3, dat$Q13.RN6, dat$Q27.LN6)
#' dat$incompatible.prac <- combineIATfourblocks(dat$Q6.RP6, dat$Q20.LP6, dat$Q10.RN3, dat$Q24.LN3)
#' }
combineIATfourblocks <- function(name1, name2, name3, name4) {
  name1 <- as.character(name1)
  name2 <- as.character(name2)
  name3 <- as.character(name3)
  name4 <- as.character(name4)
  if (all(is.na(name1)) | all(is.na(name2)) | all(is.na(name3)) | all(is.na(name4))) {
    warning("One or more of your input variables contained no data. Please check your variable names and raw data. This function is alerting you to the problem; portions of the IAT may not be scored.")
  }

  name1[is.na(name1)] <- ""
  name2[is.na(name2)] <- ""
  name3[is.na(name3)] <- ""
  name4[is.na(name4)] <- ""
  name1[name1 == " "] <- ""
  name2[name2 == " "] <- ""
  name3[name3 == " "] <- ""
  name4[name4 == " "] <- ""
  namecombined <- name1
  namecombined[name1 == ""] <- as.character(name2[name1 == ""]) # for blank ones, use alts
  namecombined[name1 == "" & name2 == ""] <- as.character(name3[name1 == "" & name2 == ""]) # for blank ones, use alts
  namecombined[name1 == "" & name2 == "" & name3 == ""] <- as.character(name4[name1 == "" & name2 == "" & name3 == ""]) # for blank ones, use alts
  namecombined[is.na(namecombined)] <- ""
  return(namecombined)
}



### TWO IAT VERSION .... KEEPING FOR COMPATIBILITY BUT NOT USED

#' Data analysis function: Collapses IAT permutations down prior to cleaning and analysis (two-permutation version)
#' @description This function is a variation of \code{combineIATfourblocks()} but using two permutations as inputs instead of four. Some users may opt to reduce the number of permutations of the IAT (e.g., fixing one category or target to the left side and the other to the right across participants). This was used in intial testing of iatgen and was retained should it be of interest.
#' @param name1 A vector of responses representing a critical block (either compatible or incompatible) of trials for one of the IAT permutations.
#' @param name2 A vector of responses representing a critical block (either compatible or incompatible) of trials for the other IAT permutation.
#' @return Returns a single vector of responses that contains all four permutations collapsed into one variable.
#' @export
#' @examples \dontrun{
#' ### Example with only Target A on the right and Target B on the left  ####
#' dat$compatible <- combineIATtwoblocks(dat$Q4.RP4, dat$Q14.RN7)
#' dat$incompatible <- combineIATtwoblocks(dat$Q7.RP7, dat$Q11.RN4)
#' }
combineIATtwoblocks <- function(name1, name2) {
  name1 <- as.character(name1)
  name2 <- as.character(name2)
  name1[is.na(name1)] <- ""
  name2[is.na(name2)] <- ""
  name1[name1 == " "] <- ""
  name2[name2 == " "] <- ""

  if (all(is.na(name1)) | all(is.na(name2))) {
    warning("One or more of your input variables contained no data. Please check your variable names and raw data. This function is alerting you to the problem; portions of the IAT may not be scored.")
  }

  namecombined <- name1
  namecombined[name1 == ""] <- as.character(name2[name1 == ""]) # for blank ones, use alts
  namecombined[is.na(namecombined)] <- ""
  return(namecombined)
}
