############## WRITE IAT STIMULI POOLS AND CODE ##############
requireNamespace("stringr")
requireNamespace("jsonlite")

writeIATstim <- function(type, combined.type="alternating", n, posside, Aside, catType, nPos, nNeg, poswords, negwords, tgtType, nA, nB, Awords, Bwords, tgtCol="black", catCol="green", norepeat=FALSE, write.me, out){

  ## Misspecification errors:
  if ( n %% 2 != 0 ) {stop("The number of trials per block must be even in all IAT blocks in Iatgen. This allows an equal distribution of left-hand and right-hand stimuli.")}

  if (type == "combined"){
    if ( n %% 4 != 0 ) {stop("The number of trials per combined block must be divisible by four in Iatgen. This allows an equal distribution of Positive, Negative, Target A, and Target B stimuli.")}
    if (combined.type != "random" && combined.type != "alternating") {stop("Type must be 'random' or 'alternating.'")}
  }
  if (type != "combined" && type != "target" && type != "category") {stop("The type of block must be either combined or target. Type is misspecified.")}
  if (catType != "words" && catType != "images") {stop("Category must be either words or images.")}
  if (tgtType != "words" && tgtType != "images") {stop("Targets must be either words or images.")}

  ### DEFINE ELEMENTS
  startpos <- "\tposstim = ["
  startneg <- "\tnegstim = ["
  startA <- "\tAstim = ["
  startB <- "\tBstim = ["
  mid <- "\t\t{stimulus: INSERTSTIM, correct:INSERTCOR, index: INSERTINDEX},"
  end <- "\t];"

  ### IMAGE NUMBER INDEXING
  if (tgtType == "images" && catType =="images"){
    nums.pos <- c(0:(nPos-1))
    nums.neg <- c(nPos:(nPos+nNeg-1))
    nums.A <- c( (nPos + nNeg) : (nPos + nNeg + nA - 1))
    nums.B <- c( (nPos + nNeg + nA) : (nPos + nNeg + nA + nB - 1) )
  }

  if (tgtType == "words" && catType =="images"){
    nums.pos <- c(0:(nPos-1))
    nums.neg <- c(nPos:(nPos+nNeg-1))
  }

  if (tgtType == "images" && catType =="words"){
    nums.A <- c( 0 : (nA - 1))
    nums.B <- c( nA : (nA + nB - 1) )
  }

  ### BUILD POSITIVE STIMULI POOL
  # build posbody
  if (catType=="words"){length.pos <- length(poswords)}
  if (catType=="images"){length.pos <- nPos}
  body <- character()
  for (i in 1:length.pos) {body <- rbind(body, mid)} # add more sections to body
  body[length(body)] <-  gsub("}," , "}",  body[length(body)]) #remove last comma
  body <- rbind(startpos, body, end)
  finpos <- body

  # pos stimuli builder
  if (catType=="words"){
    stim.pos <- paste('\"<b style=\'color:',catCol,'\'>',    poswords,  '</b>"' , sep="")
  } else {
    stim.pos <- paste('images[',nums.pos, ']' , sep="")
  }

  # add content to finpos
  for (i in 2:(length.pos+1)){  #loops through row numbers containing stimuli=normal count + 1. Use i-1 to get normal count.
    finpos[i] <- gsub("INSERTSTIM", stim.pos[(i-1)], finpos[i])
    if (posside == "right") {finpos[i] <- gsub("INSERTCOR", 73, finpos[i])}
    if (posside == "left") {finpos[i] <- gsub("INSERTCOR", 69, finpos[i])}
    if (posside == "none") {finpos[i] <- gsub("INSERTCOR", "\"NA\"", finpos[i])}
    finpos[i] <- gsub("INSERTINDEX", i-1, finpos[i])
  }

  ### BUILD NEGATIVE STIMULI POOL
  # build negbody
  if (catType=="words"){length.neg <- length(negwords)}
  if (catType=="images"){length.neg <- nNeg}
  body <- character()
  for (i in 1:length.neg) {body <- rbind(body, mid)} # add more sections to body
  body[length(body)] <-  gsub("}," , "}",  body[length(body)]) #remove last comma
  body <- rbind(startneg, body, end)
  finneg <- body

  # neg stimuli builder
  if (catType=="words"){
    stim.neg <- paste('\"<b style=\'color:',catCol,'\'>',    negwords,  '</b>"' , sep="")
  } else {
    stim.neg <- paste('images[',nums.neg, ']' , sep="")
  }

  # add content to finneg
  for (i in 2:(length.neg+1)){  #loops through row numbers containing stimuli=normal count + 1. Use i-1 to get normal count.
    finneg[i] <- gsub("INSERTSTIM", stim.neg[(i-1)], finneg[i])
    if (posside == "left") {finneg[i] <- gsub("INSERTCOR", 73, finneg[i])}
    if (posside == "right") {finneg[i] <- gsub("INSERTCOR", 69, finneg[i])}
    if (posside == "none")  {finneg[i] <- gsub("INSERTCOR", "\"NA\"", finneg[i])}
    finneg[i] <- gsub("INSERTINDEX", i + length.pos - 1, finneg[i])
  }

  ### BUILD A STIMULI POOL
  # build Abody
  if (tgtType=="words"){length.A <- length(Awords)}
  if (tgtType=="images"){length.A <- nA}
  body <- character()
  for (i in 1:length.A) {body <- rbind(body, mid)} # add more sections to body
  body[length(body)] <-  gsub("}," , "}",  body[length(body)]) #remove last comma
  body <- rbind(startA, body, end)
  finA <- body

  # A stimuli builder
  if (tgtType=="words"){
    stim.A <- paste('\"<b style=\'color:',tgtCol,'\'>',    Awords,  '</b>"' , sep="")
  } else {
    stim.A <- paste('images[',nums.A, ']' , sep="")
  }

  # add content to finA
  for (i in 2:(length.A+1)){  #loops through row numbers containing stimuli=normal count + 1. Use i-1 to get normal count.
    finA[i] <- gsub("INSERTSTIM", stim.A[(i-1)], finA[i])
    if (Aside == "right") {finA[i] <- gsub("INSERTCOR", 73, finA[i])}
    if (Aside == "left") {finA[i] <- gsub("INSERTCOR", 69, finA[i])}
    if (Aside == "none") {finA[i] <- gsub("INSERTCOR", "\"NA\"", finA[i])}
    finA[i] <- gsub("INSERTINDEX", (i+length.pos+length.neg-1), finA[i])
  }

  ### BUILD B STIMULI POOL
  # build Bbody
  if (tgtType=="words"){length.B <- length(Bwords)}
  if (tgtType=="images"){length.B <- nB}
  body <- character()
  for (i in 1:length.B) {body <- rbind(body, mid)} # add more sections to body
  body[length(body)] <-  gsub("}," , "}",  body[length(body)]) #remove last comma
  body <- rbind(startB, body, end)
  finB <- body

  # B stimuli builder
  if (tgtType=="words"){
    stim.B <- paste('\"<b style=\'color:',tgtCol,'\'>',    Bwords,  '</b>"' , sep="")
  } else {
    stim.B <- paste('images[',nums.B, ']' , sep="")
  }

  # add content to finB
  for (i in 2:(length.B+1)){  #loops through row numbers containing stimuli=normal count + 1. Use i-1 to get normal count.
    finB[i] <- gsub("INSERTSTIM", stim.B[(i-1)], finB[i])
    if (Aside == "left") {finB[i] <- gsub("INSERTCOR", 73, finB[i])}
    if (Aside == "right") {finB[i] <- gsub("INSERTCOR", 69, finB[i])}
    if (Aside == "none") {finB[i] <- gsub("INSERTCOR", "\"NA\"", finB[i])}
    finB[i] <- gsub("INSERTINDEX", (i + length.pos + length.neg + length.A - 1), finB[i])
  }


  ## MID SECTION IS EITHER EMPTY OR CONTAINS INTERMEDIATE CODE FOR ALTERNATING-TRIAL COMBINED BLOCKS
  if (type == "target") {altsection <- ""}
  if (type == "category") {altsection <- ""}
  if (type == "combined" && combined.type=="random") {altsection <- ""}
  if (type == "combined" && combined.type=="alternating") {

    starttgts <- "\ttgts = ["
    startcats <- "\tcats = ["
    midalt<- "\t\t{stimulus: \"\", correct: \"\", index: \"\"},"
    lastalt <- "\t\t{stimulus: \"\", correct: \"\", index: \"\"}"
    endalt <- "\t];"

    bodyalt <- character()
    for (i in 1:(n/2)) {bodyalt <- rbind(bodyalt, midalt)} # add more sections to body
    bodyalt[length(bodyalt)] <- lastalt #replace last row with row w/o ending comma

    #CATS
    headercats <- "\t//EMPTY SET OF CATEGORY STIMULI - USED FOR ALTERNATING TRIALS FORMAT ONLY"
    bodycats <- rbind(headercats, startcats, bodyalt, endalt)

    #TGTS
    headertgts <- "\t//EMPTY SET OF TARGET STIMULI - USED FOR ALTERNATING TRIALS FORMAT ONLY"
    bodytgts <- rbind(headertgts, starttgts, bodyalt, endalt)

    finalt <- rbind(bodycats, "", bodytgts, "")

    ## ADD CODE TO TAKE CONTENTS FROM THESE POOLS TO FINAL STIMULI OBJECT
    # default version randomly samples w/o replacement and randomizes order; otherwise they can be displayed without

      if (norepeat==FALSE) {
        altcode <- rbind(
          "\t//ASSEMBLE TGTS AND CATS FOR ALTERNATING TRIAL FORMAT",
          "\tvar half = tgts.length / 2; //SAME FOR TGTS AND CATS",
          "\tvar cutoffs = [0, half, tgts.length];",
          "\tstimBuilder(Astim, tgts, cutoffs[0], cutoffs[1]);",
          "\tstimBuilder(Bstim, tgts, cutoffs[1], cutoffs[2]);",
          "\tstimBuilder(posstim, cats,  cutoffs[0], cutoffs[1]);",
          "\tstimBuilder(negstim, cats, cutoffs[1], cutoffs[2]);",
          "\tshuffle(tgts);",
          "\tshuffle(tgts);",
          "\tshuffle(cats);",
          "\tshuffle(cats);"
        )
        altsection <- rbind(bodycats, "", bodytgts, "", altcode, "")
      } else {
        altcode <- rbind(
          "\t//ASSEMBLE TGTS AND CATS FOR ALTERNATING TRIAL FORMAT - WILL NOT DISPLAY REPEATS UNTIL ALL TGT/CAT STIMULI ARE SHOWN",
          "\tvar tgtcombo = Astim.concat(Bstim);",
          "\tvar catcombo = posstim.concat(negstim);",
          "\tstimBuilder(tgtcombo, tgts, 0, tgts.length);",
          "\tstimBuilder(catcombo, cats,  0, cats.length);"
        )
        altsection <- rbind(bodycats, "", bodytgts, "", altcode, "")
      }
  }


  ### BUILD STIMULI CONTAINER

  startstim <- "\tstimuli = ["
  midstim <- "\t\t{stimulus: \"\", correct: \"\", index: \"\"},"
  laststim <- "\t\t{stimulus: \"\", correct: \"\", index: \"\"}"
  endstim <- "\t];"

  body <- character()
  for (i in 1:n) {body <- rbind(body, midstim)} # add more sections to body
  body <- rbind(startstim, body)
  body[length(body)] <- laststim #replace last row with row w/o ending comma
  stimheader <- "\t//EMPTY SET OF TRIALS - LOADS FROM POOLS ABOVE"
  finstim <- rbind(stimheader, body, endstim)

  ### COMPILE TRIALS CODE
  trials <- rbind(finpos, "", finneg, "", finA, "", finB, "", altsection, "" , finstim)



  ### JAVASCRIPT CODE THAT ADDS CONTENT TO STIMULI

  if (type=="combined" && combined.type=="random"){
    call <- rbind(
      "\tvar quarter = stimuli.length / 4;",
      "\tvar cutoffs = [0, quarter, quarter*2, quarter*3, stimuli.length];",
      "",
      "\tstimBuilder(posstim, stimuli, cutoffs[0], cutoffs[1]);",
      "\tstimBuilder(negstim, stimuli, cutoffs[1], cutoffs[2]);",
      "\tstimBuilder(Astim, stimuli, cutoffs[2], cutoffs[3]);",
      "\tstimBuilder(Bstim, stimuli, cutoffs[3], cutoffs[4]);",
      "",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);"
    )
  }


  # The reverse is needed for 'norepeat' variants; stimuli displayer pulls from end. Doesn't impact standard variatn as it's random order anywayß
  if(type=="combined" & combined.type=="alternating"){
    call <- rbind(
      "\taltStimuil();",
      "\tstimuli.reverse();"
    )
  }


  if (type=="target" & norepeat==FALSE){
    call <- rbind(
      "\tvar half = stimuli.length / 2;",
      "\tvar cutoffs = [0, half, stimuli.length];",
      "",
      "\tstimBuilder(Astim, stimuli, cutoffs[0], cutoffs[1]);",
      "\tstimBuilder(Bstim, stimuli, cutoffs[1], cutoffs[2]);",
      "",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);"
    )
  }

  if (type=="target" & norepeat==TRUE){
    call <- rbind(
      "\tvar tgtcombo = Astim.concat(Bstim);",
      "\tstimBuilder(tgtcombo, stimuli, 0, stimuli.length);",
      "\tstimuli.reverse();"
    )
  }


   if (type=="category" & norepeat==FALSE){
    call <- rbind(
      "\tvar half = stimuli.length / 2;",
      "\tvar cutoffs = [0, half, stimuli.length];",
      "",
      "\tstimBuilder(posstim, stimuli, cutoffs[0], cutoffs[1]);",
      "\tstimBuilder(negstim, stimuli, cutoffs[1], cutoffs[2]);",
      "",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);"
    )
   }

  if (type=="category" & norepeat==TRUE){
    call <- rbind(
      "\tvar catcombo = posstim.concat(negstim);",
      "\tstimBuilder(catcombo, stimuli, 0, stimuli.length);",
      "\tstimuli.reverse();"
    )
  }

  fin <- rbind(trials, "", "", "\t//BUILD TRIALS", "", call)

  if (write.me){
    con <- file(out, open="wb")
    writeLines(fin, con=out, sep="\n")
    close(con)
  }
  return(fin)
}



############## WRITE IAT JAVASCRIPT FILE ##############

writeIATjs <- function(type, combined.type="alternating", n, posside, Aside, catType, catCol="green", nPos, nNeg,
                       poswords, negwords, tgtType, tgtCol="black", nA, nB, Awords, Bwords,
                       pause=250, errorpause=300, correct.error=F, note=F, norepeat=FALSE,
                       imgs, out) {

  apath  <- system.file("codefiles", "codeA.txt", package="iatgen")
  codeA <- as.matrix(readLines(apath, warn=F))


  ## if IAT uses images, build an image_srcs array
  if (tgtType == "images" || catType == "images"){
    codeimage <- "\timage_srcs = ["
    for (i in 1:length(imgs)) {
      codeimage <- rbind(codeimage, paste('\t\t\"',imgs[i],'\",', sep=""))
    }
    codeimage[length(codeimage)] <- gsub(",$", "", codeimage[length(codeimage)]) # remove comma from last line
    codeimage <- rbind(codeimage,"\t];")
  } else {
    codeimage <- "\timage_srcs = [];"
  }

  bpath  <- system.file("codefiles", "codeB.txt", package="iatgen")
  codeB <- as.matrix(readLines(bpath, warn=F))
  codestim <- writeIATstim(type=type, combined.type=combined.type, n=n, catType=catType, catCol=catCol, nPos=nPos, nNeg=nNeg, poswords=poswords, negwords=negwords, posside=posside, tgtType=tgtType, tgtCol=tgtCol, nA=nA, nB=nB, Awords=Awords, Bwords=Bwords, Aside=Aside, norepeat=norepeat, write.me=FALSE)
  cpath  <- system.file("codefiles", "codeC.txt", package="iatgen")
  codeC <- as.matrix(readLines(cpath, warn=F))
  temp <- rbind(codeA, codeimage, codeB, codestim, codeC)

  #for forced error correction, change the keycheck function call and remover
  if(correct.error==T){
    temp <- gsub(", keyCheck, false);",
                 ", keyCheckForcedError, false);",
                  temp)
  }

  #add note below IAT window
  if(correct.error==T && note==T){
    temp <- gsub("note.innerHTML = \"\";",
                 "note.innerHTML = \"Press E or I to advance to the next word/image. Correct mistakes by pressing the other key.\";",
                 temp)
  }
  if(correct.error==F && note==T){
    temp <- gsub("note.innerHTML = \"\";",
                 "note.innerHTML = \"Press E or I to advance to the next word/image.\";",
                 temp)
  }

  #replace the default 250 ms intertrial pause with one set by user. Greenwald et al 1998 settled on 250 ms
  temp <- gsub(250, pause, temp)

  #replace the default 300 ms error pause with one set by the user.  Greenwald et al 1998 settled on 300 ms
  temp <- gsub(300, errorpause, temp)

  con <- file(out, open="wb")
  writeLines(temp, con, sep="\n")
  close(con)
}




















############## WRITE IAT BLOCKS TO WORKING DIRECTORY FILE ##############

writeIATblocks <- function(startqid=1, combined.type="alternating", foldernum=1, posname, negname, Aname, Bname, posstart, Astart, IATname="IAT", n=c(20, 20, 20, 40, 40, 20, 40),
                           catType, catCol="green", poswords, negwords, nPos, nNeg, posimgs, negimgs, tgtType, tgtCol="black", nA, nB, Awords, Bwords, Aimgs, Bimgs,
                           easy.img=F, pause=250, errorpause=300, correct.error=F, note=F, norepeat=FALSE, imgs
                           ) {


  # add error message if tgtType and catType are not both either "images" or "words

  if (easy.img==T) {
    #easy.img inferrs the nA and nB from the number of images in the vector. I prefer the manual control. Might this cause issues?

    if(tgtType == "images" && catType == "words") {
      # add error message if there are not appropriately specified images
      imgs <- c(Aimgs, Bimgs)
      nA <- length(Aimgs)
      nB <- length(Bimgs)
    }

    if(tgtType == "images" && catType == "images") {
      # add error message if there are not appropriately specified images
      imgs <- c(posimgs, negimgs, Aimgs, Bimgs)
      nA <- length(Aimgs)
      nB <- length(Bimgs)
      nPos <- length(posimgs)
      nNeg <- length(negimgs)
    }

    if(tgtType == "words" && catType == "words") {}

    if(tgtType == "words" && catType == "images") {
      # add error message if there are not appropriately specified images
      imgs <- c(posimgs, negimgs)
      nPos <- length(posimgs)
      nNeg <- length(negimgs)
    }
  }

  if (easy.img==F){
    if (tgtType == "images" || catType == "images"){
      if (sum(c(nPos, nNeg, nA, nB), na.rm=T) != length(imgs)){warning("The number of image URLs provided did not match the number of images listed.")}
    }
  }

  possides <- cbind(matrix(c("NA", "right", "right", "right", "left", "left", "left")),
                    matrix(c("NA", "left", "left", "left", "right", "right", "right")))

  colnames(possides) <- c("right","left") # name columns for the STARTING valence position

  if (Astart == "right" && posstart == "right") { suffix <- "rp" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  if (Astart == "left" && posstart == "right") { suffix <- "ln" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  if (Astart == "right" && posstart == "left") { suffix <- "rn" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  if (Astart == "left" && posstart == "left") { suffix <- "lp" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A

  qids <- 0:6 + startqid

  mainDir <- getwd()
  subDir <- paste(foldernum, " ",IATname,"_",suffix,sep="")

  if (!file.exists(subDir)){
    dir.create(file.path(mainDir, subDir))
  }

  file.copy(system.file("codefiles", "html_1.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "html_2.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "html_3.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "html_4.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "html_5.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "html_6.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "html_7.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "codeA.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "codeB.txt", package="iatgen"), file.path(mainDir, subDir))
  file.copy(system.file("codefiles", "codeC.txt", package="iatgen"), file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))


  writeIATjs(type = "target",
             combined.type=combined.type,
             n=n[1],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             Aside = Astart,
             posside="none",
             nA = nA,
             nB = nA,
             Awords = Awords,
             Bwords = Bwords,
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[1], " JavaScript_1.txt",sep=""))

  writeIATjs(type = "category",
             combined.type=combined.type,
             n = n[2],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside = possides[2,posstart],
             Aside="none",
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             Awords = Awords,
             Bwords = Bwords,
             nA = nA,
             nB = nB,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[2], " JavaScript_2.txt",sep=""))

  writeIATjs(type = "combined",
             combined.type=combined.type,
             n = n[3],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside=possides[3,posstart],
             Aside = Astart,
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             nA = nA,
             nB = nA,
             Awords = Awords,
             Bwords = Bwords,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[3], " JavaScript_3.txt",sep=""))

  writeIATjs(type = "combined",
             combined.type=combined.type,
             n = n[4],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside=possides[4,posstart],
             Aside = Astart,
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             nA = nA,
             nB = nA,
             Awords = Awords,
             Bwords = Bwords,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[4], " JavaScript_4.txt",sep=""))

  writeIATjs(type = "category",
             combined.type=combined.type,
             n = n[5],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside = possides[5,posstart],
             Aside = "none",
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             Awords = Awords,
             Bwords = Bwords,
             nA = nA,
             nB = nB,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[5], " JavaScript_5.txt",sep=""))

  writeIATjs(type = "combined",
             combined.type=combined.type,
             n = n[6],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside=possides[6,posstart],
             Aside = Astart,
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             nA = nA,
             nB = nA,
             Awords = Awords,
             Bwords = Bwords,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[6], " JavaScript_6.txt",sep=""))

  writeIATjs(type = "combined",
             combined.type=combined.type,
             n = n[7],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside=possides[7,posstart],
             Aside = Astart,
             poswords = poswords,
             negwords = negwords,
             nPos = nPos,
             nNeg = nNeg,
             nA = nA,
             nB = nA,
             Awords = Awords,
             Bwords = Bwords,
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             norepeat=norepeat,
             out = paste("Q",qids[7], " JavaScript_7.txt",sep=""))

  ### change the html text
  blocknames <- c("html_1.txt", "html_2.txt", "html_3.txt", "html_4.txt", "html_5.txt", "html_6.txt", "html_7.txt")

  ## NOTE: HTML files are hard-coded with the defaults (green for targets, black for categories). Thus, these just need to be swapped out for tgtCol and catCol regardless of configuration.

  ## Keep the A starts right, good format from the source files
  if (suffix == "rp"){
    for (i in 1:length(blocknames)){
      bltemp <- readLines(blocknames[i], warn=F)
      bltemp <- gsub("tgtA", Aname, bltemp)
      bltemp <- gsub("tgtCol", tgtCol, bltemp)
      bltemp <- gsub("tgtB", Bname, bltemp)
      bltemp <- gsub("POS", posname, bltemp)
      bltemp <- gsub("NEG", negname, bltemp)
      bltemp <- gsub("catCol", catCol, bltemp)

      if (tolower(tgtCol) != "black" || tolower(catCol) != "black") {
        bltemp <- gsub("<!-- colins -->", "The label/item colors may help you identify the appropriate category.", bltemp)
      }
      if (correct.error==T) {
        bltemp <- gsub("<!--errorins-->", "Correct errors by hitting the other key.", bltemp)
      }

      con <- file(paste("Q",qids[i], " ",blocknames[i],sep=""), open="wb")
      writeLines(as.matrix(bltemp), con,sep="\n")
      close(con)
    }
  }

  # A starts right, bad
  if (suffix == "rn"){
    for (i in 1:length(blocknames)){
      bltemp <- readLines(blocknames[i], warn=F)
      bltemp <- gsub("tgtA", Aname, bltemp)
      bltemp <- gsub("tgtB", Bname, bltemp)
      bltemp <- gsub("tgtCol", tgtCol, bltemp)
      bltemp <- gsub("POS", negname, bltemp)
      bltemp <- gsub("NEG", posname, bltemp)
      bltemp <- gsub("catCol", catCol, bltemp)

      if (tolower(tgtCol) != "black" || tolower(catCol) != "black") {
        bltemp <- gsub("<!-- colins -->", "The label/item colors may help you identify the appropriate category.", bltemp)
      }
      if (correct.error==T) {
        bltemp <- gsub("<!--errorins-->", "Correct errors by hitting the other key.", bltemp)
      }
      con <- file(paste("Q",qids[i], " ",blocknames[i],sep=""), open="wb")
      writeLines(as.matrix(bltemp), con,sep="\n")
      close(con)
    }
  }

  # A starts left, bad
  if (suffix == "ln"){
    for (i in 1:length(blocknames)){
      bltemp <- readLines(blocknames[i], warn=F)
      bltemp <- gsub("tgtA", Bname, bltemp)
      bltemp <- gsub("tgtB", Aname, bltemp)
      bltemp <- gsub("tgtCol", tgtCol, bltemp)
      bltemp <- gsub("POS", posname, bltemp)
      bltemp <- gsub("NEG", negname, bltemp)
      bltemp <- gsub("catCol", catCol, bltemp)

      if (tolower(tgtCol) != "black" || tolower(catCol) != "black") {
        bltemp <- gsub("<!-- colins -->", "The label/item colors may help you identify the appropriate category.", bltemp)
      }
      if (correct.error==T) {
        bltemp <- gsub("<!--errorins-->", "Correct errors by hitting the other key.", bltemp)
      }
      con <- file(paste("Q",qids[i], " ",blocknames[i],sep=""), open="wb")
      writeLines(as.matrix(bltemp), con,sep="\n")
      close(con)
      }
  }

  ## A starts left, good
  if (suffix == "lp"){
    for (i in 1:length(blocknames)){
      bltemp <- readLines(blocknames[i], warn=F)
      bltemp <- gsub("tgtA", Bname, bltemp)
      bltemp <- gsub("tgtB", Aname, bltemp)
      bltemp <- gsub("tgtCol", tgtCol, bltemp)
      bltemp <- gsub("POS", negname, bltemp)
      bltemp <- gsub("NEG", posname, bltemp)
      bltemp <- gsub("catCol", catCol, bltemp)

      if (tolower(tgtCol) != "black" || tolower(catCol) != "black") {
        bltemp <- gsub("<!-- colins -->", "The label/item colors may help you identify the appropriate category.", bltemp)
      }
      if (correct.error==T) {
        bltemp <- gsub("<!--errorins-->", "Correct errors by hitting the other key.", bltemp)
      }
      con <- file(paste("Q",qids[i], " ",blocknames[i],sep=""), open="wb")
      writeLines(as.matrix(bltemp), con,sep="\n")
      close(con)
      }
  }

  file.remove("codeA.txt")
  file.remove("codeB.txt")
  file.remove("codeC.txt")
  file.remove("html_1.txt")
  file.remove("html_2.txt")
  file.remove("html_3.txt")
  file.remove("html_4.txt")
  file.remove("html_5.txt")
  file.remove("html_6.txt")
  file.remove("html_7.txt")
  setwd(mainDir) #revert WD back to original
}








############## WRITE FULL IAT FOR USE IN RESEARCH ##############
#' Builds a fully functional IAT with counterbalanced permutations
#'
#' This is the primary function for building IATs. It has two modes. In automatic mode (set \code{qsf=TRUE}), the function creates a fully functional *.qsf file (Qualtrics survey file) in the user’s working directory, ready to import into Qualtrics. In manual mode (the default option, \code{qsf=FALSE}), it creates four numbered folders which contain HTML and JavaScript code which can be pasted into a template (see tutorial for manual mode at www.iatgen.wordpress.com). In both modes, the user specifies features of the IAT. The user must specify a name for the IAT (\code{IATname}) and the four labels for the targets and categories (\code{posname}, \code{negname}, \code{Aname}, and \code{Bname}). The user must also always specify the type of stimuli for both targets (\code{tgtType="words"} or \code{tgtType="images"}) and categories (\code{catType="words"} or \code{catType="images"}). The user must also specify the stimuli sets for each of the four terms (when images: \code{Aimgs}, \code{Bimgs}, \code{posimgs}, and \code{negimgs}; when words: \code{Awords}, \code{Bwords}, \code{poswords}, and \code{negwords}). Word stimuli are specified by vectors of words (e.g., \code{poswords=c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend")}, whereas images are specified by vectors of image URLs: (e.g., \code{posimgs=c("www.website.com/gentle.jpg", "www.website.com/enjoy.jpg", "www.website.com/Heaven.jpg"}). We recommend users host their own images to avoid issues for participants (see tutorial on www.iatgen.wordpress.com). Beyond the above, there are a number of additional settings. By default, the IAT creates a 250-ms pause between trials (Greenwald et al., 1998), but this can be changed using the pause argument (e.g., \code{pause=500}). By default, the function also produces the original Greenwald et al. (1998) variant in which an error message (a red X) flashes for a pause before automatically starting the next trial (default = 300 ms, can be changed by setting \code{errorpause} to a number other than 300 [e.g., \code{errorpause = 400}]). We recommend using a popular variant of the IAT in which the user must correct errors before proceeding to the next trial, which is accomplished by setting \code{correct.error=TRUE}. Users can also edit the color of the targets using the optional \code{tgtCol} argument (e.g., \code{tgtCol="black"}) which is set to black by default. Users can change the color of attributes using the \code{catCol} argument (e.g., \code{catCol="green"}), and can be set to any CSS color (see www.w3schools.com/colors/colors_names.asp for a list of all compatible colors). The user can set the number of trials by specifying the n argument.   Users can enable a note reminding participants about the keypress directions during the IAT by setting \code{note=TRUE}, which is used on some popular IAT websites (e.g., www.projectimplicit.org) and may be useful in online settings where participants cannot directly approach an experimenter with questions.
#'
#' @param qsf (Required, set by default). Logical value, set to \code{FALSE} by default. If \code{qsf=TRUE}, creates a functional Qualtrics Survey File (*.qsf) for the user to import directly into Qualtrics. If set to \code{qsf=FALSE}, the user must copy paste JavaScript and HTML files into a template. The QSF feature is not compatible with multi-IAT designs. See tutorial on www.iatgen.wordpress.com for multi-IAT designs and usage of manual mode.
#' @param IATname (Required, set by default). A short string of text that serves to name the IAT. By default, set as 'IAT'. Do not add spaces or special characters.
#' @param n (Required, set by default). A numeric vector of length seven, indicating the number of trials in each block. By default, \code{c(20, 20, 20, 40, 40, 20, 40)}. Block 5 is set, by default, to 40 trials following Nosek et al. (2005). Trials should be even numbers, and for combined blocks, divisible by 4.
#' @param posname (Required). The name of the positive category as appears to participants.
#' @param negname (Required). The name of the negative category as appears to participants.
#' @param Aname (Required). The name of Target A as appears to participants.
#' @param Bname (Required). The name of Target B as appears to participants.
#' @param catType (Required). Can be \code{catType="words"} or \code{catType="images"}. Determines whether the code adds text or images for category stimuli. If \code{catType="words"}, user must specify two additional arguments: \code{poswords} and \code{negwords}. If \code{catType="images"}, the user must specify two additional arguments: \code{posimgs} and \code{negimgs}.
#' @param catCol (Required, set by default). Sets the color of the category stimuli and on-screen labels. By default, set to \code{catCol="green"} but can be set to any CSS color name.
#' @param poswords (Required if \code{catType="words"}). Should be a vector of stimuli, e.g. \code{poswords=c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend")}. Ignored if \code{catType="images"}.
#' @param negwords (Required if \code{catType="words"}). Should be a vector of stimuli, e.g.  \code{negwords=c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt")}. Ignored if \code{catType="images"}.
#' @param posimgs (Required if \code{catType="images"}). Should be a vector of image URLs, e.g. \code{posimgs=c("www.website.com/gentle.jpg", "www.website.com/enjoy.jpg")}. Users should have legal rights to use images and should host them personally (or via Qualtrics). For more on image sizing and how to host images, see tutorial at www.iatgen.wordpress.com. Ignored if \code{catType="words"}.
#' @param negimgs (Required if \code{catType="images"}). Should be a vector of image URLs, e.g. \code{negimgs=c("www.website.com/poison.jpg", "www.website.com/evil.jpg")}. Users should have legal rights to use images and should host them personally (or via Qualtrics). For more on image sizing and how to host images, see tutorial at www.iatgen.wordpress.com. Ignored if \code{catType="words"}.
#' @param tgtType (Required). Can be \code{tgtType="words"} or \code{tgtType="images"}. Determines whether text or images are used for tgt stimuli. If \code{tgtType="words"}, user must specify two additional arguments: \code{Awords} and \code{Bwords}. If \code{tgtType="images"}, the user must specify two additional arguments: \code{Aimgs} and \code{Bimgs}.
#' @param tgtCol (Required, set by default). Sets the color of the target stimuli and on-screen labels. By default, set to \code{tgtCol="black"} but can be set to any CSS color name.
#' @param Awords (Required if \code{tgtType="words"}). Should be a vector of stimuli, e.g. \code{Awords=c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily")}. Ignored if \code{tgtType="images"}.
#' @param Bwords (Required if \code{tgtType="words"}). Should be a vector of stimuli, e.g. \code{Bwords=c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat")}. Ignored if \code{tgtType="images"}.
#' @param Aimgs (Required if \code{tgtType="images"}). Should be a vector of image URLs, e.g. \code{Aimgs=c("www.website.com/Orchid.jpg", "www.website.com/Tulip.jpg")}. Users should have legal rights to use images and should host them personally (or via Qualtrics). For more on image sizing and how to host images, see tutorial at www.iatgen.wordpress.com. Ignored if \code{tgtType="words"}.
#' @param Bimgs (Required if \code{tgtType="images"}). Should be a vector of image URLs, e.g. \code{Bimgs=c("www.website.com/Wasp.jpg", "www.website.com/flea.jpg")}. Users should have legal rights to use images and should host them personally (or via Qualtrics). For more on image sizing and how to host images, see tutorial at www.iatgen.wordpress.com. Ignored if \code{tgtType="words"}.
#' @param pause (Required, set by default). Numeric value sets the delay between trials (displaying the fixation cross) in milliseconds. By default, set to 250 (Greenwald et al., 1998) but can be set to any value.
#' @param errorpause (Required if \code{correct.error=TRUE}). This sets the amount of time in milliseconds to display the red X in case of an error. By default, set to 300 ms (Greenwald et al., 1998) but can be set to any value. Ignored if \code{correct.error=T}.
#' @param correct.error (Required, set by default). Logical value, set to \code{TRUE} by default. When \code{correct.error=TRUE}, creates a variant where participants must correct errors in order to proceed from one trial to the next (see Greenwald et al., 2003). When \code{correct.error=FALSE}, the IAT follows the original Greenwald et al. (1998) procedure in which an error message flashes on the screen between trials. Note that forced error correction is the default in most modern IAT software.
#' @param note (Required, set by default). Logical value, set to \code{FALSE} by default. When \code{note=TRUE}, displays a persistent note at the bottom of the window reminding participants which keys to press and how to handle errors (if \code{correct.error=TRUE}). This is recommended for non-laboratory use, where participants are unable to ask for assistance.
#' @param norepeat (Required, set by default). Logical value, set to \code{FALSE} by default. This controls the order in which stimuli are displayed. In the IAT, we always sample stimuli randomly without replacement from pools, replenishing the pools after they are depleted. However, iatgen then randomizes (within each block) the order in which those stimuli are displayed (e.g., Gawronski, 2002). Setting this to \code{TRUE} displays stimuli in the order sampled, meaning that there are no repeats seen *by the participant* until all stimuli from that stimuli set have been seen.
#' @param startqid (Required, set by default). Numeric value that impacts how files are named, which is only visible to users in manual mode. Although this does not substantively impact the IAT, it can make building multi-IAT studies easier in manual mode (see tutorial at www.iatgen.wordpress.com). By default, \code{startqid=1}, which means that iatgen creates files named Q1 through Q28, which are intended to be pasted into Q1 through Q28 of a Qualtrics survey. If a user is starting an IAT on a different question number (e.g., adding a second IAT, which starts on Q29 and ends on adding an additional IAT (e.g., as in the multi-IAT templates on www.iatgen.wordpress.com), then (for convenience) the user should set \code{startqid} to the lowest question number for the new IAT. For example, if a user wished to add an a second IAT to Q29 through Q56, the user would set \code{startqid=29}. The software will then clearly label the files Q29 through Q56 so it is clear where to add the code to the survey. This is intended only for advanced users and users building multi-IAT studies (see www.iatgen.wordpress.com for more information).
#' @return Nothing is returned. However, a QSF file (if \code{qsf=T}) or folders (if \code{qsf=F}) are made in the working directory containing both HTML and JavaScript files that are to be pasted into Qualtrics.
#' @seealso See www.iatgen.wordpress.com for tutorials and files.
#' @references Greenwald, A. G., McGhee, D. E., & Schwartz, J. L. K. (1998). Measuring individual differences in implicit cognition: The Implicit Association Test. \emph{Journal of Personality and Social Psychology, 74}, 1464–1480. https://doi.org/10.1037/0022-3514.74.6.1464
#' @references Greenwald, A. G., Nosek, B. A., & Banaji, M. R. (2003). Understanding and using the Implicit Association Test: I. An improved scoring algorithm. \emph{Journal of Personality and Social Psychology, 85}, 197–216. https://doi.org/10.1037/0022-3514.85.2.197
#' @references Nosek, B. A., Greenwald, A. G., & Banaji, M. R. (2005). Understanding and using the implicit association test: II. Method variables and construct validity. \emph{Personality and Social Psychology Bulletin, 31}, 166–180. https://doi.org/10.1177/0146167204271418
#' @examples \dontrun{
#'
#' ### A words-only IAT with recommended settings. IAT examines insects vs. flowers and is named "flowins". Recommended settings builds a QSF file automatically with forced error correction and a note reminding participants of the instructions.
#' ## Note: the following are specified below for example purposes but are specified by default automatically and can be omitted: coloring of stimuli, number of trials per block, pause between trials
#'
#' writeIATfull(IATname="flowins",
#'            posname="Pleasant",
#'            negname="Unpleasant",
#'            Aname="Flowers",
#'            Bname="Insects",
#'            catType="words",
#'            poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
#'            negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
#'            tgtType="words",
#'            Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
#'            Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
#'
#'            #advanced options with recommended IAT settings
#'            n=c(20, 20, 20, 40, 40, 20, 40),
#'            qsf=T,
#'            note=T,
#'            correct.error=T,
#'            pause=250,
#'            tgtCol="black",
#'            catCol="green"
#' )
#'
#'  ### Same IAT but with the persistent task directions disabled (\code{note=FALSE}), forced error correction disabled (\code{correct.error=FALSE}) and a 300 ms pause for the error message (\code{errorpause=300}).
#'
#'writeIATfull(IATname="flowins",
#'             posname="Pleasant",
#'             negname="Unpleasant",
#'             Aname="Flowers",
#'             Bname="Insects",
#'             catType="words",
#'             poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
#'             negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
#'             tgtType="words",
#'             Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
#'             Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
#'
#'             #advanced options
#'             n=c(20, 20, 20, 40, 40, 20, 40),
#'             qsf=T,
#'             note=F,
#'             correct.error=F,
#'             pause=250,
#'             errorpause=300,
#'             tgtCol="black",
#'             catCol="green"
#')
#'
#' ### Same IAT as prior example but with 10 trials for all non-critical blocks and 12 trials for all critical blocks.
#'
#'writeIATfull(IATname="flowins",
#'             posname="Pleasant",
#'             negname="Unpleasant",
#'             Aname="Flowers",
#'             Bname="Insects",
#'             catType="words",
#'             poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
#'             negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
#'             tgtType="words",
#'             Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
#'             Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
#'
#'             #advanced options
#'             n=c(10, 10, 10, 12, 10, 10, 12),
#'             qsf=T,
#'             note=F,
#'             correct.error=F,
#'             pause=250,
#'             errorpause=300,
#'             tgtCol="black",
#'             catCol="green"
#')
#'
#' ### An images-only IAT with recommended settings. Note that image URL vectors are specified first to simplify the code.
#' goodjpg <- c("www.website.com/gentle.jpg",
#'              "www.website.com/enjoy.jpg",
#'              "www.website.com/Heaven.jpg",
#'              "www.website.com/Cheer.jpg")
#'
#' badjpg <- c("www.website.com/Poison.jpg",
#'             "www.website.com/Evil.jpg.",
#'             "www.website.com/Vomit.jpg",
#'             "www.website.com/Ugly.jpg")
#'
#' Ajpg <- c("www.website.com/Orchid.jpg",
#'           "www.website.com/Tulip.jpg",
#'           "www.website.com/Rose.jpg",
#'           "www.website.com/Daisy.jpg")
#'
#' Bjpg <- c("www.website.com/Wasp.jpg",
#'           "www.website.com/Flea.jpg",
#'           "www.website.com/Moth.jpg",
#'           "www.website.com/Bedbug.jpg")
#'
#' writeIATfull(IATname="flowins",
#'             posname="Pleasant",
#'             negname="Unpleasant",
#'             Aname="Flowers",
#'             Bname="Insects",
#'             catType="images",
#'             posimgs = goodjpg,
#'             negimgs = badjpg,
#'             tgtType="images",
#'             Aimgs = Ajpg,
#'             Bimgs = Bjpg,
#'
#'             #advanced options with recommended IAT settings
#'             n=c(20, 20, 20, 40, 40, 20, 40),
#'             qsf=T,
#'             note=T,
#'             correct.error=T,
#'             pause=250,
#'             tgtCol="black",
#'             catCol="green"
#')
#'
#' ### Example IAT with images for categories and words for targets, with recommended settings.
#' goodjpg <- c("www.website.com/gentle.jpg",
#'              "www.website.com/enjoy.jpg",
#'              "www.website.com/Heaven.jpg",
#'              "www.website.com/Cheer.jpg")
#'
#' badjpg <- c("www.website.com/Poison.jpg",
#'             "www.website.com/Evil.jpg.",
#'             "www.website.com/Vomit.jpg",
#'             "www.website.com/Ugly.jpg")
#'
#'writeIATfull(IATname="flowins",
#'             posname="Pleasant",
#'             negname="Unpleasant",
#'             Aname="Flowers",
#'             Bname="Insects",
#'             catType="images",
#'             posimgs = goodjpg,
#'             negimgs = badjpg,
#'             tgtType="words",
#'             Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
#'             Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
#'
#'             #advanced options with recommended IAT settings
#'             n=c(20, 20, 20, 40, 40, 20, 40),
#'             qsf=T,
#'             note=T,
#'             correct.error=T,
#'             pause=250,
#'             tgtCol="black",
#'             catCol="green"
#')
#'
#' ### Example IAT with images for targets and words for categories, with recommended settings.
#' Ajpg <- c("www.website.com/Orchid.jpg",
#'           "www.website.com/Tulip.jpg",
#'           "www.website.com/Rose.jpg",
#'           "www.website.com/Daisy.jpg")
#'
#' Bjpg <- c("www.website.com/Wasp.jpg",
#'           "www.website.com/Flea.jpg",
#'           "www.website.com/Moth.jpg",
#'           "www.website.com/Bedbug.jpg")
#'
#' writeIATfull(IATname="flowins",
#'             posname="Pleasant",
#'             negname="Unpleasant",
#'             Aname="Flowers",
#'             Bname="Insects",
#'             catType="words",
#'             poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
#'             negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
#'             tgtType="images",
#'             Aimgs = Ajpg,
#'             Bimgs = Bjpg,
#'
#'             #advanced options with recommended IAT settings
#'             n=c(20, 20, 20, 40, 40, 20, 40),
#'             qsf=T,
#'             note=T,
#'             correct.error=T,
#'             pause=250,
#'             tgtCol="black",
#'             catCol="green"
#')
#'
#' ### EXAMPLE IAT USING 'norepeat=TRUE" TO SUPPRESS REPEAT STIMULI UNTIL ALL STIMULI FROM THAT CATEGORY HAVE BEEN SEEN
#'
#'#'writeIATfull(IATname="flowins",
#'             posname="Pleasant",
#'             negname="Unpleasant",
#'             Aname="Flowers",
#'             Bname="Insects",
#'             catType="words",
#'             poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
#'             negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
#'             tgtType="words",
#'             Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
#'             Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
#'
#'             #advanced options
#'             n=c(20, 20, 20, 40, 40, 20, 40),
#'             qsf=T,
#'             note=F,
#'             correct.error=F,
#'             pause=250,
#'             errorpause=300,
#'             tgtCol="black",
#'             catCol="green",
#'             norepeat=TRUE
#')
#' }
writeIATfull <- function(IATname="IAT",
                         posname,
                         negname,
                         Aname,
                         Bname,
                         n=c(20,20,20,40,40,20,40),
                         catType,
                         catCol="green",
                         poswords,
                         negwords,
                         posimgs,
                         negimgs,
                         tgtType,
                         tgtCol="black",
                         Awords,
                         Bwords,
                         Aimgs,
                         Bimgs,
                         qsf=FALSE,
                         pause=250,
                         errorpause=300,
                         correct.error=TRUE,
                         note=FALSE,
                         norepeat=FALSE,
                         startqid = 1
) {

  ##IF FORCED ERROR CORRECTION, MAKE ERRORPAUSE THE SAME AS THE REGULAR PAUSE
  # NOTE: ERRORPAUSE IS USED TO HANDLE ISI FOR ERROR TRIALS. IF FORCED ERROR CORRECTION,
  # WE WANT TO USE THE SAME PAUSE REGARDLESS OF ERROR OR NOT
  if (correct.error==T){
    errorpause <- pause
  }

  if ((tgtType != "images") & (tgtType != "words")){
    stop("tgtType argument is not correctly specified.")
  }

  if ((catType != "images") & (catType != "words")){
    stop("catType argument is not correctly specified.")
  }

  if (length(n) != 7){
    stop("n argument is not correctly specified. You must provide the number of trials for all seven blocks.")
  }

  ## BY DEFAULT, IMPLEMENTS THE EASY IMAGE METHOD. nA, nB, nPos, and nNeg not specified by user in this version. Pulls that information from image URL vectors directly.
  if(tgtType == "images" & catType == "words") {
    # add error message if there are not appropriately specified images
    imgs <- c(Aimgs, Bimgs)
    nA <- length(Aimgs)
    nB <- length(Bimgs)
    nPos <- 0
    nNeg <- 0
  }


  if(tgtType == "images" & catType == "images") {
    # add error message if there are not appropriately specified images
    imgs <- c(posimgs, negimgs, Aimgs, Bimgs)
    nA <- length(Aimgs)
    nB <- length(Bimgs)
    nPos <- length(posimgs)
    nNeg <- length(negimgs)
  }

  if(tgtType == "words" & catType == "words") {
    nA <- 0
    nB <- 0
    nPos <- 0
    nNeg <- 0
  }

  if(tgtType == "words" & catType == "images") {
    # add error message if there are not appropriately specified images
    imgs <- c(posimgs, negimgs)
    nPos <- length(posimgs)
    nNeg <- length(negimgs)
    nA <- 0
    nB <- 0
  }

  #Enforce this to prevent errors
  # May not be needed in v10 and up; keep for backwards compatibility
  if(qsf==T){
    startqid <- 1
  }

  # not modifiable to user in v10.
  combined.type <- "alternating"

    writeIATblocks(startqid=startqid, posstart="right", Astart="right", IATname=IATname, foldernum=1, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB,
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, norepeat=norepeat, note=note, imgs = imgs)

    writeIATblocks(startqid=(startqid+7), posstart="left", Astart="right", IATname=IATname, foldernum=2, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB,
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, norepeat=norepeat, note=note, imgs = imgs)

    writeIATblocks(startqid=(startqid+14), posstart="left", Astart="left", IATname=IATname, foldernum=3, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB,
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, norepeat=norepeat, note=note, imgs = imgs)

    writeIATblocks(startqid=(startqid+21), posstart="right", Astart="left", IATname=IATname, foldernum=4, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB,
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, norepeat=norepeat, note=note, imgs = imgs)



  ## if qsf argument is true, make a qsf file
  ## Thanks to Michal Kouril for this incredible code!
  if(qsf==T){


    #code below uses lowercase
    iatname <- IATname

    #copy the template file to the wd
    file.copy(system.file("codefiles", "FullTemplate_-_For_Shiny_V10.qsf", package="iatgen"), file.path(getwd()))

    filename = function() {
      paste('iat-', iatname, '.qsf', sep='')
    }


    qsfTemplate="FullTemplate_-_For_Shiny_V10.qsf"

    # library(jsonlite)
    require(jsonlite)
    q <- jsonlite::fromJSON(qsfTemplate)

    q$SurveyName <- iatname
    q$SurveyEntry$SurveyName <- iatname

    files=c(paste("1 ",iatname,"_rp", sep=''),
            paste("2 ",iatname,"_rn", sep=''),
            paste("3 ",iatname,"_lp", sep=''),
            paste("4 ",iatname,"_ln", sep=''))


    filecontent <- c()
    txtfiles <- list.files(path=files, pattern="*.txt", full.names=T, recursive=T)
    cat(toJSON(txtfiles))
    lapply(txtfiles, function(x) {
      cat(paste("reading file:",x,"\n"))
      t <- readChar(x,file.info(x)$size) # load file
      k <- gsub("^.*/(Q[0-9]+) ([hJ]).*$", "\\1\\2", x)
      filecontent[[k]] <<- t
    })


    cat("Replacing html and Javascript content....\n")
    for (i in 1:length(q$SurveyElements$Payload)) {
      m <- 0
      if (is.list(q$SurveyElements$Payload[i][[1]])) {
        m <- length(grep("Q[0-9]+ [RL][NP][0-9]", q$SurveyElements$Payload[i][[1]]$DataExportTag))
      }
      if (!(m == 0)) {
        # q$SurveyElements$Payload[i][[1]]$DataExportTag
        qnumber <- gsub("^(Q[0-9]+) [RL][NP][0-9]$", "\\1", q$SurveyElements$Payload[i][[1]]$DataExportTag)
        qnumberhtml <- paste(qnumber,'h',sep="")
        qnumberjs <- paste(qnumber,'J',sep="")
        paste(qnumberhtml,qnumberjs)
        q$SurveyElements$Payload[i][[1]]$QuestionText <- filecontent[[qnumberhtml]]
        q$SurveyElements$Payload[i][[1]]$QuestionJS <- filecontent[[qnumberjs]]
      } else {
        if (exists("q$SurveyElements$Payload[i][[1]]$QuestionText") &&
            length(q$SurveyElements$Payload[i][[1]]$QuestionText)>0) {
          qtext <- q$SurveyElements$Payload[i][[1]]$QuestionText
          qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
          q$SurveyElements$Payload[i][[1]]$QuestionText <- qtext
        }
        if (exists("q$SurveyElements$Payload[i][[1]]$QuestionJS") &&
            length(q$SurveyElements$Payload[i][[1]]$QuestionJS)>0) {
          qtext <- q$SurveyElements$Payload[i][[1]]$QuestionJS
          qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
          q$SurveyElements$Payload[i][[1]]$QuestionJS <- qtext
        }
        if (exists("q$SurveyElements$Payload[i][[1]]$QuestionDescription") &&
            length(q$SurveyElements$Payload[i][[1]]$QuestionDescription)>0) {
          qtext <- q$SurveyElements$Payload[i][[1]]$QuestionDescription
          qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
          q$SurveyElements$Payload[i][[1]]$QuestionDescription <- qtext
        }
        if (exists("q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display") &&
            length(q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display)>0) {
          qtext <- q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display
          qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
          q$SurveyElements$Payload[i][[1]]$Choices[1][[1]]$Display <- qtext
        }
        if (exists("q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display") &&
            length(q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display)>0) {
          qtext <- q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display
          qtext <- qsf_iat_rename("Insects", input$aName, "flowers", input$bName, qtext)
          q$SurveyElements$Payload[i][[1]]$Choices[7][[1]]$Display <- qtext
        }
      }
    }

    if (is.character(q$SurveyElements$Payload$DataExportTag)) {
      for (i in 1:length(q$SurveyElements$Payload$DataExportTag)) {
        m <- length(grep("Q[0-9]+ [RL][NP][0-9]", q$SurveyElements$Payload$DataExportTag[i]))
        if (!(m == 0)) {
          qnumber <- gsub("^(Q[0-9]+) [RL][NP][0-9]$", "\\1", q$SurveyElements$Payload$DataExportTag[i])
          qnumberhtml <- paste(qnumber,'h',sep="")
          qnumberjs <- paste(qnumber,'J',sep="")
          paste(qnumberhtml,qnumberjs)
          q$SurveyElements$Payload$QuestionText[i] <- filecontent[[qnumberhtml]]
          q$SurveyElements$Payload$QuestionJS[i] <- filecontent[[qnumberjs]]
        }
      }
    }

    cat("Generating JSON....\n")
    qjson <- toJSON(q,null="null",auto_unbox=T)
    minify(qjson)
    con <- file(filename(), open="wb")
    write(qjson, con)
    close(con)


    #remove template
    file.remove("FullTemplate_-_For_Shiny_V10.qsf")

    #remove HTML and JavaScript folders if QSF
    unlink(files[1], recursive = T)
    unlink(files[2], recursive = T)
    unlink(files[3], recursive = T)
    unlink(files[4], recursive = T)
  }

}

















