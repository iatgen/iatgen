############## WRITE IAT STIMULI POOLS AND CODE ##############

writeIATstim <- function(type, combined.type="alternating", n, posside, Aside, catType, nPos, nNeg, poswords, negwords, tgtType, nA, nB, Awords, Bwords, tgtCol="black", catCol="green",write.me, out){
  
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
  
  if(type=="combined" && combined.type=="alternating"){
    call <- "\taltStimuil();"
  }
  
  if (type=="target"){
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
  
  if (type=="category"){
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
  
  fin <- rbind(trials, "", "", "\t//BUILD TRIALS", "", call)
  
  if (write.me){ writeLines(fin, con=out) }
  return(fin)
}



############## WRITE IAT JAVASCRIPT FILE ##############

writeIATjs <- function(type, combined.type="alternating", n, posside, Aside, catType, catCol="green", nPos, nNeg, 
                       poswords, negwords, tgtType, tgtCol="black", nA, nB, Awords, Bwords, 
                       pause=250, errorpause=300, correct.error=F, note=F,
                       imgs, out) {
  
  apath  <- system.file("codefiles", "codeA.txt", package="iatgen") 
  codeA <- as.matrix(readLines(apath, warn=F))

  
  ## if IAT uses images, build an image_srcs array
  if (tgtType == "images" || catType == "images"){
    codeimage <- "\timage_srcs = ["
    for (i in 1:length(imgs)) {
      codeimage <- rbind(codeimage, paste('\t\t\"',imgs[i],'\",', sep=""))
    }
    codeimage[length(codeimage)] <- gsub(",", "", codeimage[length(codeimage)]) # remove comma from last line
    codeimage <- rbind(codeimage,"\t];")
  } else {
    codeimage <- "\timage_srcs = [];"
  }
    
  bpath  <- system.file("codefiles", "codeB.txt", package="iatgen")
  codeB <- as.matrix(readLines(bpath, warn=F))
  codestim <- writeIATstim(type=type, combined.type=combined.type, n=n, catType=catType, catCol=catCol, nPos=nPos, nNeg=nNeg, poswords=poswords, negwords=negwords, posside=posside, tgtType=tgtType, tgtCol=tgtCol, nA=nA, nB=nB, Awords=Awords, Bwords=Bwords, Aside=Aside, write.me=FALSE)
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
  
  writeLines(temp, out)
}




















############## WRITE IAT BLOCKS TO WORKING DIRECTORY FILE ##############

writeIATblocks <- function(startqid=1, combined.type="alternating", foldernum=1, posname, negname, Aname, Bname, posstart, Astart, IATname="IAT", n=c(20, 20, 20, 40, 40, 20, 40), 
                           catType, catCol="green", poswords, negwords, nPos, nNeg, posimgs, negimgs, tgtType, tgtCol="black", nA, nB, Awords, Bwords, Aimgs, Bimgs,
                           easy.img=F, pause=250, errorpause=300, correct.error=F, note=F, imgs
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
  
  if (file.exists(subDir)){
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
  } else {
    dir.create(file.path(mainDir, subDir))
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
  }
  
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
      writeLines(as.matrix(bltemp), paste("Q",qids[i], " ",blocknames[i],sep=""))
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
      writeLines(as.matrix(bltemp), paste("Q",qids[i], " ",blocknames[i],sep=""))
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
      writeLines(as.matrix(bltemp), paste("Q",qids[i], " ",blocknames[i],sep=""))
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
      writeLines(as.matrix(bltemp), paste("Q",qids[i], " ",blocknames[i],sep=""))
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
                         correct.error=T,
                         note=F,
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
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, note=note, imgs = imgs)
    
    writeIATblocks(startqid=(startqid+7), posstart="left", Astart="right", IATname=IATname, foldernum=2, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, note=note, imgs = imgs)
    
    writeIATblocks(startqid=(startqid+14), posstart="left", Astart="left", IATname=IATname, foldernum=3, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, note=note, imgs = imgs)
    
    writeIATblocks(startqid=(startqid+21), posstart="right", Astart="left", IATname=IATname, foldernum=4, n=n,
                   posname = posname, negname = negname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, negwords = negwords, nPos = nPos, nNeg = nNeg,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                   pause=pause, errorpause=errorpause, correct.error=correct.error, combined.type=combined.type, note=note, imgs = imgs)

  
  
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
    
    library(jsonlite)
    
    q <- fromJSON(qsfTemplate)
    
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
    write(qjson, filename())
  
    #remove template
    file.remove("FullTemplate_-_For_Shiny_V10.qsf")
    
    #remove HTML and JavaScript folders if QSF
    unlink(files[1], recursive = T)
    unlink(files[2], recursive = T)
    unlink(files[3], recursive = T)
    unlink(files[4], recursive = T)
  }

}


  
  


    











############################### DATA ANALYSIS PACKAGE ###############################
library(stringr)


###### UNCOMMON FUNCTION BUT INCLUDED FOR TEST USERS: APPEND TWO BLOCKS TOGETHER
#' Data analysis function: Appends trials from two blocks together for analysis as one large block
#' @description Allows the user to append trials from multiple blocks together for analysis as a larger block. Accepts as input two vectors of raw data (e.g., practice and critical blocks) and returns one vector. This procedure is not currently used in data cleaning but may be useful for researchers wishing to examine different properties of IAT data. 
#' @param b1 A vector of responses, one per participant, representing a block of trials. 
#' @param b2 A vector of responses, one per participant, representing a different block of trials.
#' @return Returns a single vector of responses that includes trials from \code{b1} and \code{b2}. 
mergeIATblock <- function(b1, b2){
  temp <- character()
  for (i in 1:length(b1)){
    if (!is.na(b1[i]) && !is.na(b2[i])) {
      temp[i] <- paste(b1[i], ',', b2[i], sep="" )
      temp[i] <- sub("END,", "", temp[i])
    } else { 
      temp[i] <- NA
    }
  }
  return(temp)
}



###### STEP 1: COMBINED LEFT / RIGHT VARIANTS

#' Data analysis function: Collapses IAT permutations down prior to cleaning and analysis
#' @description The first step after importing IAT data is collapsing the IAT data into two variables, typically labeled “compatible” and “incompatible.” Data analysis for the IAT requires comparing responses in the "compatible" blocks against performance in the "incompatible" blocks. However, the Qualtrics IAT runs four permutations of the IAT, counterbalancing left/right starting positions of both targets and categories (although research has consistently failed to find any left/right bias on the IAT; e.g., Greenwald et al., 1998; Nosek et al., 2005). As a result of these four permutations, any one block of trials (e.g., the compatible critical block) is distributed across four variables. Thus, it must be combined back together. This is done with the \code{combineIATfourblocks} function. Typically, four variables are of interest (compatible practice, incompatible practice, compatible critical, incompatible critical; see Greenwald et al., 2003; Nosek et al., 2005). Thus, we must create these four variables. The positioning of this information varies depending on the permutation. For example, if Target A starts on the right initially paired with the positive category (RP permutation), then the compatible block comes first and block 3 (RP.3) and block 4 (RP.4) contain compatible practice and critical blocks. However, if Target A starts on the right initially paired with negative (RN permutation), then the incompatible block comes first and this same information comes in blocks 6 and 7 (RN.6 and RN.7). The IAT analysis script (and examples below) have done the work of locating this information for you and are ready to run (so long as users do not alter variable names in the Qualtrics survey).
#' @param name1 A vector of responses representing a critical block (either compatible or incompatible) of trials for one of the four IAT permutations. 
#' @param name2 A vector of responses representing a critical block (either compatible or incompatible) of trials for another of the four IAT permutations. 
#' @param name3 A vector of responses representing a critical block (either compatible or incompatible) of trials for another of the four IAT permutations. 
#' @param name4 A vector of responses representing a critical block (either compatible or incompatible) of trials for another of the four IAT permutations. 
#' @return Returns a single vector of responses that contains all four permutations collapsed into one vector. 
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
#'}

combineIATfourblocks <- function(name1, name2, name3, name4){
  name1 <- as.character(name1)
  name2 <- as.character(name2)
  name3 <- as.character(name3)
  name4 <- as.character(name4)
  if ( all(is.na(name1)) | all(is.na(name2)) | all(is.na(name3)) | all(is.na(name4))){warning("One or more of your input variables contained no data. Please check your variable names and raw data. This function is alerting you to the problem; portions of the IAT may not be scored.")}
  
  name1[is.na(name1)] <- ""
  name2[is.na(name2)] <- ""
  name3[is.na(name3)] <- ""
  name4[is.na(name4)] <- ""
  namecombined <- name1
  namecombined[name1==""] <- as.character(name2[name1==""]) # for blank ones, use alts
  namecombined[name1=="" & name2==""] <- as.character(name3[name1=="" & name2==""]) # for blank ones, use alts
  namecombined[name1=="" & name2=="" & name3==""] <- as.character(name4[name1=="" & name2=="" & name3==""]) # for blank ones, use alts
  namecombined[is.na(namecombined)] <- ""
  return(namecombined)
}



### TWO IAT VERSION .... KEEPING FOR COMPATIBILITY BUT NOT USED

#' Data analysis function: Collapses IAT permutations down prior to cleaning and analysis (two-permutation version)
#' @description This function is a variation of \code{combineIATfourblocks()} but using two permutations as inputs instead of four. Some users may opt to reduce the number of permutations of the IAT (e.g., fixing one category or target to the left side and the other to the right across participants). This was used in intial testing of iatgen and was retained should it be of interest. 
#' @param name1 A vector of responses representing a critical block (either compatible or incompatible) of trials for one of the IAT permutations. 
#' @param name2 A vector of responses representing a critical block (either compatible or incompatible) of trials for the other IAT permutation. 
#' @return Returns a single vector of responses that contains all four permutations collapsed into one variable. 
#' @examples \dontrun{
#' ### Example with only Target A on the right and Target B on the left  ####
#' dat$compatible <- combineIATtwoblocks(dat$Q4.RP4, dat$Q14.RN7)
#' dat$incompatible <- combineIATtwoblocks(dat$Q7.RP7, dat$Q11.RN4)
#' }
combineIATtwoblocks <- function(name1, name2){
  name1 <- as.character(name1)
  name2 <- as.character(name2)
  name1[is.na(name1)] <- ""
  name2[is.na(name2)] <- ""
  
  if ( all(is.na(name1)) | all(is.na(name2)) ){warning("One or more of your input variables contained no data. Please check your variable names and raw data. This function is alerting you to the problem; portions of the IAT may not be scored.")}
  
  namecombined <- name1
  namecombined[name1==""] <- as.character(name2[name1==""]) # for blank ones, use alts
  namecombined[is.na(namecombined)] <- ""
  return(namecombined)
}



###### STEP TWO: CLEAN THE RAW DATA
#' Data analysis function: Processes and cleans raw IAT data
#' @description Prior to running, please see \code{combineIATfourblocks()}. This function processes, cleans, and scores the combined IAT data. In addition, it returns diagnostics (see examples, below). By default, the function implements the D600 cleaning procedures (Greenwald et al., 2003, p 214, center column) but can be easily configured to do other scoring procedures as well. The function accepts as an input four vectors of IAT responses (see \code{prac1}, \code{crit1}, \code{prac2}, and \code{crit2}, below). It returns a list containing a variety of IAT variables, including matrices of clean latencies and other information (see below). The most important is \code{clean$D}, which is the final D score for the analysis. Users can also extract clean block means for each participant using \code{clean$clean.means.prac1}, \code{clean$clean.means.crit1}, \code{clean$clean.means.prac2}, and \code{clean$clean.means.crit2}. Users can extract matricies of clean latencies using \code{clean$clean.latencies.prac1}, \code{clean$clean.latencies.crit1}, etc. Raw latencies can be requested with \code{clean$raw.latencies.prac1}, etc. Users can request to know whether a trial was correct with \code{clean$clean.correct.prac1}, etc. and precisely which stimulus was used on a given trial with \code{clean$clean.stim.number.prac1}, etc. (Stimuli are numbered based on their order entered within each category and following the sequence "positive, negative, tgtA, tgtB". For example, stimulus 1 is the first positive stimulus). See below for more details on other information that is returned from this function. The data cleaning function adheres to Greenwald et al. (2003; see also Lane et al., 2005, p. 92 for a simplified table of data cleaning steps). There are four main data cleaning options. First, long responses are usually dealt with by setting \code{timeout.drop=TRUE} (enabled by default), which drops individual trials over a given threshold (\code{timeout.ms}, which is 10000 ms by default). Next, overly short responses (i.e., button mashing) are dealt with by setting \code{fastprt.drop=TRUE} (enabled by default), which drops participants who have too many fast responses (more than a \code{fastprt.percent} proportion [default = .10] of responses faster than \code{fastprt.ms} [default = 300 ms]). Alternatively, one can remove individual fast trials by setting \code{fasttrial.drop=TRUE} (disabled by default), which uses a default threshold of \code{fasttrial.ms=400} ms. (This is seldom used but enables users to use alternative scoring methods [e.g., Greenwald et al., 2003, p 214, right column]). Finally, an error penalty is imposed on incorrect responses in some variants. If the IAT forces participants to correct errors, then no error penalty should be imposed (\code{error.penalty=FALSE}, the default setting). However, if participants are not forced to correct errors, one is added. Most common is a 600 ms penalty above the clean block mean (Greenwald et al., 2003), which is done by setting \code{error.penalty.ms=600}, sometimes known as the D600 scoring procedure. Greenwald et al. (2003) also suggested one could use two standard deviations instead of 600 ms, which is done by setting \code{error.penalty.ms="2SD"}. 
#' @param prac1 A vector of one kind of practice responses (e.g., compatible practice), one per participant. 
#' @param crit1 A vector of that same kind of critical responses (e.g., compatible critical), one per participant. 
#' @param prac2 A vector of the other kind of practice responses (e.g., incompatible practice), one per participant. 
#' @param cric2 A vector of that same kind of critical responses (e.g., incompatible critical), one per participant. 
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
#' \code{clean.std.prac1} is a vector of clean block standard deviations in the first practice block, one per participant.
#' \code{clean.std.crit1} is a vector of clean block standard deviations in the first critical block, one per participant.
#' \code{clean.std.prac2} is a vector of clean block standard deviations in the second practice block, one per participant.
#' \code{clean.std.crit2} is a vector of clean block standard deviations in the second critical block, one per participant.
#' \code{clean.means.prac1} is a vector of clean block mean of latencies in the first practice block, one per participant.
#' \code{clean.means.crit1} is a vector of clean block mean of latencies in the first critical block, one per participant.
#' \code{clean.means.prac2} is a vector of clean block mean of latencies in the second practice block, one per participant.
#' \code{clean.means.crit2} is a vector of clean block mean of latencies in the second critical block, one per participant.
#' \code{diff.prac} is a vector (one per person) of the difference between mean latencies compatible and incompatible (practice) blocks.
#' \code{diff.crit} is a vector (one per person) of the difference between mean latencies compatible and incompatible (critical) blocks.
#' \code{pool.sd.prac} is a vector (one per person) of the pooled SDs for the clean practice blocks. 
#' \code{pool.sd.crit} is a vector (one per person) of the pooled SDs for the clean critical blocks. 
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


cleanIAT <- function(prac1, crit1, prac2, crit2, timeout.drop=TRUE, timeout.ms=10000, fasttrial.drop=FALSE, fasttrial.ms=400, fastprt.drop=TRUE, fastprt.percent=.10, fastprt.ms=300, error.penalty=FALSE, error.penalty.ms=600) {
  
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
  
  
  ## Detect if task was skipped
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
  
  # NA any skipped / non-real responses  trials
  raw.latencies.crit1[!(raw.latencies.crit1 >= 0)] <- NA
  raw.latencies.crit2[!(raw.latencies.crit2 >= 0)] <- NA
  raw.latencies.prac1[!(raw.latencies.prac1 >= 0)] <- NA
  raw.latencies.prac2[!(raw.latencies.prac2 >= 0)] <- NA
  raw.correct.crit1[!(raw.latencies.crit1 >= 0)] <- NA
  raw.correct.crit2[!(raw.latencies.crit2 >= 0)] <- NA
  raw.correct.prac1[!(raw.latencies.prac1 >= 0)] <- NA
  raw.correct.prac2[!(raw.latencies.prac2 >= 0)] <- NA
  raw.stim.number.crit1[!(raw.latencies.crit1 >= 0)] <- NA
  raw.stim.number.crit2[!(raw.latencies.crit2 >= 0)] <- NA
  raw.stim.number.prac1[!(raw.latencies.prac1 >= 0)] <- NA
  raw.stim.number.prac2[!(raw.latencies.prac2 >= 0)] <- NA
  num.raw.trials.prac1 <- rowSums(!is.na(raw.latencies.prac1))
  num.raw.trials.prac2 <- rowSums(!is.na(raw.latencies.prac2))
  num.raw.trials.crit1 <- rowSums(!is.na(raw.latencies.crit1))
  num.raw.trials.crit2 <- rowSums(!is.na(raw.latencies.crit2))
  
  
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
  
  ## save clean block SD
  
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
  
  ## Dscore
  
  diff.prac <- clean.means.prac2-clean.means.prac1
  diff.crit <- clean.means.crit2-clean.means.crit1
  
  pool.sd.prac <- sqrt((clean.std.prac1^2 * (num.clean.trials.prac1-1) + clean.std.prac2^2 * (num.clean.trials.prac2-1)) / (num.clean.trials.prac1-1 + num.clean.trials.prac2-1))
  pool.sd.crit <- sqrt((clean.std.crit1^2 * (num.clean.trials.crit1-1) + clean.std.crit2^2 * (num.clean.trials.crit2-1)) / (num.clean.trials.crit1-1 + num.clean.trials.crit2-1))
  D.prac <- diff.prac / pool.sd.prac
  D.crit <- diff.crit / pool.sd.crit
  D <- (D.prac + D.crit) / 2
  
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
    clean.std.prac1=clean.std.prac1,
    clean.std.crit1=clean.std.crit1,
    clean.std.prac2=clean.std.prac2,
    clean.std.crit2=clean.std.crit2,
    clean.means.prac1=clean.means.prac1,
    clean.means.crit1=clean.means.crit1,
    clean.means.prac2=clean.means.prac2,
    clean.means.crit2=clean.means.crit2,
    diff.prac=diff.prac,
    diff.crit=diff.crit,
    pool.sd.prac=pool.sd.prac,
    pool.sd.crit=pool.sd.crit,
    D=D
  ))
}










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


