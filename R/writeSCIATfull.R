############## WRITE IAT STIMULI POOLS AND CODE ##############
library(stringr)
writeSCIATstim <- function(type, n, posside, Aside, catType, nPos, poswords, tgtType, nA, nB, Awords, Bwords, tgtCol="black", catCol="green",write.me, out){
  
  ## Misspecification errors:
  if ( n %% 2 != 0 ) {stop("The number of trials per block must be even in all SC-IAT blocks in Iatgen.")}
  
  if (type == "combined"){
    if ( n %% 24 != 0 ) {stop("The number of trials per combined block must be divisible by 24 in an SC-IAT. This allows a 7:7:10 ratio")}
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

    nums.A <- c( nPos: (nPos  + nA - 1))
    nums.B <- c( (nPos + nA) : (nPos + + nA + nB - 1) )
  }
  
  if (tgtType == "words" && catType =="images"){
    nums.pos <- c(0:(nPos-1))
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
    finA[i] <- gsub("INSERTINDEX", (i+length.pos-1), finA[i])
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
    finB[i] <- gsub("INSERTINDEX", (i + length.pos + length.A - 1), finB[i])
  }
  
  
  ## MID SECTION IS EITHER EMPTY OR CONTAINS INTERMEDIATE CODE FOR ALTERNATING-TRIAL COMBINED BLOCKS
  if (type == "target") {altsection <- ""}
  if (type == "category") {altsection <- ""}
  if (type == "combined") {altsection <- ""} #effectively not using. Used for alternating types in full IAT. Keep here in case you want some sort of fixed format later. 
  
  
  
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
  trials <- rbind(finpos, "", finA, "", finB, "", altsection, "" , finstim)
  

  
  ### JAVASCRIPT CODE THAT ADDS CONTENT TO STIMULI
  
  if (type=="combined"){
    
    scale <- n/24
    
    #maintains 7-7-10 ratio (pos, A, B) for congruent blocks
    cutsAPOS <- c(
      0,
      (7*scale-1)+1,
      (14*scale-1)+1,
      (n-1)+1
    )
    
    #maintains 7-10-7 ratio (pos, A, B) for incongruent blocks
    cutsBPOS <- c(
      0,
      (7*scale-1)+1,
      (17*scale-1)+1,
      (n-1)+1
    )
    
    
    call <- rbind(
      if(Aside==posside){paste("\tvar cutoffs = [0, ",cutsAPOS[2], ", ", cutsAPOS[3], ", ", cutsAPOS[4],"];", sep="")},
      if(Aside!=posside){paste("\tvar cutoffs = [0, ",cutsBPOS[2], ", ", cutsBPOS[3], ", ", cutsBPOS[4],"];", sep="")},
      "",
      "\tstimBuilder(posstim, stimuli, cutoffs[0], cutoffs[1]);",
      "\tstimBuilder(Astim, stimuli, cutoffs[1], cutoffs[2]);",
      "\tstimBuilder(Bstim, stimuli, cutoffs[2], cutoffs[3]);",
      "",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);",
      "\tshuffle(stimuli);"
    )
  }
  
  #if(type=="combined" && combined.type=="alternating"){
  #  call <- "\taltStimuil();"
  #}
  
  #if (type=="target"){
  #  call <- rbind(
  #    "\tvar half = stimuli.length / 2;",
  #    "\tvar cutoffs = [0, half, stimuli.length];",
  #    "",
  #    "\tstimBuilder(Astim, stimuli, cutoffs[0], cutoffs[1]);",
  #    "\tstimBuilder(Bstim, stimuli, cutoffs[1], cutoffs[2]);",
  #    "",
  #    "\tshuffle(stimuli);",
  #    "\tshuffle(stimuli);"
  #  )
  #}
  
  #if (type=="category"){
  #  call <- rbind(
  #    "\tvar half = stimuli.length / 2;",
  #    "\tvar cutoffs = [0, half, stimuli.length];",
  #    "",
  #   "\tstimBuilder(posstim, stimuli, cutoffs[0], cutoffs[1]);",
  #    "\tstimBuilder(negstim, stimuli, cutoffs[1], cutoffs[2]);",
  #    "",
  #    "\tshuffle(stimuli);",
  #    "\tshuffle(stimuli);"
  #  )
  #}
  
  fin <- rbind(trials, "", "", "\t//BUILD TRIALS", "", call)
  
  if (write.me){ writeLines(fin, con=out) }
  return(fin)
}



############## WRITE IAT JAVASCRIPT FILE ##############

writeSCIATjs <- function(type, n, posside, Aside, catType, catCol="green", nPos, 
                       poswords, tgtType, tgtCol="black", nA, nB, Awords, Bwords, 
                       pause=250, errorpause=300, correct.error=F, note=F,
                       imgs, out) {
  
  apath  <- system.file("codefiles", "SCcodeA.txt", package="iatgen") 
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
    
  bpath  <- system.file("codefiles", "SCcodeB.txt", package="iatgen")
  codeB <- as.matrix(readLines(bpath, warn=F))
  codestim <- writeSCIATstim(type=type, 
                           n=n, 
                           catType=catType, 
                           catCol=catCol, 
                           nPos=nPos, 
                           poswords=poswords, 
                          posside=posside, 
                          tgtType=tgtType, 
                          tgtCol=tgtCol, 
                          nA=nA, 
                          nB=nB, 
                          Awords=Awords, 
                          Bwords=Bwords, 
                          Aside=Aside, 
                          write.me=FALSE)
  cpath  <- system.file("codefiles", "SCcodeC.txt", package="iatgen")
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

writeSCIATblocks <- function(startqid=1, foldernum=1, posname, Aname, Bname, posstart, Astart, IATname="IAT", n=c(24, 72, 24, 72), 
                           catType, catCol="green", poswords, nPos, posimgs, tgtType, tgtCol="black", nA, nB, Awords, Bwords, Aimgs, Bimgs,
                           easy.img=F, pause=250, errorpause=300, correct.error=F, note=F, imgs
                           ) {
  

  # add error message if tgtType and catType are not both either "images" or "words 

  #DISABLED IN THIS FUCNTION, BUT HIGHER ORDER FUNCTIONS IMPLEMENT AT THAT LEVEL AND PASS DOWN. KEEP HERE FOR TESTING.
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
      imgs <- c(posimgs, Aimgs, Bimgs)
      nA <- length(Aimgs)
      nB <- length(Bimgs)
      nPos <- length(posimgs)
    }
    
    if(tgtType == "words" && catType == "words") {}
    
    if(tgtType == "words" && catType == "images") {
      # add error message if there are not appropriately specified images
      imgs <- c(posimgs)
      nPos <- length(posimgs)
    }
  }
  
  if (easy.img==F){
    if (tgtType == "images" || catType == "images"){
      if (sum(c(nPos, nA, nB), na.rm=T) != length(imgs)){warning("The number of image URLs provided did not match the number of images listed.")}
    }
  }
  
  possides <- cbind(matrix(c("right", "right", "left", "left")), 
                    matrix(c("left", "left", "right", "right")))
  
  colnames(possides) <- c("right","left") # name columns for the STARTING valence position
  
  if (Astart == "right" && posstart == "right") { suffix <- "rp" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  if (Astart == "left" && posstart == "right") { suffix <- "ln" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  if (Astart == "right" && posstart == "left") { suffix <- "rn" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  if (Astart == "left" && posstart == "left") { suffix <- "lp" } # SUFFIX ALWAYS REFLECTS STATUS OF TGT A
  
  qids <- 0:3 + startqid
  
  mainDir <- getwd()
  subDir <- paste(foldernum, " ",IATname,"_",suffix,sep="")
  
  if (file.exists(subDir)){
    file.copy(system.file("codefiles", "SChtml_1.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_2.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_3.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_4.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SCcodeA.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SCcodeB.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SCcodeC.txt", package="iatgen"), file.path(mainDir, subDir))
    file.rename(frome="SChtml_1.txt", to = "html_1.txt")
    file.rename(frome="SChtml_2.txt", to = "html_2.txt")
    file.rename(frome="SChtml_3.txt", to = "html_3.txt")
    file.rename(frome="SChtml_4.txt", to = "html_4.txt")
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create(file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_1.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_2.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_3.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SChtml_4.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SCcodeA.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SCcodeB.txt", package="iatgen"), file.path(mainDir, subDir))
    file.copy(system.file("codefiles", "SCcodeC.txt", package="iatgen"), file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir)) 
  }

  file.rename(from="SChtml_1.txt", to = "html_1.txt")
  file.rename(from="SChtml_2.txt", to = "html_2.txt")
  file.rename(from="SChtml_3.txt", to = "html_3.txt")
  file.rename(from="SChtml_4.txt", to = "html_4.txt")
  writeSCIATjs(type = "combined",
             n=n[1],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             Aside = Astart, 
             posside=possides[1,posstart],
             nA = nA, 
             nB = nA, 
             Awords = Awords,
             Bwords = Bwords, 
             poswords = poswords, 
             nPos = nPos,         
             imgs=imgs,
             pause=pause,
             note=note,
             errorpause=errorpause,
             correct.error=correct.error,
             out = paste("Q",qids[1], " JavaScript_1.txt",sep=""))
  
  writeSCIATjs(type = "combined",
             n = n[2],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside = possides[2,posstart],
             Aside=Astart,
             poswords = poswords, 
             nPos = nPos,
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
  
  writeSCIATjs(type = "combined",
             n = n[3],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside=possides[3,posstart],
             Aside = Astart,
             poswords = poswords, 
             nPos = nPos,
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
  
  writeSCIATjs(type = "combined",
             n = n[4],
             tgtType = tgtType,
             tgtCol = tgtCol,
             catType = catType,
             catCol = catCol,
             posside=possides[4,posstart],
             Aside = Astart,
             poswords = poswords, 
             nPos = nPos, 
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

  
  ### change the html text
  blocknames <- c("html_1.txt", "html_2.txt", "html_3.txt", "html_4.txt")
  
  ## NOTE: HTML files are hard-coded with the defaults (green for targets, black for categories). Thus, these just need to be swapped out for tgtCol and catCol regardless of configuration.
  
  ## Keep the A starts right, good format from the source files
  if (suffix == "rp"){
    for (i in 1:length(blocknames)){
      bltemp <- readLines(blocknames[i], warn=F)
      bltemp <- gsub("tgtA", Aname, bltemp)
      bltemp <- gsub("tgtCol", tgtCol, bltemp)
      bltemp <- gsub("tgtB", Bname, bltemp)
      bltemp <- gsub("POS", posname, bltemp)
      bltemp <- gsub("or<div style=\"color: catCol;\">NEG</div>", "", bltemp) 
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
      bltemp <- gsub("or<div style=\"color: catCol;\">POS</div>", "", bltemp)
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
      bltemp <- gsub("or<div style=\"color: catCol;\">NEG</div>", "", bltemp) 
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
      bltemp <- gsub("or<div style=\"color: catCol;\">POS</div>", "", bltemp)
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
  
  file.remove("SCcodeA.txt")
  file.remove("SCcodeB.txt")
  file.remove("SCcodeC.txt")
  file.remove("html_1.txt")
  file.remove("html_2.txt")
  file.remove("html_3.txt")
  file.remove("html_4.txt")
  setwd(mainDir) #revert WD back to original
}









writeSCIATfull <- function(IATname="IAT", 
                         posname, 
                         Aname, 
                         Bname,
                         n=c(24,72,24,72), 
                         catType, 
                         catCol="green",
                         poswords,
                         posimgs, 
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
    
  if (length(n) != 4){
    stop("n argument is not correctly specified. You must provide the number of trials for all four blocks.")
  }
    
  ## BY DEFAULT, IMPLEMENTS THE EASY IMAGE METHOD. nA, nB, and nPos not specified by user in this version. Pulls that information from image URL vectors directly.
  if(tgtType == "images" & catType == "words") {
    # add error message if there are not appropriately specified images
    imgs <- c(Aimgs, Bimgs)
    nA <- length(Aimgs)
    nB <- length(Bimgs)
    nPos <- 0
  }
  
  
  if(tgtType == "images" & catType == "images") {
    # add error message if there are not appropriately specified images
    imgs <- c(posimgs, Aimgs, Bimgs)
    nA <- length(Aimgs)
    nB <- length(Bimgs)
    nPos <- length(posimgs)
  }
  
  if(tgtType == "words" & catType == "words") {
    nA <- 0
    nB <- 0
    nPos <- 0
  }
  
  if(tgtType == "words" & catType == "images") {
    # add error message if there are not appropriately specified images
    imgs <- c(posimgs)
    nPos <- length(posimgs)
    nA <- 0
    nB <- 0
  }
  
  #Enforce this to prevent errors
  # May not be needed in v10 and up; keep for backwards compatibility
  if(qsf==T){
    startqid <- 1
  }
  
  
  
    writeSCIATblocks(startqid=startqid, posstart="right", Astart="right", IATname=IATname, foldernum=1, n=n,
                   posname = posname, Aname = Aname, Bname = Bname,
                   catType = catType, catCol=catCol, poswords = poswords, nPos = nPos,
                   tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                   pause=pause, errorpause=errorpause, correct.error=correct.error, note=note, imgs = imgs)
    
    writeSCIATblocks(startqid=(startqid+4), posstart="left", Astart="right", IATname=IATname, foldernum=2, n=n,
                     posname = posname, Aname = Aname, Bname = Bname,
                     catType = catType, catCol=catCol, poswords = poswords, nPos = nPos,
                     tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                     pause=pause, errorpause=errorpause, correct.error=correct.error, note=note, imgs = imgs)
    
    writeSCIATblocks(startqid=(startqid+8), posstart="left", Astart="left", IATname=IATname, foldernum=3, n=n,
                     posname = posname, Aname = Aname, Bname = Bname,
                     catType = catType, catCol=catCol, poswords = poswords, nPos = nPos,
                     tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                     pause=pause, errorpause=errorpause, correct.error=correct.error, note=note, imgs = imgs)
    
    writeSCIATblocks(startqid=(startqid+12), posstart="right", Astart="left", IATname=IATname, foldernum=4, n=n,
                     posname = posname, Aname = Aname, Bname = Bname,
                     catType = catType, catCol=catCol, poswords = poswords, nPos = nPos,
                     tgtType = tgtType, tgtCol=tgtCol, Awords = Awords, Bwords = Bwords, nA = nA, nB = nB, 
                     pause=pause, errorpause=errorpause, correct.error=correct.error, note=note, imgs = imgs)

  
  
  ## if qsf argument is true, make a qsf file
  ## Thanks to Michal Kouril for this incredible code!
  if(qsf==T){
    
    
    #code below uses lowercase
    iatname <- IATname
    
    #copy the template file to the wd
    file.copy(system.file("codefiles", "SCTemplate_-_For_Shiny_V10.qsf", package="iatgen"), file.path(getwd()))
    
    filename = function() {
      paste('iat-', iatname, '.qsf', sep='')
    }
    
    
    qsfTemplate="SCTemplate_-_For_Shiny_V10.qsf"
    
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
    file.remove("SCTemplate_-_For_Shiny_V10.qsf")
    
    #remove HTML and JavaScript folders if QSF
    unlink(files[1], recursive = T)
    unlink(files[2], recursive = T)
    unlink(files[3], recursive = T)
    unlink(files[4], recursive = T)
  }

}


  
  


    










