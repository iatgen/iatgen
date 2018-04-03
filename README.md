iatgen (1.2.0)
==============

iatgen (pronounced "I A T gen") is an R package and Shiny App that
builds and analyzes Qualtrics surveys that contain IATs (Implicit
Association Tests; Greenwald et al., 1998) following a procedure
developed by Carpenter et al. (2018; preprint available at
<https://psyarxiv.com/6xdyj/>). Specifically, Carpenter et al. developed
procedures for configuring Qualtrics (<http://www.qualtrics.com>)
surveys such that they can run IATs and then developed
Qualtrics-friendly JavaScript and HTML code to implement a basic IAT in
Qualtrics. The R "iatgen" package was developed as a tool for
customizing and pasting this code into Qualtrics so that functional IAT
surveys can be rapidly built and analyzed.

#### What Exactly Does iatgen do?

First, iatgen is *not* a software tool for running IATs.

All IATs are run in and by Qualtrics (thus, Qualtrics is the "IAT
software"). This can be done without iatgen by adding custom HTML and
JavaScript to Qualtrics to implement an IAT. Anyone can produce such
JavaScript / HTML and there exist several custom HTML/JavaScript
variants of the IAT freely available online.

What Carpenter et al. (2018) did was identify the necessary conditions
to make a valid IAT survey in *Qualtrics* and developed JavaScript and
HTML variants optimized for this purpose. What iatgen does is (1)
automatically configure these files for the researcher and (2) copy them
into a Qualtrics survey template. Thus, the researcher does not need to
edit or configure multitudes of code files by hand as would be the case
without iatgen (risking errors, taking time).

In addition, iatgen provides a suite of data analysis tools for
processing the resulting data.

*Please note that the iatgen R package is licensed only for
non-commercial (e.g., academic) use under a Creative Commons (CC BY-NC
4.0) license. More details are provided under Licsense, below.*

Getting Started
---------------

#### Installation

iatgen can be installed on your computer using the `devtools` package.
You first need to install this package if you do not have it. In
addition, iatgen will use commands from the `stringr` package, so this
should be installed as well.

    install.packages("devtools")
    install.packages("stringr")

Next, iatgen can be installed using the `install_github()` command from
the `devtools` package:

    devtools::install_github("iatgen/iatgen")

#### Loading iatgen

iatgen can be loaded as normal with `library()`:

    library(iatgen)

#### Getting Help

That the primary functions in iatgen have built-in help documentation.
For example, detailed information on `writeIATfull()` can be obtained
with `?writeIATfull()`.

#### Shiny App

Users who do not wish to use the R package can use our Shiny web app,
which has the same features, at <https://applibs.shinyapps.io/iatui2/>

Building an IAT
---------------

A brief vignette and summary of IAT build features is provided; a more
detailed tutorial is found online at <https://osf.io/9w38r/>.

#### Words-Only Example

All iatgen IATs are run in Qualtrics. To build an IAT in Qualtrics,
users must (1) configure JavaScript and HTML files, (2) copy them into
Qualtrics, and (3) configure the Qualtrics files. This is done
automatically by iatgen, via the `writeIATfull()` function.

Within `writeIATfull()`, the user specifies several arguments. First,
the user specifies `IATname`, which becomes part of the file names for
the HTML and JavaScript files that are created. Note that if the
`qsf=TRUE` argument is set (as it is by default), these JavaScript and
HTML files will automatically be added to a Qualtrics survey template
and deleted; otherwise, they are added to the user's working directory
(saved as .txt files). A words-only IAT is specified as follows:

    writeIATfull(IATname="flowins",
                 posname="Pleasant", 
                 negname="Unpleasant",
                 Aname="Flowers",
                 Bname="Insects",
                 catType="words",
                 poswords = c("Gentle", "Enjoy", "Heaven", "Cheer", "Happy", "Love", "Friend"),
                 negwords = c("Poison", "Evil", "Gloom", "Damage", "Vomit", "Ugly", "Hurt"),
                 tgtType="words",
                 Awords = c("Orchid", "Tulip", "Rose", "Daffodil", "Daisy", "Lilac", "Lily"),
                 Bwords = c("Wasp", "Flea", "Roach", "Centipede", "Moth", "Bedbug", "Gnat"),
                 
                 #advanced options with recommended IAT settings
                 n=c(20, 20, 20, 40, 40, 20, 40),
                 qsf=T, 
                 note=T,
                 correct.error=T,
                 pause=250, 
                 errorpause=300, #not used if correct.error=TRUE
                 tgtCol="black",
                 catCol="green"
    )

As illustrated above, the positive category is named with the `posname`
argument; the negative category is named with the `negname` argument;
Target A is named with `Aname`; and Target B is named with `Bname`. The
categories and targets are set to words with the `catType` and `tgtType`
arguments set to words; following this, four vectors of word stimuli are
specified (`poswords`, `negwords`, `Awords`, and `Bwords`). These can
also be set to images (see below). In addition the number of trials per
block can be modified with the `n` argument (presently, must be even
numbers for non-combined blocks and divisible by 4 for combined blocks).
A note can be added to the bottom of the screen to remind people what
keys to press by setting `note=TRUE`. Participants can be forced to
correct errors by setting `correct.error=TRUE` (the default setting); if
`correct.error=FALSE` then an error message appear on the screen for
`errorpause=300` ms (or other value as input by the user). The the
researcher should carefully note this setting as it changes how the
analysis is done. Finally, the pause between trials can be modified from
`pause=250` milliseconds if desired. By default, the target stimuli are
colored `tgtCol="black"` and the category stimuli are `catCol="green"`.
Other colors may be used. Interested users can consult the built-in help
file for technical details with `?writeIATfull()`.

For more information, readers should consult our in-depth tutorials on
the Open Science Framework at <https://osf.io/9w38r/>.

#### Image-Based IATs

Targets, categories, or both can use images. Images should be sized 250
x 250 pixels in PNG format and hosted via the user's Qualtrics account
(tutorial at <https://osf.io/9w38r/>). Then, `tgtType` and/or `catType`
arguments are set to "images" (as appropriate), and
`poswords`/`negwords` are replaced with `posimgs`/`negimgs` and/or
`Awords`/`Bwords` are replaced with `Aimgs`/`Bimgs` (as appropriate).
The only difference between the word stimuli vectors and the image
vectors is that the image vectors are vectors of image URLs. For
stability reasons and on the basis of our own testing, we *strongly*
recommend users of images only host images on their *own* Qualtrics
accounts and follow the guidelines found in the tutorial referenced
above.

Because URLs are long, we recommend specifying vectors of images URLs in
advance and referencing them in the function call:

    goodjpg <- c("www.website.com/gentle.jpg",
                 "www.website.com/enjoy.jpg",
                 "www.website.com/Heaven.jpg",
                 "www.website.com/Cheer.jpg")

    badjpg <- c("www.website.com/Poison.jpg",
                "www.website.com/Evil.jpg.",
                "www.website.com/Vomit.jpg",
                "www.website.com/Ugly.jpg")

    Ajpg <- c("www.website.com/Orchid.jpg",
                 "www.website.com/Tulip.jpg",
                 "www.website.com/Rose.jpg",
                 "www.website.com/Daisy.jpg")

    Bjpg <- c("www.website.com/Wasp.jpg",
                "www.website.com/Flea.jpg",
                "www.website.com/Moth.jpg",
                "www.website.com/Bedbug.jpg")

    writeIATfull(IATname="flowins",
                 posname="Pleasant", 
                 negname="Unpleasant",
                 Aname="Flowers",
                 Bname="Insects",
                 catType="images",
                 posimgs = goodjpg,
                 negimgs = badjpg,
                 tgtType="images",
                 Aimgs = Ajpg,
                 Bimgs = Bjpg,
                 
                 #advanced options with recommended IAT settings
                 n=c(20, 20, 20, 40, 40, 20, 40),
                 qsf=T, 
                 note=T,
                 correct.error=T,
                 pause=250, 
                 errorpause=300, #not used if correct.error=TRUE
                 tgtCol="black",
                 catCol="green"
    )

#### The Qualtrics Survey

Detailed information about this Qualtrics survey is beyond the scope of
this document and is discussed in depth in the Carpenter et al. (2018)
preprint found at <https://psyarxiv.com/6xdyj/>.

Of note, however, is that (1) each IAT block is one question and (2)
there are four permutations of the IAT exist, counterbalancing the
left/right starting position for both Target A and the positive
category. Because each IAT consists of 7 blocks, these occupy 28 survey
questions (7 blocks x 4 permutations). These questions are named using
both the question number (Q1-Q28) and a 3-digit code identifying which
IAT permutation it comes from, based on the starting position of Target
A (RP = Target A starts right, initially paired with positive; RN =
starts left with negative; LP = starts left with positive; LN = starts
left with negative). Thus, “Q9 RN2” is the second block in the IAT where
Target A starts on the right side, initially paired with negative (i.e.,
incompatible block comes first). Researchers should carefully consult
our manuscript prior to use.

Analysis
--------

Once data are collected, iatgen can process the resultant data. Several
data-analysis scripts and a user tutorial are provided via
<https://osf.io/9w38r/>. However, a brief analysis vignette is provided
here.

In this vignette, users were not asked to correct errors and therefore
the "D600" algorithm is used. Note too that data must be in the *legacy*
Qualtrics CSV format. For data loaded to R, the row containing detailed
question information is also deleted (per usual, when working with
Qualtrics data). First, the data are loaded:

    #### LOAD THE IATGEN PACKAGE ####
    library(iatgen)

    #### READ YOUR DATA HERE AND SAVE IN R AS "DAT" ####
    dat <- read.csv("IAT Flowers Insects.csv", header=T)

To analyse the IAT, the data must be collapsed into four variables
representing practice/critical versions of the compatible and
incompatible blocks. At present, these are scattered across four
hard-coded permutations of the IAT representing left/right
counterbalancing of the starting positions (naming of these variables is
discussed above and in our manuscript at <https://psyarxiv.com/6xdyj/>).
The next step in the analysis is to collapse this down using
`combineIATfourblocks()`:

    ### Collapse  IAT data down ####
    dat$compatible.crit <- combineIATfourblocks(dat$Q4.RP4, dat$Q18.LP4, dat$Q14.RN7, dat$Q28.LN7)
    dat$incompatible.crit <- combineIATfourblocks(dat$Q7.RP7, dat$Q21.LP7, dat$Q11.RN4, dat$Q25.LN4)

    ### Collapse  IAT practice blocks ####
    dat$compatible.prac<- combineIATfourblocks(dat$Q3.RP3, dat$Q17.LP3, dat$Q13.RN6, dat$Q27.LN6)
    dat$incompatible.prac <- combineIATfourblocks(dat$Q6.RP6, dat$Q20.LP6, dat$Q10.RN3, dat$Q24.LN3)

Following this, the researcher runs `cleanIAT()`. In this case, the
researcher is careful to set an `error.penalty=TRUE` and
`error.penalty.ms=600` milliseconds, given that participants were not
forced to correct errors (making this the D600 algorithm; had
participants been forced to correct errors, this would have been
`error.penalty=FALSE`, making it the D-built.in.error.penalty
algorithm). This command is done and the result saved to an object for
further use, typically named `clean`.

    ### Clean the IAT ### 
    clean <- cleanIAT(prac1=dat$compatible.prac, 
                      crit1=dat$compatible.crit, 
                      prac2=dat$incompatible.prac, 
                      crit2=dat$incompatible.crit, 
                      
                      timeout.drop=TRUE, 
                      timeout.ms=10000, 
                      
                      fasttrial.drop=FALSE, 
                      
                      fastprt.drop=TRUE, 
                      fastprt.percent=.10, 
                      fastprt.ms=300, 
                      
                      error.penalty=TRUE, 
                      error.penalty.ms=600)

There are a few things to note in the `cleanIAT()`. First, the first
four arguments (`prac1`, `crit1`, `prac2`, and `crit2`) represent the
practice and critical versions of the compatible and incompatible
blocks, respectively (see above). In addition, following Greenwald et
al. (2003)'s *D*-score algorithm, we have set a timeout such that trials
above 10,000 *ms* are removed (`timeout.drop=TRUE`, `timeout.ms=10000`).
We have not set individual fast trials to be removed in the same manner
(`fasttrial.drop=FALSE`) and instead follow the Greenwald et al. (2003)
procedure of removing all data from participants who have more than 10%
of responses under 300 ms (`fastprt.drop=TRUE`, `fastprt.percent=.10`,
`fastprt.ms=300`). Finally, as noted above, we are adding an error
penalty in analysis of 600 *ms* because participants were not required
to correct errors (`error.penalty=TRUE`, `error.penalty.ms=600`).

The `clean` object is a list containing *many* things For detailed
information see the built-in help file (`?cleanIAT()`). We focus on a
few here.

First, the number of participants who completed the IAT using
`$skipped`, a logical vector indicating whether each person completed an
IAT or not:

    ### NUMBER OF PARTICIPANTS WHO COMPLETED THE IAT ###
    sum(!clean$skipped)

    ## [1] 201

We see here that 201 people (which was the sample size) submitted a
completed IAT for analysis.

Next, we can see the proportion of trials dropped due to exceeding
10,000 ms (as specified in our function call, above) with
`$timeout.rate`:

    ### TIMEOUT DROP RATE (% of TRIALS) ###
    clean$timeout.rate

    ## [1] 0.001285347

As we see here, it is 1/10 of 1% of trials...a very small amount.

Next, we can get diagnostics on the number of participants dropped due
to overly fast responses with `$fastprt.count` and `$fastprt.rate`:

    ### FAST PARTICIPANT 'BUTTON MASHER' DROP COUNT AND RATE (% of SAMPLE) ###
    clean$fastprt.count

    ## [1] 13

    clean$fastprt.rate

    ## [1] 0.06467662

We see this is 13 participants, or approximately 6% of the sample.

How accurate were our remaining participants? This is given with
`$error.rate`:

    ### ERROR RATE ###
    clean$error.rate

    ## [1] 0.07467388

We are less than 10%, which is considered typical for an IAT.

We can estimate reliability using a procedure described in the Carpenter
et al. (2016) manuscript and De Houwer and De Bruycker (2007) using the
`IATreliability()` command. This returns a number of things, including
`$reliability` which is a split-half reliability estimate:

    ### RELIABILITY ANALYSIS ###
    IATreliability(clean)$reliability

    ## [1] 0.8058219

We see this IAT is estimated at .80, which is great for an IAT.

Next, we can examine the scores. The IAT scores are stored as `$D`. It
is common to put them back into one's datafile, but they can also be
saved and exported to other software (e.g., SPSS; they will line up with
the rows of the source datafile.) A positive score indicates one had a
preference for the compatible block:

    # place back into dat
    dat$D <- clean$D

    # test for implicit bias
    mean(clean$D, na.rm=T)

    ## [1] 0.6137453

    sd(clean$D, na.rm=T)

    ## [1] 0.3622714

    t.test(clean$D)

    ## 
    ##  One Sample t-test
    ## 
    ## data:  clean$D
    ## t = 23.229, df = 187, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  0.5616230 0.6658675
    ## sample estimates:
    ## mean of x 
    ## 0.6137453

    #cohen d
    mean(clean$D, na.rm=T) / sd(clean$D, na.rm=T)

    ## [1] 1.694159

Here we see that the mean IAT score was *M* = 0.61, *SD* = 0.36,
*t*(187) = 23.23, *p* &lt; .001, 95% CI \[0.56, 0.67\], *d* = 1.69. This
represents a rather large implicit preference for flowers over insects.

At this point, these responses could be exported to excel with a command
such as `write.csv()` and pasted into another program such as SPSS
(NOTE: user may need to delete `NA` text in missing responses prior to
pasting into SPSS):

    write.csv(clean$D, "iatOUTPUT.csv")

At this point, these *D*-scores can be correlated with other measures or
otherwise analyzed. If uses wish to report the block means by
participant, these can be found as well:

    ### RT DESCRIPTIVES BY BLOCK
    mean(clean$clean.means.crit1, na.rm=T)

    ## [1] 882.2707

    mean(clean$clean.means.crit2, na.rm=T)

    ## [1] 1065.307

    mean(clean$clean.means.prac1, na.rm=T)

    ## [1] 899.6229

    mean(clean$clean.means.prac2, na.rm=T)

    ## [1] 1165.644

    sd(clean$clean.means.crit1, na.rm=T)

    ## [1] 230.9385

    sd(clean$clean.means.crit2, na.rm=T)

    ## [1] 253.1042

    sd(clean$clean.means.prac1, na.rm=T)

    ## [1] 221.7494

    sd(clean$clean.means.prac2, na.rm=T)

    ## [1] 293.3292

License
-------

The iatgen R package is licensed only for *non-commercial (e.g.,
academic) use* under a
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative
Commons Attribution-NonCommercial 4.0 International License</a> (CC
BY-NC 4.0). The tool was created with the intent of making the IAT
accessible to academic researchers who use Qualtrics for online
research.

No warranty is offered and the licensor assumes no liability of any kind
for any consequences that may result from using the iatgen R package.
This tool can be modified and distributed with attribution to us, but
cannot be used for commercial purposes. More details are provided in the
full text of the license.

Although we believe the IAT can be validly run via Qualtrics (e.g., as
set up via iatgen), the official IAT software remains Project Implicit
(<http://implicit.Harvard.edu>) and any software endorsed by the IAT's
creators. Although the use of Qualtrics as an IAT tool has been
validated by Carpenter et al. (2018), this procedure and its code is not
provided or endorsed by the IAT's creators. We hold no copyright to the
IAT itself. We are extremely grateful to the IAT's creators, especially
Tony Greenwald, for inspiring a cohort of young scientists such as
ourselves to study implicit biases and understand why people do, think,
and feel what they do.

<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />This
work is licensed under a
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative
Commons Attribution-NonCommercial 4.0 International License</a>.

Authors
-------

The iatgen package was built and maintained by Tom Carpenter
(<tcarpenter@spu.edu>), Michal Kouril, Ruth Pogacar, and Chris Pullig.
An early prototype of the HTML and JavaScript were built by Aleksandr
Chakroff. Questions regarding iatgen should be directed to Tom
Carpenter.

Acknowledgments
---------------

We would like to express our profound gratitude to Tony Greenwald and
all other IAT scholars who have come before for inspiring our interest
in this project. We also thank Jordan LaBouff and Stephen Aguilar for
contributing validation data and to Naomi Isenberg for help setting up
our website and user tutorials.
