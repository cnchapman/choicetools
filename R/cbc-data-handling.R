# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# CBC data handling and creation routines (other than import-export!)


# FUNCTIONS:
#   generateMNLrandomTab()  : Create a design matrix from a list of attributes
#   pickMNLwinningCards()   : Given utilities, choose the winning tasks from design matrix
#   generateRNDpws()        : Create random utilities for simulated respondents
#   fillCBCwinners()        :
#   expandCBCwinners()      :


#############################################
# generateMNLrandomTab()
#############################################
# function(attrLevels, cards=3, respondents=200, trials=12,
#          balanced.sample=TRUE, best.of=100, verbose=TRUE, no.output=FALSE)
#
# Create a "TAB" style CBC design
#
# WARNING: for didactic & simulation rather than survey purposes. Designs are
#   slightly optimized for level balance, but not for other concerns such as
#   D-efficiency and optimal balanced overlap.
#
# PARAMETERS
#   attrLevels  = list of how many levels to create for each attribute
#                 (length = # of attributes; each entry = # of levels)
#   cards       = concepts shown per CBC task
#   respondents = how many sets of trials to generate
#   trials      = how many trials to generate for each "respondent"
#   balanced.sample = whether to minimize reuse of concept levels
#             TRUE  = No overlap within trials (if possible). Do not resample
#                     a level within attribute in a trial
#             FALSE = Just generate cards randomly with level resampling
#                     according to uniform distribution
#   best.of     = how many designs to evaluate; function will return the single
#                     best-balanced design among them
#   verbose     = show output as it goes
#   no.output   = whether to run silently
#
# NOTE:
# in v0.2, all respondents must have the same number of cards and trials
# see "estimateMNLfromDesign()" for more details
#
# SAMPLE CODE
# a design with 10 attributes of 3-7 levels
#   tmp.attrs  <- c(4,3,4,5,5,3,4,5,2,7)
# create random CBC cards for that design
#   tmp.tab    <- generateMNLrandomTab(tmp.attrs,respondents=200,trials=8)
# convert those to a design-coded dummy matrix
#   tmp.des    <- convertSSItoDesign(tmp.tab)
#

generateMNLrandomTab <- function(attrLevels, cards=3, respondents=200,
                                 trials=12, balanced.sample=TRUE, best.of=50,
                                 verbose=TRUE, no.output=FALSE)
{
  cat("Searching for a balanced design ...\n")
  # create a matrix to hold the result
  rnd.tab <- matrix(0,ncol=length(attrLevels),nrow=cards*respondents*trials)
  # and figure the starting point for a "best" design
  att.base <- 1/rep(attrLevels,attrLevels)        # ideal balance proportions
  hold.mss <- sum(att.base^2)

  # and one to hold trials along the way
  rnd.trial <- matrix(0,ncol=length(attrLevels),nrow=cards*respondents*trials)

  for (i in 1:best.of)
  {
    # generate a candidate trial
    # try to balance the attribute levels ?
    if (balanced.sample) {
      # figure out which attr levels can be balanced and which can't
      which.balanced <- which(attrLevels >= cards)
      which.replace  <- which(attrLevels < cards)

      # generate the columns that can be balanced
      # (sample without replacement from attribute levels)
      if (any(which.balanced)) {
        # generate balanced sets for every combination where possible
        for (ii in 1:(respondents*trials))
        {
          samp.row <- sapply(attrLevels[which.balanced],
                             sample, cards, replace=FALSE)
          rnd.trial[((ii-1)*cards+1):(ii*cards), which.balanced] <- samp.row
        }
      }

      # and now the columns that can't balance (sample with replacement)
      if (any(which.replace)) {
        samp.replace <- sapply(attrLevels[which.replace],
                               sample, cards*trials*respondents,replace=TRUE)
        rnd.trial[,which.replace] <- samp.replace
      }
    } else {
      # just generate them all randomly
      rnd.trial <- sapply(attrLevels,
                          sample, cards*trials*respondents, replace=TRUE)
    }
    # is the candidate better than the previous candidate ?
    # convert it to a design matrix
    new.des  <- convertSSItoDesign(as.data.frame(rnd.trial), no.output=TRUE)
    # calculate how much it differs from the ideal and compare to held trial
    new.mss  <- sum((colMeans(new.des)-att.base)^2)
    if (new.mss < hold.mss) {
      rnd.tab <- rnd.trial
      hold.mss <- new.mss
      if (verbose & (i>1)) {
        cat("Improved design found on trial: ", i ," SSE = " , new.mss , "\n")
      }
    }
  }
  rnd.tab <- data.frame(rnd.tab)
  if (!is.null(names(attrLevels))) {
    names(rnd.tab) <- names(attrLevels)
  }
  return(rnd.tab)
}


#############################################
# pickMNLwinningCards
#############################################
# function(design.mat, pws=rep(0, ncol(design.mat)), cards=3,
#          verbose=TRUE, vec.format="WIN")
#
# Given a design matrix and vector of part worths, returns a vector of which
#   card wins each comparison set
# NOTE: In v0.2, this function is primarily for test purposes -- it only uses
#   aggregate level part worths, plus optional random noise
#
# PARAMETERS
#   design.mat  : the design matrix in dummy-coded (0/1) format
#   pws         : list of part worths to use
#                 NOTE: currently uses aggregate-level PWs only
#   cards       : number of cards per trial
#   noise       : add randomness into the choices?
#   p.noise     : fraction of choices that will be noisy, if noise==TRUE
#   verbose     : output more as it goes
#   vec.format  : format of the output, either "WIN" == 0/1 | "ANS" = 1,2,3, etc
#
# OUTPUT
#  a vector of the winning cards for the design

pickMNLwinningCards <- function(design.mat, pws=rep(0,ncol(design.mat)),
                                cards=3, noise=FALSE, p.noise=0.3,
                                use.MNL=TRUE,                         ## to do -- choose according to MNL roulette
                                verbose=TRUE, vec.format="WIN")
{
  # how many sets of comparisons are there?
  n.trial <- nrow(design.mat)/cards
  vec.win <- rep(0,n.trial)
  ## pws.mat <- as.matrix(pws)      ##  ** TO DO: handle vector OR a matrix
  pws.mat <- pws

  # iterate over every set of comparisons and find the winner
  # ** would be nice to vectorize this -- [chris] see preference share prediction routines in portfolio model code
  for (i in 1:n.trial)
  {
    card.mat <- design.mat[((i-1)*cards+1):(i*cards),]
    card.util <- exp(pws.mat %*% t(card.mat))
    which.win <- which(card.util==max(card.util))
    if (noise) {
      if (runif(1) < p.noise) {
        which.win <- sample(1:cards, 1)
      }
    }
    if (length(which.win) > 1) {
      which.win <- sample(which.win, 1)  # choose among multiple winners randomly
    }
    vec.win[i] <- which.win
    if (verbose) {
      if (i/2000 == floor(i/2000)) {
        cat("Processing trial: ",i,"\n")
      }
    }
  }
  # expand this to the desired format (WIN==1/0 format by card; ANS=card number)
  if (vec.format == "WIN") {    # cards marked 1/0 according to whether they won or not
    vec.tmp <- ifelse(rep(vec.win,each=cards)==rep(c(1:cards),n.trial),1,0)
    vec.win <- vec.tmp
  } else if (vec.format == "ANS") {
    vec.tmp <- rep(vec.win,each=cards)
    vec.win <- vec.tmp
  } else {
    warning("Parameter 'vec.format' specified incorrectly (should be 'WIN' or 'ANS'). Raw card list returned (1 card per set).")
  }
  return(vec.win)
}


#############################################
# generateRNDpws
#############################################
# function(attrs)
#
# given a vector of attributes/features, create random zero-sum part worths
#
# PARAMETERS
#    attrs : a vector where each element represents how many levels there are for the corresponding attribute
#            e.g.,  c(2,3,2) would be 3 attributes, with 2 levels, 3 levels, and 2 levels respectively
#    mu    : mean of the randomly drawn part-worths, if you want to ensure some of them are far from zero
#            of course the return vector will still be zero-sum
#    stdev : sd of the randomly drawn part-worths, if you want to ensure some different scale factor
#
# OUTPUT
#    a vector of zero-centered (within attribute) random normal partworths (mean=mu, sd=stdev)
#
generateRNDpws <- function(attrs, mu=0, sdev=1)
{
  pw.vec <- NULL
  for (i in 1:length(attrs))
  {
    if (attrs[i] > 1) {                 # more than 1 feature so OK to generate
      pw.car <- rnorm(attrs[i]-1, mu, sdev)
      pw.cdr <- -1*sum(pw.car)        # the final pw needed to make zero-sum
      pw.vec <- c(pw.vec, sample(c(pw.car, pw.cdr)))  # shuffle and add to list
    } else if (attrs[1] < 1) {          # attribute with <1 level
      warning("Attribute level is missing (# features < 1).")
    } else {                            # exactly 1 level so PW must be 0
      pw.vec <- c(pw.vec, 0)
    }
  }
  return(pw.vec)
}


#########################
# fillCBCwinners()
#########################
#
# utility function. In a vector of CBC choices, replaces any NAs with random choices
#
# PARAMETERS
# 	win.cards = vector of CBC winning cards, e.g., c(1,3,2,3,2,1,1,3,2...)
# OUTPUT
#   vector with NA values replaced by random choices in [1..max(win.card)]
#
fillCBCwinners <- function(win.cards) {
  if (any(!is.na(win.cards))) {
    win.cards[is.na(win.cards)] <- sample(max(win.cards, na.rm=T), sum(is.na(win.cards)), replace=TRUE)
  } else {
    warning ("All cards in fillCBCwinners are NA. Assuming cards per trial = 3.")
    win.cards[is.na(win.cards)] <- sample(3, sum(is.na(win.cards)), replace=TRUE)
  }
  return(win.cards)
}


#########################
# expandCBCwinners()
#########################
#
# utility function. In a vector of CBC choices, replaces any NAs with random choices
#
# PARAMETERS
# 	win.cards = vector of CBC winning cards, e.g., c(1,3,2,3,2,1,1,3,2...)
#   cards     = how many cards per trial
#   fill      = whether to replace NA values (missing responses) with random responses
# OUTPUT
#   vector marking each concept as 0/1 for lost/won, of length (cards*length(win.cards))
#
expandCBCwinners <- function(win.cards, cards=3, fill=TRUE) {
  if (fill) {
    win.cards <- fillCBCwinners(win.cards)
  }
  win.exp <- rep(0, length(win.cards)*cards)    # vector to hold card winners. by default all choices are "no"
  win.ind <- seq(from=0, to=length(win.exp)-1, by=3) + win.cards     # the 1/0 location of just the winning cards
  win.exp[win.ind] <- 1   # set the winners
  return(win.exp)
}
