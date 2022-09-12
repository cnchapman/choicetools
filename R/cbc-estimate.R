# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# CBC estimation routines

# FUNCTIONS
#   marketSim()                 : Estimate product preference from design + utilities
#   estimateMNLfromDesignHB()   : Hierarchical Bayes estimation of aggregate+individual level utilies
#   extractHBbetas()            : convenience function to extract utility estimates
#   estimateMNLfromDesign()     : Use NR iterative estimation to find aggregate level solution


############################
# marketSim(data, prod.definitions, none.col, use.none, tuning, draws, n.resp, draws.each, use.error)
#
# "Market simulation" routine:
# Find the preference for each product vs. others in a list of defined products,
# optionally including the "none" product
#
# Useful to do your own market simulations, or as target function in an optimization routine (e.g., to optimize observed preference share)
#
# inputs
#     pw.data = matrix of part-worth utility DRAWS by respondent
#               !! could be defined as a "bigmemory" matrix object for draws -- should work OK passed as bigmemory() or matrix() or data.frame()
#     prod.defs = list defining each product as a vector of column numbers
#     none.col = the column that holds the "none" part-worth (if applicable)
#     use.none = whether to compare preference to "none"
#     tuning = multiplier for partworths to handle scale factor
#     draws = number of draws to make from HB draw matrix -- the function will return the average across multiple bootstraps if >1
#               only meaningful if using a draw file with multiple estimation draws per respondent
#     n.resp = the actual number of respondents (will be multiplied by some constant if using HB draws, e.g., x1000, otherwise = nrow()
#     draws.each = how many replicants draws there are for each respondent (e.g., 1000)
#               matrix must be structured with respondents in order, in identically sized blocks of draws (the default from most HB saves)
#   `           e.g., 1000 rows for resp 1, then 1000 rows for resp 2, and so forth
#     use.error = whether to include Gumbel error perturbation of part worths when making calculations (FALSE by default)
#               NOTE that this includes EV error added for both ATTRIBUTE level (added to betas) and PRODUCT level (added to product sum)
#               unless you change the default of the following switches
#     use.attr.error = whether to add attribute-level EV error (i.e., to betas before product summation) (ON by default but only if use.error = TRUE)
#     use.prod.error = whether to add product-level EV error (i.e., EV error added per product after betas are summed) (ON by default but only if use.error = TRUE)
#     style  = how to figure preference:
#           logit => calculate share of preference using logit rule for each respondent and return raw preference estimates.
#                    Note: logit shares have well-known IIA problem.
#           first => calculate logit share as above, then convert to 1/0 matrix for single most-preferred option (first choice).
#                    Immune to IIA problem (but not necessarily therefore more "correct").
#           roulette => do logit shares, then draw 'preferred' product with probability == its share of logit preference.
#                    Not immune to IIA. Also RANDOM -- results are not deterministic.
#           rouletteFC => TBD
#
# Example call:
#     For 2 products defined as:
#         Product 1 = columns 1,4,8
#         Product 2 = columns 1,5,9
#     and including the none part worth in column 11
# marketSim(my.partworth.data,list(c(1,4,8),c(1,5,9)),11,TRUE)

marketSim <- function(pw.data, prod.defs, none.col=NA, use.none=FALSE, tuning=1.0,
                      draws=1, n.resp=nrow(pw.data), draws.each=1,
                      use.error=FALSE, use.attr.error=TRUE, use.prod.error=TRUE, style="logit")
{
  total.matrix <- NULL
  for (i in 1:draws) {

    # take a sample from respondent draws in pw.data
    draw.sample <- rep(sample(1:draws.each, 1), n.resp)      # choose an HB draw to use, and make a vector of that to pull sample for each respondent
    row.sample  <- draw.sample + ((1:n.resp)-1) * draws.each     # which rows within the dataframe to sample (indexing to each respondent and the pulled draw)
    pws.use <- pw.data[row.sample,]                      # sample those from the pw.data

    all.sum <- NULL
    all.prematrix <- matrix(NA,nrow=nrow(pws.use),ncol=(length(prod.defs)+ifelse(use.none,1,0)))    # pre-define matrix to hold results
    all.matrix <- NULL

    # add attribute-level error to part worths (to be used across all products)
    if (use.error && use.attr.error) {                                        # add attribute-level product error for every Beta estimate in the matrix
      error.mat <- -log(-log(matrix(runif(nrow(pws.use)*ncol(pws.use)), nrow=nrow(pws.use), ncol=ncol(pws.use))))
      pws.use <- pws.use + error.mat
    }

    # iterate over the list of products defined and save the sum of each one's part worths (beta)
    ii <- 0
    for (prod.def in prod.defs) {
      ii <- ii + 1
      product.sum <- rowSums(pws.use[,prod.def]*tuning)
      all.prematrix[,ii] <- product.sum
    }

    # optionally add the value of the "none" choice
    if (use.none) {
      all.prematrix[,ii+1] <- pws.use[,none.col]*tuning
    }

    # generate Gumbel extreme value error at the summed PRODUCT level, and add it
    #
    if (use.error && use.prod.error) {
      # create matrix of EV error terms with same shape as product choice matrix
      error.mat <- -log(-log(matrix(runif(nrow(all.prematrix)*ncol(all.prematrix)),nrow=nrow(all.prematrix),ncol=ncol(all.prematrix))))
      # utility = exp(B + error)
      all.prematrix <- all.prematrix + error.mat    # now have utility by product
    }
    all.matrix <- exp(all.prematrix)                  # now have e^(utility[+error]) for every product

    # compute the total utility of all choices per respondent
    all.sum <- rowSums(all.matrix)
    # and return the shares
    if (is.null(total.matrix)) {
      total.matrix <- (all.matrix / all.sum)
    } else {
      if ( (dim(total.matrix)[1] != dim(all.matrix)[1]) |
           (dim(total.matrix)[2] != dim(all.matrix)[2]) |
           (dim(total.matrix)[1] != length(all.sum) ) )
      {
        print("Warning: dimensions don't match in prod.select.ice()")
        print(dim(total.matrix))
        print(dim(all.matrix))
        print(length(all.sum))
      }
      total.matrix <- total.matrix + (all.matrix / all.sum)
    }
  }
  tmp.ret <- total.matrix/draws
  if (style=="logit") {
    ## nothing to do -- this is the default
    ## just return the matrix of logit utilities as estimated
  } else if (style == "first") {
    ## return the 'first choice' preference as 0 or 1 calculated from the logit shares
    tmp.pref <- matrix(0,nrow=nrow(tmp.ret),ncol=ncol(tmp.ret))  ## set up a matrix of all 0's to indicate unpreferred options
    tmp.pref[cbind(1:nrow(tmp.ret),max.col(tmp.ret))] <- 1       ## set most-preferred option to '1'. Ties are broken randomly in max.col()
    tmp.ret <- tmp.pref
  } else if (style == "roulette") {
    ## return single preferred option but draw it from all the products according to their relative likelihoo
    tmp.pref <- matrix(0,nrow=nrow(tmp.ret),ncol=ncol(tmp.ret))  ## set up a matrix of all 0's to indicate unpreferred options
    tmp.which <- apply(tmp.ret,1,function(x) { sample(1:length(x),1,prob=x) } )   ## draw list of columns sampled according to probabilities in each row
    tmp.pref[cbind(1:nrow(tmp.ret),tmp.which)] <- 1       ## set the drawn option to '1'
    tmp.ret <- tmp.pref
  } else {
    warning("Undefined 'style' parameter. Use 'logit','first', or 'roulette'. Returning 'logit' shares by default.")
  }
  return(tmp.ret)
}



#########################
# estimateMNLfromDesignHB()
#########################
#
# Uses ChoiceModelR to estimate HB model for a design + winning cards.
#
# This function is a convenience to make choicemodelr (and bayesm) easier to
# use when you have Sawtooth Software CBC files or similar data.
#
# NOTE:
# currently this REQUIRES the input matrix to be perfectly rectangular!
#   i.e., same number of cards & trials for every respondent
#   as is typical of output files from Sawtooth SSI/Web CBC.
# If that is not true in your case, then:
#   If your design is rectangular, then pad the winners vector to fill
#     unobserved choices with noise (function expandCBCwinners(fill=TRUE))
#   If you design is a list, different per respondent, then:
#     use the code below as a model to call choicemodelr() directly
#
# PARAMETERS
#   tmp.des    : design matrix, rectangular for respondent*trial*concept
#   tmp.win    : vector of 1/0 winning observations per tmp.des rows
#   kCards     : cards (concepts) shown on each trial
#   kTrials    : number of trials per respondent
#   kResp      : number of respondents in design file
#   mcmcIters  : iterations to run the MCMC chain (can be very slow)
#   pitersUsed : proportion of iterations to use in the final draws that are
#                saved for respondents. Default is to use the final 1000 draws,
#                and sampling every "drawKeepK=10", resulting in 100 draws per
#                respondent. Set pitersUsed to be higher if you want more,
#                e.g., pitersUsed=0.1 to sample from the final 10% of draws.
#   drawKeep   : whether to save the draws. Usually a good idea to do so, since
#                the whole point of HB is individual-level estimates
#   drawKeepK  : the interval for keeping draws. Default=10 means to retain
#                every 10th draw from the final proportion "pitersUsed" of the
#                total mcmcIters chain.
#
# OUTPUT
#   a list from choicemodelr with draws and parameters. see choicemodelr for
#   documentation
# WARNING
#   functionality for "none" estimation is pending. See ?choicemodelr and use this
#   code as a model if you need to investigate none parameter specifically.
#

estimateMNLfromDesignHB <- function(tmp.des, tmp.win,
                                    kCards=3, kTrials=8, kResp=200,
                                    mcmcIters = 10000, pitersUsed=1000/mcmcIters,
                                    drawKeep = TRUE, drawKeepK = 10, none=FALSE) {
  require("ChoiceModelR")
  tmp.ids <- rep(1:kResp, each=kCards*kTrials)         # respondent IDs
  tmp.set <- rep(rep(1:kTrials, each=kCards), kResp)   # trial set within respondent ID
  tmp.seq <- rep(1:kCards, kResp*kTrials)              # alternative within trial set

  # assign the winning card for each trial in the correct format (first line in set lists the winning entry)
  tmp.wincols <- max.col(matrix(tmp.seq*tmp.win, ncol=kCards, byrow=T))
  tmp.win2 <- rep(0, length(tmp.win))
  tmp.win2[seq(from=1, to=length(tmp.win), by=kCards)] <- tmp.wincols   # put winning card number into first row of each trial set

  # set up ChoiceModelR parameters
  tmp.des2 <- cbind(tmp.ids, tmp.set, tmp.seq, tmp.des, tmp.win2)    # ids, design, and winners in ChoiceModelR format
  tmp.coding <- rep(0, ncol(tmp.des))                                         # 0 = categorical coding for the attribute
  tmp.mcmc <- list(R = mcmcIters, use = mcmcIters*pitersUsed)
  tmp.opt <- list (none=none, save=drawKeep, keep=drawKeepK)

  # be sure to display graphics window to see convergence plot
  # ... and run it!
  cmr.out <- choicemodelr(data=tmp.des2, xcoding=tmp.coding, mcmc=tmp.mcmc, options=tmp.opt,
                          directory=getwd())

  return(cmr.out)
}

#########################
# extractHBbetas()
#########################
#
# utility function to reshape choicemodelr beta draws into the familiar,
# sum-to-zero shape with 1 column per every attribute level
#
# WARNING: needs debugging to double-check vs. real data  **
#
# INPUT
#   tmp.cmrlist : an object from estimateMNLfromDesignHB() / choicemodelr
#   attr.list   : vector with your list of attribute sizes (levels)
# OUTPUT
#   a matrix with mean betas per respondent * attribute level
#   padded to be zero-summed per standard part worths
#

extractHBbetas <- function(tmp.cmrlist, attr.list) {
  # figure out where the columns start and end without and with zero-sum PWs
  from.ends <- cumsum(attr.list-1)
  from.starts <- c(1, from.ends+1)
  to.ends <- cumsum(attr.list)-1
  to.starts <- c(1, to.ends+2)

  # create a matrix to hold all the answers
  tmp.betas <- matrix(0, ncol=sum(attr.list), nrow=dim(tmp.cmrlist$betadraw)[1])

  # iterate over all the attributes and fill out the zero-sum matrix
  for (i in 1:length(from.ends)) {
    # get the slice of columns that represent a particular attribute's levels
    # and find the per-respondent means across the draws
    if(to.ends[i] > to.starts[i]) {
      tmp.slice <- apply(tmp.cmrlist$betadraw[ , from.starts[i]:from.ends[i], ],
                         c(1,2), mean)
      tmp.slicesum <- apply(tmp.slice, 1, sum)
    } else {
      tmp.slice <- apply(tmp.cmrlist$betadraw[, from.starts[i], ], 1, mean)
      tmp.slicesum <- tmp.slice
    }
    tmp.betas[, to.starts[i]:to.ends[i]] <- tmp.slice
    tmp.betas[, to.ends[i]+1] <- -1.0 * tmp.slicesum
  }
  return(tmp.betas)
}

#############################################
# estimateMNLfromDesign()
#############################################
# function(df.in, cards.win, start.pws=rep(0,ncol(df.in)),
#          stop.limit=1e-10, stop.iters=200, verbose=FALSE)
#
# input:  a DESIGN-CODED matrix with winning cards marked
# output: Multinomial logit model partworths on per-attribute basis
#         should be nearly-identical to SSI SMRT "Logit run" utilities
#
# Calculates partworths using maximum-likelihood estimation and gradient descent
# General algorithm:
#    Set initial partworths (all 0 by default)
#    Estimate probabilities of observed choices using those partworths
#    Compute difference from observed choices (i.e., choices with p=1 or p=0)
#    Update the partworths in the direction of the observed choices
#    Repeat until deviation from best estimate is very small
#
# Parameters
#   df.in: Design-coded matrix of attribute levels
#          can be derived from SSI SMRT "TAB" output using the
#          "convertSSItoDesign()" function above
#   cards.win: 1/0 coded winner for each line.
#   start.pws: starting partworths for estimation.
#              Not necessary; starting from all 0 (default) usually works fine
#   stop.limit: point at which to stop estimation, when MSS of differences in
#              prob are smaller than this limit (default is usually OK)
#   stop.iters: maximum number of iteration steps
#   verbose: show estimation steps while running
# Output
#   a list consisting of
#      [[1]] the MSS at stopping point of the algorithm
#      [[2]] the estimated partworths in order of the attribute levels
#
# **************
# **** NOTE ****
#    Assumes identical number of cards/trial, and identical trials/respondent.
#        i.e., 3 concepts * 8 trials = 24 lines in design for every respondent
#    That is typical of CBC data from Sawtooth SSI/Web CBC.
#
#    WARNING: NOT DESIGNED or TESTED for unequal numbers of cards or tasks.
#        In such cases, there are three options:
#          1. pad the responses with unbiased random responses
#          2. use the estimateMNLfromDesignHB estimation instead
#          3. use a commercial CBC package!
# **************
# **************
#
# Sample code that runs directly
#   Create a TAB-coded matrix (same format as the attribute columns in SMRT TAB)
#     tab format for 2 attributes * 3 trials of 3 concepts (9 cards total):
#     att.list <- data.frame(cbind(c(1,2,3,3,1,3,1,2,1), c(1,2,3,2,3,3,1,2,1)))
#   Convert that to design coded matrix
#     winning cards for those 3 trials (e.g., 1,2,1).  from TAB output:
#      my.ca <- convertSSItoDesign(att.list, cards.win=c(1,1,1,2,2,2,1,1,1))
#          ALTERNATIVELY, can just pass the last column in TAB file coded 1/0
#   Determine whether each card was the winner or not
#      was each card the winner or not? :
#      my.ca.wins <- my.ca[,7]==rep(c(1,2,3),3)
#        ALTERNATIVELY, use the last column from TAB file (already coded 1/0)
#        -- in which case skip this; send that vector to "estimateMNLfromDesign"
#   Estimate MNL model
#      estimateMNLfromDesign(my.ca[ ,1:6], my.ca.wins)
#
# SAMPLE code that uses an SMRT "TAB" file (must modify for your own file):
#   read the TAB file:
#   usm0.tab <- read.csv("./somedir/somefile.tab")
#                          ^^^^^ match your TAB file from SMRT
#   convert attribute columns + coded winners to dummy-coded attribute levels:
#   usm0.des <- convertSSItoDesign(usm0.tab[,15:24])
#                                             ^^ match your file's attributes
#   pass in all of those levels + the winning cards dummy coded:
#   usm0.logit <- estimateMNLfromDesign(usm0.des[,1:45], usm0.tab[,26])
#                                                   ^^            ^^
#     match your file's number of total attribute levels and winning cards vector

estimateMNLfromDesign <- function(df.in, cards.win, cards=3,
                                  start.pws=rep(0,ncol(df.in)),
                                  stop.limit=1e-10, stop.iters=200,
                                  verbose=TRUE, no.output=FALSE)
{
  # set "previous pass" results and counter to test against in while() loop
  mnl.ssdiff <- 1
  mnl.sslast <- 1
  mnl.iters <- 0

  # set initial partworth estimates
  mnl.pws <- start.pws

  # make sure cards (concepts shown per trial) is in acceptable range
  if (cards < 2 || cards > 5) {
    warning("Error. Number of concepts per trial is out of range (2-5).\n")
    return()
  }

  # main MLE estimation loop
  # run until condition met for little improvement OR maximum loop iterations
  while ((mnl.ssdiff > stop.limit) && (mnl.iters < stop.iters))
  {
    # figure attribute level values according to current partworth estimates
    mnl.consum <-  mnl.pws %*% t(df.in)
    mnl.conexp <- exp(mnl.consum)

    # add those up to get the sum per concept shown
    # NOTE: hard-coded to 2-5 CONCEPTS per (every) trial
    if (cards==2) {
      mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] +
        mnl.conexp[seq(2, nrow(df.in), by=cards)]
    } else if (cards==3) {
      mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] +
        mnl.conexp[seq(2, nrow(df.in), by=cards)] +
        mnl.conexp[seq(3, nrow(df.in), by=cards)]
    } else if (cards==4) {
      mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] +
        mnl.conexp[seq(2, nrow(df.in), by=cards)] +
        mnl.conexp[seq(3, nrow(df.in), by=cards)] +
        mnl.conexp[seq(4, nrow(df.in), by=cards)]
    } else if (cards==5) {
      mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] +
        mnl.conexp[seq(2, nrow(df.in), by=cards)] +
        mnl.conexp[seq(3, nrow(df.in), by=cards)] +
        mnl.conexp[seq(4, nrow(df.in), by=cards)] +
        mnl.conexp[seq(5, nrow(df.in), by=cards)]
    } else {        ## should not occur in most CBC studies
      if (!no.output) cat("Error. Cards out of range (2-5).\n")
      warning("Error. Cards out of range (2-5).\n")
      return()
    }
    # match the vector length
    mnl.extsum <- rep(mnl.trialsum, each=cards)

    # estimate probability of each card being chosen according to MNL model
    mnl.trialp <- mnl.conexp / mnl.extsum
    mnl.estp <- apply(df.in, 2, "*", mnl.trialp)

    # estimate total probability for each attribute within the trial block
    if (cards==2) {
      mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] +
        mnl.estp[seq(2, nrow(df.in), by=cards), ]
    } else if (cards==3) {
      mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] +
        mnl.estp[seq(2, nrow(df.in), by=cards), ] +
        mnl.estp[seq(3, nrow(df.in), by=cards), ]
    } else if (cards==4) {
      mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] +
        mnl.estp[seq(2, nrow(df.in),by=cards), ] +
        mnl.estp[seq(3, nrow(df.in),by=cards), ] +
        mnl.estp[seq(4, nrow(df.in),by=cards), ]
    } else if (cards==5) {
      mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] +
        mnl.estp[seq(2, nrow(df.in), by=cards), ] +
        mnl.estp[seq(3, nrow(df.in), by=cards), ] +
        mnl.estp[seq(4, nrow(df.in), by=cards), ] +
        mnl.estp[seq(5, nrow(df.in), by=cards), ]
    }

    # observed probability for each winning card
    mnl.chop   <- df.in[cards.win==1, ]

    # difference between observed and estimated probability
    mnl.diff   <- mnl.chop - mnl.estsum
    mnl.avdiff <- colMeans(mnl.diff)
    mnl.ss <- mean(mnl.avdiff^2)
    mnl.ssdiff <- mnl.sslast - mnl.ss
    mnl.sslast <- mnl.ss

    # update the partworths
    mnl.pws <- mnl.pws + mnl.avdiff   # gradient = 1.0 of diff. works OK.
    mnl.iters <- mnl.iters+1
    if (verbose & !no.output) {
      if (mnl.iters/20 == floor(mnl.iters/20) || mnl.iters==1) {
        cat("Iteration: ", mnl.iters, "  MSS: ", mnl.ss, "\n")
      }
    }
  }
  if (verbose & !no.output) {
    cat("Iteration: ", mnl.iters, "  MSS: ", mnl.ss, " Done.\n")
    cat(mnl.pws, "\n", fill=TRUE)
  }
  mnl.pws <- data.frame(t(mnl.pws))
  return(mnl.pws)
}

