# CBC experimental routines
#   use with caution, these are alpha release functions
#   they are a mix of useful (estimateMNLattrImpact()) and didactic (bootstrapMNLfromDesign())

# FUNCTIONS
#   estimateMNLattrImpact()   :
#
#   bootstrapMNLfromDesign()  :
#

#############################################
# estimateMNLattrImpact
#############################################
# function(df.in, cards.win, attrLevels, cards=3, n.samples=10, sample.prob = 0.67, start.pws=NULL, base.rate=NULL, imp.method="shuffle", stop.limit=1e-5, stop.iters=100, verbose=TRUE, no.output=FALSE)
#
# Calculates several measures of the "importance" of attributes in observed MNL prediction accuracy
#
# PARAMETERS
#   df.in           : Design-coded matrix of attribute levels
#                     can be derived from SSI SMRT "TAB" output using the "convertSSItoDesign()" function above
#   cards.win       : 1/0 coded winner for each card shown in the design file
#   attrLevels      : list of how many levels to create for each attribute (length = # of attributes; each entry = # of levels)
#   cards           : concepts shown per CBC task
#   n.samples       : iterations to run with new random samples to bootstrap the result
#   sample.prob     : Proportion of responses to use in the training set to develop MNL model
#   start.pws       : partworths for base MNL model, if known
#   base.rate       : base rate for base (comparison) MNL model, to which newly estimated predictions are compared. If NULL, will estimate from scratch with MNL by MLE
#   imp.method      :
#   est.method      : how the utilities are estimated. "aggregate" = using aggregate MNL (faster but less theoretically sound)
#                     "HB" = use hierarchical HB estimation provided through ChoiceModelR (much slower but better)
#   stop.limit      : point at which to stop estimation, when MSS of differences in prob are smaller than this limit. Default matches SMRT output to 3 digits in author's trials.
#   stop.iters      : maximum number of iteration steps
#   verbose         : show estimation steps while running
#   no.output       : run silently. only recommended if you're looping this function somewhere else; it takes a while to run
#
# OUTPUT
#   a matrix with 1 row per attribute, and columns as follow
#   Col 1 = "attr"          =  attribute number [i.e., 1:length(attrLevels)]
#   Col 2 = "n"             =  n.samples that the bootstrap was run [constant for all rows]
#   Col 3 = "mean"          =  the bootstrap mean of the attribute's choice prediction under the tested data perturbation model
#   Col 4 = "mean.dev"      =  the deviation of the mean above from the full-model base rate (i.e., the impact on prediction when this attribute is perturbed)
#   Col 5 = "sd"            =  observed sd of the mean across the n bootstraps
#   Col 6 = "mean.scaled"   =  the impact (mean.dev) as a proportion of the base rate observed in full model
#   Col 7 = "pct.of.TAD"    =  proportion this attribute represents, compared to sum of all attributes' absolute deviations
#                              i.e., deviationI / sum(abs(deviationALL))
# NOTES
#   1. The authors suggest using col 7 as an indicator of "impact" -- it sums to 1 across attributes, but is not zero-bounded for impact (negative impact is possible)
#   2. If you don't want pre-bootstrapped results, then set n.samples = 1, call this function inside a loop, and then work with the k=1 individual sample results
#
# ######################################################
# SAMPLE CODE
#
if (FALSE) {
  set.seed(4567)
  # create some CBC cards and known part worths
  tmp.attrs  <- c(4,4,5,2,7)                                                # let's create a CBC defined by 5 attributes, each with 2-7 levels
  tmp.tab    <- generateMNLrandomTab(tmp.attrs,respondents=200,cards=3,trials=8)    # create random CBC design for the given list of attributes/levels
  tmp.des    <- convertSSItoDesign(tmp.tab)                                 # convert those cards to a design-coded dummy matrix
  tmp.pws    <- generateRNDpws(tmp.attrs)                                   # make up some zero-sum part worths
  tmp.win    <- pickMNLwinningCards(tmp.des,tmp.pws, noise=TRUE)            # pick the winning cards in the design according to those part worths

  tmp.impact <- estimateMNLattrImpact(tmp.des, tmp.win, tmp.attrs)                       # 10 runs of attribute impact
  print(tmp.impact)
}
# ###############end sample code########################


# ====>>>>>> IN DEVELOPMENT <<<<<<====
# ====>>>>>> IN DEVELOPMENT <<<<<<====
# ====>>>>>> IN DEVELOPMENT <<<<<<====

estimateMNLattrImpact <- function(df.in, cards.win, attrLevels, cards=3, trials=8, resp=200,
                                  n.samples=10, sample.prob = 0.67,
                                  start.pws=NULL, base.rate=NULL, imp.method="shuffle", est.method="aggregate",
                                  stop.limit=1e-5, stop.iters=100, verbose=TRUE, no.output=FALSE)
{
  if (is.null(start.pws)) {
    if (!no.output) {
      cat("Estimating base MNL model ...\n")
      flush.console()
    }
    if (est.method=="HB") {
      bs.hb <- estimateMNLfromDesignHB(df.in, cards.win, kCards=cards, kTrials=trials, kResp=resp)
      bs.run <- as.data.frame(t(as.matrix(apply(extractHBbetas(bs.hb, attrLevels), 2, mean))))
    } else if (est.method=="aggregate") {
      bs.run <- estimateMNLfromDesign(df.in, cards.win,
                                      stop.limit=stop.limit, stop.iters=stop.iters,
                                      verbose=FALSE, no.output=TRUE)
    } else {
      warning("Incorrect est.method. Should be 'aggregate' or 'HB'. Assuming 'aggregate'.")
      bs.run <- estimateMNLfromDesign(df.in, cards.win,
                                      stop.limit=stop.limit, stop.iters=stop.iters,
                                      verbose=FALSE, no.output=TRUE)
    }
    if (!no.output) {
      cat("Done.\n")
    }
    base.pws <- bs.run[1,]
  } else {
    base.pws <- as.data.frame(start.pws)
    if (ncol(base.pws)==1) {
      base.pws <- t(base.pws)
    }
  }
  ## to do: find bootstrapped BASE RATE using holdout samples.
  ##        current implementation uses full sample MNL for base rate determination
  ## workaround: do this manually and pass in the base rate prediction
  ##
  if (is.null(base.rate)) {
    if (!no.output) {
      cat("Finding prediction base rate ...\n")
      flush.console()
    }
    pred.win <- pickMNLwinningCards(df.in,pws=as.vector(t(base.pws[1,])),cards=cards,verbose=FALSE,vec.format="WIN")
    pred.pct <- sum(pred.win==cards.win)/length(cards.win)
    if (!no.output) {
      cat("Base rate =",pred.pct,"\n")
    }
  } else {
    if (base.rate >= 0 && base.rate <= 1) {
      pred.pct <- base.rate
    } else {
      cat("Incorrect base rate specified (must be [0-1]).\n")
      cat("Finding prediction base rate ...\n")
      pred.win <- pickMNLwinningCards(df.in,pws=as.vector(t(base.pws[1,])),cards=cards,verbose=FALSE,vec.format="WIN")
      pred.pct <- sum(pred.win==cards.win)/length(cards.win)
      cat("Base rate =",pred.pct,"\n")
    }
  }

  # iterate over all attributes and see what happens when that attribute is removed from MNL estimation
  # create a matrix to hold the results
  rtn.mat <- data.frame(matrix(NA,ncol=7,nrow=length(attrLevels)))
  names(rtn.mat) <- c("attr","n","mean","mean.dev","sd","mean.scaled","pct.of.TAD")

  for (i in 1:length(attrLevels)) {
    if (verbose && !no.output) {
      cat("Estimating attribute",i,"impact ... ")
    }
    # select perturbed design file according to desired analysis method
    # omit: simply remove the attribute and re-estimate model without it
    #
    # resample "n.samples" number of times using holdout sample
    pct.vec <- NULL
    for (j in 1:n.samples) {
      if (verbose && !no.output) {
        cat(j," ")
        flush.console()
      }
      # omit: test model with all attributes *other* than the one in question to see how much performan drops with it omitted
      if (imp.method == "omit") {
        df.sel <- df.in[,-which(rep(1:length(attrLevels),attrLevels)==i)]
        # random: shuffle the levels of that attribute so they no longer match the shown design and see what effect that has
      } else if (imp.method == "shuffle") {
        df.sel <- df.in
        # single: test model with *only* the attribute in question to see how well it performs by itself
      } else if (imp.method == "single") {
        df.sel <- df.in[,which(rep(1:length(attrLevels),attrLevels)==i)]
      } else {
        cat("Sampling method specified incorrectly (\"single\" or \"omit\" or \"shuffle\"). Using \"omit\".\n")
        df.sel <- df.in[,-which(rep(1:length(attrLevels),attrLevels)==i)]
      }
      # divide sample into two samples for fitting and estimation
      # pick the card sets to sample
      n.sample <- floor(sample.prob * nrow(df.sel)/cards)
      fit.sample <- sample(nrow(df.sel)/cards,n.sample,replace=FALSE)
      # expand that to take all the rows
      fit.sample.ext <- (rep(fit.sample,each=cards)-1)*cards + rep(0:(cards-1),length(fit.sample)) + 1
      df.sel1 <- df.sel[fit.sample.ext,]
      df.sel2 <- df.sel[-fit.sample.ext,]
      # permute data in holdout sample ONLY, if method is "random"
      if (imp.method=="shuffle") {
        df.sel2[,which(rep(1:length(attrLevels),attrLevels)==i)] <- df.sel2[sample(nrow(df.sel2)),which(rep(1:length(attrLevels),attrLevels)==i)]
      }
      if (est.method=="aggregate") {
        # estimate new MNL model with only training sample cards
        pws.i <- estimateMNLfromDesign(df.sel1, cards.win[fit.sample.ext],
                                       stop.limit=stop.limit, stop.iters=stop.iters, verbose=FALSE, no.output=TRUE)
        # predict the cards in the holdout sample from those partworths
        pred.i <- pickMNLwinningCards(df.sel2, pws=as.vector(t(pws.i[1,])),
                                      cards=cards, verbose=FALSE, vec.format="WIN")
        # see how many of those we got right vs. the known correct cards
        pct.i  <- sum(pred.i==cards.win[-fit.sample.ext])/length(cards.win[-fit.sample.ext])
        pct.vec <- c(pct.vec, pct.i)
      } else {      # HB estimation
        # *****
        # *****
        # estimate new MNL model with only training sample cards
        #              tmp.hb <- estimateMNLfromDesignHB(df.sel1, cards.win[fit.sample.ext], kCards=cards, kTrials=trials, kResp=resp)
        #              pws.hb <- as.data.frame(extractHBbetas(bs.hb, attrLevels))

        # *****
        # *****   just default to previous for now

        pws.i <- estimateMNLfromDesign(df.sel1,cards.win[fit.sample.ext],stop.limit=stop.limit,stop.iters=stop.iters,verbose=FALSE,no.output=TRUE)

        # predict the cards in the holdout sample from those part worths
        pred.i <- pickMNLwinningCards(df.sel2,pws=as.vector(t(pws.i[1,])),cards=cards,verbose=FALSE,vec.format="WIN")
        # see how many of those we got right vs. the known correct cards
        pct.i  <- sum(pred.i==cards.win[-fit.sample.ext])/length(cards.win[-fit.sample.ext])
        pct.vec <- c(pct.vec, pct.i)
      }
    }
    # cat ("Rate excluding attr ",i," = ",pct.i,"\n")
    # cat ("Diff from baserate = ",(pct.i-pred.pct),"\n")
    #       names(rtn.mat) <- c("attr","n","mean","mean.dev","sd","mean.scaled","pct.of.TAD")
    pct.i <- mean(pct.vec)
    rtn.mat[i,1] <- i
    rtn.mat[i,2] <- length(pct.vec)    # ** to do: error test vs. n.samples, should be same
    rtn.mat[i,3] <- pct.i
    rtn.mat[i,4] <- (pct.i-pred.pct)
    rtn.mat[i,5] <- sd(pct.vec)
    rtn.mat[i,6] <- ifelse(pred.pct != 0, (pct.i-pred.pct)/pred.pct, NA)
    if (verbose && !no.output) {
      cat(pct.i-pred.pct,"\n")
      flush.console()
    }
  }
  rtn.TAD <- sum(abs(rtn.mat[,4]))
  rtn.mat[,7] <- rtn.mat[,4] / rtn.TAD

  return(rtn.mat)
}


#############################################
# bootstrapMNLfromDesign()
#############################################
# function(df.in, cards.win, cards, trials, bs.prop=1.0, bs.reps=1000,
#   bs.rescale=FALSE, start.pws=rep(0,ncol(df.in)), stop.limit=1e-6,
#   stop.iters=100, no.output=FALSE)
#
# Performs bootstrap of MNL models using a design matrix + winning cards
# Primary usage is to find empirical credible intervals of aggregate MNL models
#
# PARAMETERS
#   df.in = design matrix
#   cards.win = vector of 1/0 denoting winning cards to match design matrix
#   cards = number of cards per trial
#   trials = number of trials per respondent
#   bs.prop = ratio of bootstrap sample size to respondent total sample size
#             bootstrapping uses sample-with-replacement, so 1.0 may be OK
#   bs.reps = how many times to sample the bootstrap model
#   bs.rescale = whether to rescale the results to a comparable baseline
#   ^^^^^^^^^^   EXPERIMENTAL, but consider using because MNL partworths are
#                not absolutely scaled or comparable without rescaling
#   ^^^^^^^^^^ Needs further work to ensure probabilities are comparable.
#              current version is seat of the pants only!
#
#   start.pws = starting partworths for "estimateMNLfromDesign()".
#               rep(0,...) is generally OK unless you have convergence problems
#   stop.limit = stopping gradient for "estimateMNLfromDesign()"
#   stop.iters = stopping point for "estimateMNLfromDesign()"
#   no.output = run silently?   (not recommended)
#
# NOTE:
# in v0.2, all respondents must have the same number of cards and trials
# see "estimateMNLfromDesign()" for more details
#
# SEE ABOVE FOR SAMPLE CODE THAT WILL RUN DIRECTLY
#

bootstrapMNLfromDesign <- function(df.in, cards.win, cards, trials,
                                   bs.prop=1.0, bs.reps=1000, bs.rescale=FALSE,
                                   start.pws=rep(0,ncol(df.in)),
                                   stop.limit=1e-6, stop.iters=100,
                                   no.output=FALSE)
{
  # key sampling parameters
  n.resp <- nrow(df.in) / cards / trials
  n.samp <- round(n.resp * bs.prop)

  # get a single-shot MNL estimate from full df.in,
  # used for rescaling results to a comparable base
  logit.run <- estimateMNLfromDesign(df.in, cards.win, start.pws=start.pws,
                                     stop.iters=stop.iters, verbose=FALSE, no.output=TRUE)

  # set up matrix to hold results
  bs.models <- data.frame(matrix(0,nrow=bs.reps,ncol=ncol(df.in)))
  for (i in 1:bs.reps)
  {
    # set up sample
    bs.samp <- sample(1:n.resp, n.samp, replace=TRUE)
    bs.sel  <- sapply((bs.samp-1)*cards*trials+1, seq,
                      length.out=cards*trials)
    df.bs <- df.in[bs.sel, ]
    cards.bs <- cards.win[bs.sel]

    # estimate MNL on that sample
    bs.run <- estimateMNLfromDesign(df.bs, cards.bs, start.pws=start.pws,
                                    stop.limit=stop.limit,
                                    stop.iters=stop.iters,
                                    verbose=FALSE, no.output=TRUE)

    # handle MNL part worth scale instability
    # rescale the results to be comparable to the single-shot logit model
    # EXPERIMENTAL: use with caution -- simple linear rescaling for now, so
    #   it doesn't preserve probabilities and exponential relationships
    if (bs.rescale) {
      # compute simple scale factor, within 80% quantiles (trim 10% hi/lo)
      bs.scale <- mean(t(logit.run/bs.run),trim=0.10)
      # and scale the results by that much
      bs.run <- bs.run * bs.scale
    }

    # add to overall results matrix
    bs.models[i, 1:ncol(df.in)] <- bs.run[1, 1:ncol(df.in)]
    if (!no.output) {
      cat("Iteration: ", i, "  PWs 1-5: ",
          paste(bs.run[1, 1:(ifelse(ncol(df.in)>=5, 5, ncol(df.in)))],
                sep=","), "\n")
    }
  }

  if (!no.output) {
    cat("Done.\n\n")
    cat(rcbc.citation.string)
  }
  return(bs.models)
}
