# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# MaxDiff estimation

#############################################################
#############################################################
#
#  md.hb(md.define, mcmc.iters=1000, pitersUsed=0.1,
#        mcmc.seed=..., restart=FALSE)
#
#  estimate MaxDiff utilities using hierarchical Bayes model
#  this is the recommended way to estimate the model, although it can be slow
#
#  md.define   : your study object with md.block for observations
#  mcmc.iters  : how many MCMC iterations to run. Suggest default 1000 for
#                quick check of model, and 20000 or more for actual results.
#  pitersUsed  : the fraction of posterior draws to use when taking draws
#                for example 0.1 [default] will use the last 10% of draws.
#                Within that set, every 10th draw will be saved (that is fixed
#                and has nothing to do with this parameter as such.)
#  mcmc.seed   : set this if you want to specify a reusable random seed
#                otherwise, function will pick one and report it
#  restart     : default FALSE. Whether to use estimates from a prior run, if
#                available.
#
#  Note on Performance: if you run more than 5000 MCMC iterations, it can be
#  much faster to call this function repeatedly with restart=TRUE. For example,
#  to get 50000 iterations, call it once, and then put it in a loop with 9
#  additional iterations of 5000 draws, "restart=TRUE". Finally, call it
#  once more after than with pitersUsed=0.5 to get a final posterior draw of
#  whatever size you prefer.
#

#' Hierarchical Bayes estimation for MaxDiff data
#'
#' @description Estimates a hierarchical Bayes (HB) model for MaxDiff observations.
#' This is primarily a wrapper for \code{ChoiceModelR::choicemodelr} that
#' formats the data, calls \code{choicemodelr}, and extracts the results.
#'
#' @param md.define The structured data with MaxDiff observations. This is
#' typically created by an importing function such as \code{read.md.qualtrics()}
#'
#' @param mcmc.iters How many iterations to run the MCMC estimation process.
#' Default is 1000 iterations (suitable only for testing), recommend 10000 or
#' more for typical usage.
#'
#' @param pitersUsers The proportion of the MCMC chain to retain, from
#' the end of the chain. Default 0.1 for 10% posterior draws.
#'
#' @param mcmc.seed Random number seed to make the process repeatable. Default
#' is that the function will draw a random number to be the seed and report it.
#'
#' @returns Returns a list with the following objects: \code{md.model.hb} is the
#' result from a call to \code{choicemodelr} to estimate the model;
#' \code{md.hb.betas} are the raw multinomial logit model beta coefficients;
#' and \code{md.hb.betas.zc} are zero-centered difference scores that may
#' be more interpretable for stakeholder audiences. Use \code{plot.md.range()}
#' to plot the aggregate results, or \code{plot.md.indiv()} to plot the
#' individual-level results, or \code{plot.md.group()} to compare
#' distributions by categorical groups such as demographic or treatment groups.
#'
md.hb <- function(md.define,
                  mcmc.iters=1000, pitersUsed = 0.1,
                  mcmc.seed=runif(1, min=0, max=1e8), restart=FALSE) {

  cat("Setting up HB estimation with random seed", mcmc.seed, "\n")
  md.block <- md.define$md.block

  if (mcmc.iters < 10000) {
    warning("You appear to have 'mcmc.iters' set too low for a production run.\nThis is OK for testing, but increase for actual estimation!")
  }

  ## .1: vector for the sequential order of tasks within best/worst blocks
  ## ... (called "Alt" by ChoiceModelR)

  # helper function to count sequential occurrences of 1s in a vector
  best.seq <- function(x) {
    Reduce(function(x, y) if (y == 0) 0 else x+y, x, accumulate=TRUE)
  }

  # get sequence of 1s for all the "best" alternatives (max(design cols) == 1)
  task.seq.b <- best.seq( apply(md.block[ , 3:(md.define$md.item.k+2)], 1, max))

  # same for "worst" alternatives (min == -1)
  task.seq.w <- best.seq(-apply(md.block[ , 3:(md.define$md.item.k+2)], 1, min))

  # get the united sequences for B and W -- the "Alt" sequence for ChoiceModelR
  task.seq   <- pmax(task.seq.b, task.seq.w)    # ALT

  ## .2: vector for the sets of tasks within respondent (ChoiceModelR "Set")
  ##
  task.order <- function(which.resp) {
    task.set              <- rep(0, sum(md.block$resp.id==which.resp))
    which.first           <- which(task.seq[md.block$resp.id==which.resp]==1)
    task.set[which.first] <- 1:length(which.first)
    task.set              <- cummax(task.set)
    task.set
  }
  task.count <- rep(0, nrow(md.block))            # "SET"
  for (i in unique(md.block$resp.id)) {
    task.count[md.block$resp.id==i] <- task.order(i)
  }

  ## .3: vector for wining concept's line within block, specified on line 1 ("y")
  ##
  if (sum(md.block$win==1) != sum(task.seq==1)) {
    stop("Some task blocks have no winner. CHO file may include incomplete data (not yet handled).")
  }
  task.win              <- rep(0, length(task.seq))
  task.win[task.seq==1] <- task.seq[md.block$win==1]            # "y"
  task.win
  # tail(task.win, 50)

  ## .4: put together the data for ChoiceModelR
  cmr.block <- data.frame(UnitID = md.block$resp.id,
                          Set    = task.count,
                          Alt    = task.seq,
                          md.block[ , 3:(md.define$md.item.k+2)], #+1
                          y      = task.win)

  cmr.block <- cmr.block[order(cmr.block$UnitID), ]   # must be ordered by ID !

  ## .5: set up estimation parameters
  ##
  # set up ChoiceModelR parameters
  tmp.coding  <- rep(0, md.define$md.item.k) #-1                   # 0 = categorical coding for the attribute
  tmp.mcmc    <- list(R = mcmc.iters, use = mcmc.iters*pitersUsed)
  opt.restart <- restart & file.exists("restart.txt")            # automatically restarts if available
  tmp.opt     <- list (none=FALSE, save=TRUE, keep=10, restart=opt.restart)               # no "none" values, save draws, keep every 10

  # .6: be sure to display graphics window to see convergence plot
  # ... and run it!
  library(ChoiceModelR)


  ## .6a: Actual estimation
  ## WARNING: SLOW! Est'd 1hr per 40K iterations
  ##
  set.seed(mcmc.seed)
  cmr.out <- choicemodelr(data=cmr.block,
                          xcoding=tmp.coding, mcmc=tmp.mcmc, options=tmp.opt,
                          directory=getwd())


  ## .7: get the betas per respondent

  # helper function
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

  # .71: get the individual-level average betas from ChoiceModelR model object
  md.attrs <- rep(2, md.define$md.item.k)
  cmr.beta <- extractHBbetas(cmr.out, md.attrs)[ , seq(from=1, to=md.define$md.item.k*2, by=2)]

  # .72: reshape the betas to a data frame
  if (!is.null(md.define$md.item.names)) {
    colnames(cmr.beta) <- md.define$md.item.names[1:md.define$md.item.k]
  } else {
    colnames(cmr.beta) <- names(md.define$md.block[3:(md.define$md.item.k+2)])
  }
  cmr.beta <- data.frame(cmr.beta)

  # .73: add respondent ID
  cmr.beta$ID <- unique(cmr.block$UnitID)    # works b/c cmr.beta is really beta.mu (mean), so 1 ID per row

  # .8: rescale within respondent for comparability
  # Rescale to Zero-centered diffs, following steps noted at
  # https://sawtoothsoftware.com/forum/6140/is-there-a-formula-for-calculating-the-zero-centered-diffs

  # we're going to make a new ".zc" frame to hold the results
  cmr.beta.zc <- cmr.beta[ , 1:md.define$md.item.k]     # get just the utility columns, omitting ID

  cmr.beta.mu <- rowMeans(cmr.beta.zc)       # average utility per respondent
  cmr.beta.zc <- cmr.beta.zc - cmr.beta.mu   # mean-centered within respondent  (step #1 from URL)

  library(matrixStats)
  # total spread btw Min & Max across all attributes (step #2 from URL)
  cmr.beta.zc.sumdiffs <- max(cmr.beta.zc)-min(cmr.beta.zc)
  cmr.beta.zc.mult     <- 100/ cmr.beta.zc.sumdiffs   # multiplier to rescale (step #3)

  # now recale the zero-centered utilities to that 100 pt scale
  cmr.beta.zc <- cmr.beta.zc * cmr.beta.zc.mult     # (step #4 from URL)

  # check the diffs between min and max per attribute (should be 100 on average)
  #
  # TO DO:check these and warn if any problems
  #
  # colMaxs(as.matrix(cmr.beta.zc))-colMins(as.matrix(cmr.beta.zc))        # should be roughly 50-150 each
  # mean(colMaxs(as.matrix(cmr.beta.zc))-colMins(as.matrix(cmr.beta.zc)))  # should be exactly 100

  # add the ID column back into it
  cmr.beta.zc$ID <- cmr.beta$ID

  return(list(md.model.hb=cmr.out, md.hb.betas=cmr.beta, md.hb.betas.zc=cmr.beta.zc))
}


#############################################################
#############################################################
#
#  md.quicklogit(md.define, preadapt.only)
#
#  Estimates a maxdiff model using classical multinomial logit (aggregate
#  level only). Results are relative to omitted reference level (final attribute).
#  Recommend using this as a quick check of data sanity and correct
#  directional coding. For reportable results, recommend md.hb() instead.
#
#  md.define     : study object with md.block to estimate
#  preadapt.only : default TRUE. whether to exclude any blocks of augmented data
#                  (recommended, as it may fail otherwise)
#

#' Estimate MaxDiff utilities quickly with an aggregate logit model
#'
#' @description
#' \code{md.quicklogit} is intended to give a quick check to see whether
#' your data are structure correctly and have reasonable estimates,
#' before investing time to run a hierarchical Bayes model (\code{md.hb()}).
#' A common error is to have the values reversed for the "best" and "worst"
#' observations. That will appear in the results with obviously preferred
#' items showing low preference, while poor items show high preference.
#'
#' The fix in that case is to relabel those data points such that the
#' "best" items' value
#' is greater than the value for the worst items (the exact values don't
#' matter). For example, if your data accidentally code best as 1, and worst
#' as 2, you could replace all of the best observations with a value of 3.
#' Then run \code{md.quicklogit} again.
#'
#' Note that \code{md.quicklogit} drops 1 item for model identification, and
#' thus reports K-1 estimates. For example, if you have 15 items you will
#' see 14 estimated parameters in a summary of the return object.
#'
#' @param md.define A study object as documented for \code{parse.md.qualtrics()}
#' @param preadapt.only An experimental parameter for adaptive models, to be
#' documented in the future. For now, leave this as \code{TRUE}.
#'
#' @return A model object from \code{mlogit::mlogit()} with parameter estimates
#' for K-1 levels of your MaxDiff items. Use \code{md.plot.logit()} to plot
#' the results.
#'
#' @seealso [md.hb()] for the recommended hierarchical Bayes estimation.
#'
md.quicklogit <- function(md.define, preadapt.only=TRUE) {
  md.block <- md.define$md.block
  rownames(md.block) <- paste0("r", 1:nrow(md.block))

  # to do: integrate the block and row ("set" and "alt") from ChoiceModelR section below
  #

  library(mlogit)
  nrow.use <- ifelse(preadapt.only,
                     ifelse(is.null(md.define$md.nrow.preadapt),
                            nrow(md.block),
                            md.define$md.nrow.preadapt),
                     nrow(md.block))

  # shape data depending on which version of mlogit is installed
  if (packageVersion("mlogit") < "1.1") {
    mlogit.ready <- mlogit.data(md.block[1:nrow.use, ],
                                shape = "long",
                                choice = "choice.coded",
                                chid.var="chid",
                                alt.levels=seq(1, md.define$md.item.pertask, 1),
                                id.var="resp.id")   # choice is the response, chid is the task id, alt.levels shows the options per task
  } else {
    library(dfidx)
    # add chid for unique block indexing per mlogit 1.1+
    md.block$chid <- ((md.block$resp.id-1)*md.define$md.item.pertask + md.block$Block)*2 + ifelse(md.block$Set=="Worst", 0, -1)
    # and add the alternative counter for each
    # note that this assumes perfect rectangularity for now -- TO DO relax that
    md.block$alt  <- rep(1:md.define$md.item.pertask, nrow(md.define$md.block)/md.define$md.item.pertask)
    md.block$choice.coded <- ifelse(md.block$choice.coded=="yes", TRUE, FALSE)

    mlogit.ready <- dfidx(md.block[1:nrow.use, ],
                          choice = "choice.coded",
                          idx="chid")
  }

  mlogit.f.raw <- paste("choice.coded ~ 0 + ",
                        paste(names(md.block[3:(md.define$md.item.k+1)]), collapse=" + "))

  # mlogit 1.1 also changed its formula interface
  if (packageVersion("mlogit") < "1.1") {
    mlogit.f     <- mFormula(as.formula(mlogit.f.raw))
  } else {
    mlogit.f     <- formula(as.formula(mlogit.f.raw))
  }

  cat("Estimating mlogit formula:\n", as.character(mlogit.f),"\n\n")

  # estimation!
  mlogit.model <- (mlogit(mlogit.f, data = mlogit.ready, probit = FALSE))
  cat("Done.")

  return(mlogit.model)

}

