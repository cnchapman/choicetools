# Initial open source portions were copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# Author: Chris Chapman
# Update: May 2023

############################
# MaxDiff plots

#############################################################
#############################################################
#
#  plot.md.range(md.define, use.raw=FALSE, item.disguise=FALSE, plot.zero=FALSE)
#
#  plot upper-level estimates from md.hb() with 95% credible intervals
#
#  md.define     : study object, with ...$md.hb.betas[.zc] present from md.hb()
#  use.raw       : use $md.hb.betas instead of zero-centered diffs, $md.hb.betas.zc
#  item.disguise : default FALSE. Should the labels be replaced with generic
#                  names? Useful for redacted presentations.
#  plot.zero     : whether to plot dotted vertical line at X=0 (default FALSE)
#                  (was the default as plotted through v0.70)
#

#' Plot sample level averages and CIs for MaxDiff utilities
#'
#' @param md.define A MaxDiff study object that contains individual-level
#' utility estimates present in a member called \code{$md.hb.betas} or
#' \code{$md.hb.betas.zc}.
#' Typically those are estimated created by \code{md.hb()} in this package.
#'
#' These data also be imported or created
#' by any other process, as long as they are rectangular with items in the
#' columns and respondents in the rows, and there is an "ID" column with
#' unique respondent identifies for each row. For example, if you export
#' data from Sawtooth Software or some other platform, you could load
#' and plot it with this function; just create an object
#' \code{md.define$md.hb.betas} with the estimates and an ID column.
#'
#' @param use.raw Whether to use raw MNL beta coefficients \code{$md.hb.betas},
#' if present, or some transformed e.g., zero-centered diff, version of them
#' present in \code{$md.hb.betas}
#' @param item.disguise Remove the item labels and number them generically.
#' This is included to make it easy to share and show results at a conference
#' or other audience when the underlying details are confidential.
#' @param plot.zero Plot a vertical line for zero (the point of indifference),
#' which only applies for raw beta utilities.
#'
#' @return A ggplot2 object with the plot.
#'
plot.md.range <- function(md.define, use.raw=FALSE,
                          item.disguise=FALSE, plot.zero=FALSE) {
  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }

  # first get the data reshaped for plotting
  library(reshape2)
  library(ggplot2)

  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }

  md.plot.df <- melt(cmr.beta.zc, id.vars="ID")

  # reorder the results by median utility
  cmr.order <- order(unlist(lapply(cmr.beta.zc[ , -ncol(cmr.beta.zc)], mean)))   # drop last column b/c it's ID

  md.plot.df$variable <- factor(md.plot.df$variable,
                                levels=unique(md.plot.df$variable)[cmr.order])
  if (item.disguise) {
    levels(md.plot.df$variable) <- paste0("i", 1:length(unique(md.plot.df$variable)[cmr.order]))[cmr.order]
  }

  # aggregate CIs by variable
  library(Rmisc)
  cmr.beta.agg <- group.CI(value ~ variable, md.plot.df)
  head(cmr.beta.agg)

  # TO DO: consider better scaling and centering?
  # y.center <- mean(cmr.beta.agg[, 3])
  # y.limits <- c(-max(abs(cmr.beta.agg[, 2:4])), max(abs(cmr.beta.agg[, 2:4]))+y.center)

  cmr.order <- order(cmr.beta.agg[ , 3])
  cmr.beta.agg$variable <- factor(cmr.beta.agg$variable,
                                  levels=unique(cmr.beta.agg$variable)[cmr.order])

  library(ggplot2)
  p <- ggplot(cmr.beta.agg,
              aes(x=variable, y=value.mean)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=value.lower, ymax=value.upper)) +
    ylab("Relative Preference") +
    xlab("Feature") +
    ggtitle("Preference by Task (overall average)") +
    # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +       # no scale labels
    # ylim(y.limits) +                                                         # will center on mean score (adds white space)
    coord_flip()

  # does user want a line at preference==0 ?
  if (plot.zero) {
    p <- p + geom_hline(yintercept=0, colour="darkred", linetype="dashed")
  }

  p
}


#############################################################
#############################################################
#
#  plot.md.indiv(md.define, use.raw=FALSE, item.disguise=FALSE,
#                plot.density=TRUE, plot.mean=TRUE, ran.seed=98103)
#
#  quasi-strip plot of individuals' mean betas along with the overall mean
#  useful to visualize the dispersion of estimated individual-level utilities
#
#  md.define     : study object, with ...$md.hb.betas[.zc] present from md.hb()
#  use.raw       : use $md.hb.betas instead of zero-centered diffs, $md.hb.betas.zc
#  item.disguise : default FALSE. Should the labels be replaced with generic
#                  names? Useful for redacted presentations.
#  plot.zero     : plot dotted line at X-axis preference == 0 ?
#  plot.density  : whether to use ggridges to draw density curves (default)
#                : FALSE --> draw strip plot of individuals w/o density curve
#  plot.mean     : add the mean beta as a red square? (default TRUE)
#  ran.seed      : random number seed, if you don't want the default
#                  used to make the point jitter/fill consistent
#

#' Plot individual-level estimates (density) for MaxDiff utilites
#'
#' This plot shows each individual's estimates as a blue circle for
#' each item, and allows visual inspection of the degree of dispersion
#' and general density properties of the estimates.
#' The sample level mean value is plotted as a red box.
#'
#' @param md.define A MaxDiff study object that contains individual-level
#' utility estimates present in a member called \code{$md.hb.betas} or
#' \code{$md.hb.betas.zc}.
#' Typically those are estimated created by \code{md.hb()} in this package.
#'
#' These data also be imported or created
#' by any other process, as long as they are rectangular with items in the
#' columns and respondents in the rows, and there is an "ID" column with
#' unique respondent identifies for each row. For example, if you export
#' data from Sawtooth Software or some other platform, you could load
#' and plot it with this function; just create an object
#' \code{md.define$md.hb.betas} with the estimates and an ID column.
#'
#' @param use.raw Whether to use raw MNL beta coefficients \code{$md.hb.betas},
#' if present, or some transformed e.g., zero-centered diff, version of them
#' present in \code{$md.hb.betas}
#' @param item.disguise Remove the item labels and number them generically.
#' This is included to make it easy to share and show results at a conference
#' or other audience when the underlying details are confidential.
#' @param plot.zero Plot a vertical line for zero (the point of indifference),
#' which only applies for raw beta utilities.
#' @param plot.density Whether to draw density lines
#' @param plot.mean Whether to add the sample mean value as a red box
#' @param ran.seed RNG seed for the minor jittering that is added to the plot
#' for better filling of the density areas.
#'
#' @return A ggplot2 object with the plot.
#'

plot.md.indiv <- function(md.define, use.raw=FALSE,
                          item.disguise=FALSE, plot.zero=FALSE,
                          plot.density=TRUE, plot.mean=TRUE, ran.seed=98103) {
  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }

  # first get the data reshaped for plotting
  library(reshape2)
  library(ggplot2)

  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }

  md.plot.df <- melt(cmr.beta.zc, id.vars="ID")

  # reorder the results by median utility
  cmr.order <- order(unlist(lapply(cmr.beta.zc[ , -ncol(cmr.beta.zc)], mean)))   # drop last column b/c it's ID

  mean.df <- lapply(cmr.beta.zc[ , -ncol(cmr.beta.zc)], mean)[cmr.order]
  # mean.df <- cbind(mean.df, 1:nrow(mean.df))

  md.plot.df$variable <- factor(md.plot.df$variable,
                                levels=unique(md.plot.df$variable)[cmr.order])
  if (item.disguise) {
    levels(md.plot.df$variable) <- paste0("i", 1:length(unique(md.plot.df$variable)[cmr.order]))[cmr.order]
  }

  p.resp <- length(unique(md.plot.df$ID))

  set.seed(ran.seed)
  if (plot.density) {
    # draw individuals with per-item density curves
    library(ggridges)
    if (plot.mean) {
      p <- ggplot(data=md.plot.df, aes(x=value, y=variable, group=variable)) +
        geom_density_ridges(scale=0.9, alpha=0, jittered_points=TRUE,
                            rel_min_height=0.005,
                            position="points_sina",
                            # quantile_lines = TRUE, quantiles = 2,
                            # quantile_fun=mean,
                            # vline_color = "red", vline_size=0.8,
                            # vline_alpha = 0.3,                             # seems to be unsupported
                            point_color = "blue", point_alpha=1/sqrt(p.resp),
                            point_size=2.5) +
        ylab("Item") + xlab("Relative preference (blue=individuals, red=mean)") +
        ggtitle("Preference estimates: Overall + Individual level")
      # add mean points
      for (i in 1:length(mean.df)) {
        p <- p + geom_point(x=mean.df[[i]], y=i, colour="tomato",
                            alpha=0.5, size=2.0, shape=0, inherit.aes=FALSE)
      }
    } else {
      p <- ggplot(data=md.plot.df, aes(x=value, y=variable, group=variable)) +
        geom_density_ridges(scale=0.9, alpha=0, jittered_points=TRUE,
                            rel_min_height=0.005,
                            point_color = "blue", point_alpha=1/sqrt(p.resp),
                            point_size=2.5) +
        ylab("Item") + xlab("Relative preference (points=individuals)") +
        ggtitle("Preference estimates: Overall + Individual level")
    }

    if (plot.zero) {
      p <- p + geom_vline(xintercept=0, colour="darkred", linetype="dashed")
    }


  } else {
    # draw individuals on strip plot, plus per-item mean beta
    p <- ggplot(data=md.plot.df, aes(x=variable, y=value)) +
      geom_point(size=3, alpha=1/sqrt(p.resp), colour="darkblue") +
      stat_summary(fun.y = "mean", colour = "red",
                   size = 3, geom = "point", alpha=0.5) +
      coord_flip() +
      xlab("Item") +
      ylab("Relative preference (blue=individuals; red=overall)") +
      ggtitle("Preference estimates: Overall + Individual level")
    if (plot.zero) {
      p <- p + geom_hline(yintercept=0, colour="darkred", linetype="dashed")
    }
  }

  p
}


############################################################
#############################################################
#
#  plot.md.heatmap()
#
#  Creates a heat map with row and column clusters for item/respondent
#  biclustering. Utility function.
#
#  md.define     : study object with ...$md.hb.betas[.zc] from md.hb() estimation
#  use.raw       : use raw betas instead of zero-centered
#  rnd.seed      : random seed to make the process repeatable. default 98103.
#  clus          : vector specifying number of clusters for (rows, cols)
#  clus.method   : "kmeans" or "hclust"
#  smooth.it     : whether to smooth all cells in a cluster into one color
#  item.disguise : replace labels with generic item numbers
#  col.scheme    : a color scheme: "viridis" (red, purple, blue, grey, green)
#

plot.md.heatmap <- function(md.define,
                            use.raw=FALSE, rnd.seed=98103,             # seed to make clustering repeatable
                            clus=c(4,5),                               # clus = c(rows, cols) for cluster grouping
                            clus.method = "kmeans",                    # options: kmeans or hierarchical
                            smooth.it=TRUE,                            # smooth=smooth over clusters
                            item.disguise=FALSE,                       # disguise the item labels?
                            col.scheme="viridis") {                    # options: viridis, red, purple, blue, grey, green

  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }
  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }

  if (item.disguise) {
    colnames(cmr.beta.zc) <- paste0("i", 1:length(colnames(cmr.beta.zc)))
  }

  set.seed(rnd.seed)
  library(superheat)
  superheat(t(cmr.beta.zc[ , 1:md.define$md.item.k]),

            left.label.size = 0.3,
            bottom.label.size = 0.1,

            clustering.method = clus.method,          # "kmeans" (default here and for superheat) is recommended

            n.clusters.cols = clus[2],                # adjust up or down to tell a story
            n.clusters.rows = clus[1],                # # " "

            smooth.heat = smooth.it,                   # include this to see median color by block;

            left.label = "variable",
            left.label.text.size = 3,

            bottom.label = "variable",
            bottom.label.text.size = 3,
            bottom.label.text.angle = 90,

            # change the color
            heat.col.scheme = col.scheme           # options: viridis, red, purple, blue, grey, green
  )
}


#############################################################
#############################################################

# plot.md.group(md.define, vec.groups,
#               [ groups.to.plot, item.disguise, use.raw, item.order ])
#
# Compare utilities for groups
#
#   md.define      : maxdiff object with individual-level estimates from md.hb()
#   vec.groups     : membership vector for each respondent. Must be coercible
#                      to factor variable (such as a vector of character strings)
#   groups.to.plot : optional. Among vec.groups, which ones should we plot?
#   item.disguise  : optional. Replace actual item names with generic numbers?
#   use.raw        : optional. Use raw utilities instead of zero-center diffs?
#   item.order     : optional. Vector of positions to sort the items. Defaults to
#                      the order of the overall mean beta across groups. See
#                      "?order" for details on how ordering vectors operate.
#                    Note: position "1" is at the bottom in ggplot2 plots. If
#                      you want 1 at the top, use rev(...)
#                    Alternative: name the group you want to order by, i.e., as
#                      a simple character string like item.order="Segment1"

plot.md.group <- function(md.define, vec.groups,
                          groups.to.plot = NULL,
                          item.disguise  = FALSE,
                          use.raw        = FALSE,
                          item.order     = NULL   ) {

  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }
  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }
  if (length(vec.groups) != nrow(cmr.beta.zc)) {
    stop("Can't match vec.groups to md.define utility betas. Vector length != nrow(betas).")
  }
  cmr.beta.zc$Group <- factor(vec.groups)

  # set up a melted DF for plotting, and order the variables by overall mean
  library(reshape2)
  md.plot.df <- melt(cmr.beta.zc, id.vars=c("ID", "Group"))
  # reorder the results by mean utility
  if (is.null(item.order)) {
    cmr.order <- order(unlist(lapply(cmr.beta.zc[ , 1:(ncol(cmr.beta.zc)-2)], mean)))       # drop last columns b/c ID + Group
  } else if (length(item.order) == 1) {
    cmr.order.tmp <- order(unlist(lapply(cmr.beta.zc[cmr.beta.zc$Group==item.order,
                                                     1:(ncol(cmr.beta.zc)-2)], mean)))   # drop last columns b/c ID + Group
    if (length(cmr.order.tmp) != md.define$md.item.k) {
      warning("item.order matching to group appears to be incorrect. Suggest to check exact group name.")
    }
    cmr.order <- cmr.order.tmp
  } else {
    cmr.order.tmp <- order(unlist(lapply(cmr.beta.zc[ , 1:(ncol(cmr.beta.zc)-2)], mean)))   # drop last columns b/c ID + Group
    if (length(item.order) != length(cmr.order.tmp)) {
      warning("item.order appears to be the wrong length.")
    }
    cmr.order <- item.order
  }

  md.plot.df$variable <- factor(md.plot.df$variable,
                                levels=unique(md.plot.df$variable)[cmr.order])
  if (item.disguise) {
    levels(md.plot.df$variable) <- paste0("i", 1:length(unique(md.plot.df$variable)[cmr.order]))[cmr.order]
  }

  # aggregate by group
  library(Rmisc)

  # unless groups are defined to include, include all of them
  if (is.null(groups.to.plot)) {
    groups.to.plot <- unique(vec.groups)
  }

  # get the aggregated means and CI
  # note: produces warnings for groups with N=1 member -- exclude them above
  cmr.beta.agg <- group.CI(value ~ Group + variable,
                           md.plot.df[as.character(md.plot.df$Group) %in% groups.to.plot, ])
  cmr.beta.agg <- na.omit(cmr.beta.agg)     # just in case, remove groups with N=1 (and NA values)
  head(cmr.beta.agg)

  # the plot
  library(ggplot2)
  dodge <- position_dodge(width=0.3)
  p <- ggplot(cmr.beta.agg,
              aes(x=variable, y=value.mean, group=Group)) +
    geom_point(aes(col=Group), position=dodge, size=2) +
    geom_errorbar(aes(ymin=value.lower, ymax=value.upper, color=Group),
                  position=dodge, alpha=0.4) +
    ylab("Preference estimate (mean preference + CI)") +
    xlab("Feature") +
    ggtitle("Preference for Item by Group") +
    coord_flip()
  p

}



#############################################################
#############################################################
#
# plot.md.relevant()
#
# plot the proportion of items that are relevant/irrelevant/important
#
# this may be useful if you have "relevant" and "important" checkboxes per
# the "chapman/bahna" adaptive Maxdiff method
#
# WARNING: highly experimental and not debugged
#

plot.md.relevant <- function(md.define, item.disguise=FALSE,
                             code.irrel=1, code.rel=2, code.unimp=1, code.imp=2)        # change these if your data are coded in reverse order, etc.
{

  warning ("plot.md.relevant() is currently not functioning; in progress")
  if (is.null(md.define$tasks.rel) | is.null(md.define$tasks.unimp)) {
    stop("Relevant and Important tasks (tasks.rel, tasks.unimp) not defined in md.define.")
  }

  tasks.rel   <- md.define$tasks.rel     # checkboxes for relevant
  tasks.unimp <- md.define$tasks.unimp   # checkboxes for "important to me"

  # set up DF to hold the status for each respondent on each task
  item.status <- md.define$md.csvdata[ , tasks.rel]
  item.status <- NA                                    # reset to NA for each respondent

  item.status[md.define$md.csvdata[ , tasks.rel] == code.irrel] <- "Irrelevant"
  item.status[md.define$md.csvdata[ , tasks.rel] == code.rel &
                md.define$md.csvdata[ , tasks.unimp] == code.unimp] <- "Relevant.but.not.Important"
  item.status[md.define$md.csvdata[ , tasks.rel] == code.rel &
                md.define$md.csvdata[ , tasks.unimp] == code.imp]   <- "Relevant.and.Important"

  tasks.grid.rel   <- colMeans(na.omit(md.define$md.csvdata[ , tasks.rel]==code.rel))
  tasks.grid.irrel <- 1-tasks.grid.rel

  data.unimp       <- md.define$md.csvdata[ , tasks.unimp]
  # OBS: because an irrelevant task will be NA, we need to recode those as "important" to get proper full-proportion relative share
  data.unimp[is.na(data.unimp)] <- code.imp

  tasks.grid.unimp       <- colMeans(na.omit(data.unimp==code.unimp))
  tasks.grid.imp         <- tasks.grid.rel - tasks.grid.unimp  # net importance, in addition to relevant


  # tasks.grid.cond.unimp  <- colMeans(na.omit(data.unimp==code.unimp))
  # tasks.grid.cond.imp    <- 1-tasks.grid.cond.unimp     # conditional importance, after being "relevant"
  # tasks.grid.imp         <- tasks.grid.rel * tasks.grid.cond.imp  # net importance, in addition to relevant
  # tasks.grid.unimp       <- 1-tasks.grid.imp

    tasks.grid <- data.frame(Task=paste0("i", 1:length(md.define$md.item.names)),
                             Relevant                = tasks.grid.rel )

  # if (item.disguise) {
  #   tasks.grid <- data.frame(Task=paste0("i", 1:length(md.define$md.item.names)),
  #                            Irrelevant                = tasks.grid.irrel,
  #                            Relevant.but.notImportant = tasks.grid.unimp,
  #                            Important.to.Job          = tasks.grid.imp )
  # } else {
  #   tasks.grid <- data.frame(Task=md.define$md.item.names,
  #                            Irrelevant                = tasks.grid.irrel,
  #                            Relevant.but.notImportant = tasks.grid.unimp,
  #                            Important.to.Job          = tasks.grid.imp )
  # }

  library(reshape2)
  tasks.grid.m <- melt(tasks.grid)

  names(tasks.grid.m) <- c("Item", "Rating", "value")
  library(ggplot2)
  p <- ggplot(data=tasks.grid.m,
              aes(x=Item, y=value, fill=Rating)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey60")) +
    scale_y_continuous(expand = c(0, 0)) +
    ylab("Proportion of Respondents") +
    ggtitle("Item Relevance for Respondents") +
    coord_flip()

  p
}



#############################################################
#############################################################
#
#  md.plot.logit(md.define, item.disguise)
#
#  a plot of aggregate logit results as found by md.quicklogit()
#
#  md.define     : study object with results from md.quicklogit() present in
#                  an ...$md.model.logit object
#  item.disguise : default FALSE. Should the labels be replaced with generic
#                  names? Useful for redacted presentations.

#' Plot aggregate logit model from quick estimation of MaxDiff utilities
#'
#' This gives a quick plot of results from \code{md.quicklogit()} to check
#' whether one's data looks reasonable before running hierarchical Bayes
#' estimation. See \code{md.quicklogit()} for details.
#' A good alternative is a simple plot of best and worst counts,
#' as done by \code{plot.md.counts()}
#'
#' @param md.define A MaxDiff study object that contains aggregate
#' utility estimates in an object member called \code{$md.model.logit},
#' as estimated by \code{md.quicklogit()} in this package.
#'
#' @param item.disguise Remove the item labels and number them generically.
#' This is included to make it easy to share and show results at a conference
#' or other audience when the underlying details are confidential.
#'
#' @return A ggplot2 object plotting the CI ranges for each item, as
#' estimated by \code{md.quicklogit()}.
#'
md.plot.logit <- function(md.define, item.disguise=FALSE) {

  if (is.null(md.define$md.model.logit)) {
    stop("No logit model present in md.define object.")
  }
  # get estimates to plot with CIs
  mlogit.ci      <- data.frame(confint(md.define$md.model.logit))
  mlogit.ci$mean <- md.define$md.model.logit$coefficients
  names(mlogit.ci) <- c("ciLow", "ciHigh", "Mean")
  if (item.disguise) {
    mlogit.ci$Feature <- paste0("i", 1:length(rownames(mlogit.ci)))
  } else {
    mlogit.ci$Feature <- rownames(mlogit.ci)
  }

  # reorder the results by median utility
  md.order <- order(mlogit.ci[ ,3])
  mlogit.ci$Feature <- factor(mlogit.ci$Feature, levels=mlogit.ci$Feature[md.order])

  library(ggplot2)
  p <- ggplot(data=mlogit.ci, aes(x=Feature, y=Mean)) +
    geom_errorbar(aes(ymin=ciLow, ymax=ciHigh)) +
    geom_point() +
    coord_flip() +
    ylab("Relative Preference") +
    ggtitle("Task Preference (aggregate model)")

  p
}


#############################################################
#
#  plot.md.counts(md.define, item.disguise=FALSE)
#
#  Plots counts of Best, Worst, and Best-Worst
#  Counts are normalized to how many times each item was shown
#
#  md.define     : study object with answers formatted in an "md.block"
#                  as read by the data import functions in this package
#  item.disguise : default FALSE. Should the labels be replaced with generic
#                  names? Useful for redacted presentations.

#' Plot best, worst, and net counts from MaxDiff data
#'
#' This plot shows how often each item was chosen as best and as worst, and
#' shows the difference between those (the net count).
#' This is a simple descriptive alternative to logit model estimation
#' of MaxDiff values and generally corresponds extremely closely with
#' aggregate and upper-level model estimates from multinomial regression
#' and hierarchical Bayes estimation.
#'
#' In the case that some items were shown more or less often than others,
#' regardless of whether they were chosen as best or worst or neither,
#' the counts here are rescaled to make the relative proportions equivalent.
#' For example, if one item appeared 1000 times but other items appeared only
#' 800 times each, then the counts for those other items would be multiplied by
#' 1.25 to account for the effective "proportion of times shown", compared
#' to the item with 1000 appearances.
#'
#' @param md.define A MaxDiff study object with \code{$md.block} data,
#' as imported by \code{read.md.qualtrics()} or \code{read.md.cho()}.
#' @param item.disguise Remove the item labels and number them generically.
#' This is included to make it easy to share and show results at a conference
#' or other audience when the underlying details are confidential.
#'
#' @return A ggplot2 chart with the best, worst, and net counts.

plot.md.counts <- function(md.define, item.disguise=FALSE) {
  if (is.null(md.define$md.block)) {
    stop("Could not find md.block matrix within the md.define object. Make sure data have been loaded first.")
  }
  exclude.cols <- c("win", "resp.id", "Block", "sys.resp", "Set", "choice.coded")
  item.cols    <- names(md.define$md.block)
  item.cols    <- item.cols[!item.cols %in% exclude.cols]

  best.appear <- colSums(md.define$md.block[md.define$md.block$Set=="Best", item.cols])
  best.win    <- colSums(md.define$md.block[md.define$md.block$Set=="Best" &
                                              md.define$md.block$win==1, item.cols])

  worst.appear <- colSums(md.define$md.block[md.define$md.block$Set=="Worst", item.cols])
  worst.win    <- colSums(md.define$md.block[md.define$md.block$Set=="Worst" &
                                               md.define$md.block$win==1, item.cols])

  item.scale  <- max(best.appear) / best.appear

  md.counts <- data.frame(Item  = item.cols,
                          Best  = best.win*item.scale,
                          Worst = worst.win*item.scale)

  if (item.disguise) {
    md.counts$Item <- paste0("i", 1:nrow(md.counts))
  }

  library(ggplot2)
  p <- ggplot(aes(x=reorder(Item, Best+Worst), y=Best),
              data=md.counts) +
    geom_col(alpha=0.3, color="darkgreen", fill="darkgreen") +
    geom_col(aes(x=Item, y=Worst), alpha=0.3, color="darkred", fill="darkred") +
    geom_point(aes(x=Item, y=Best+Worst), color="black", shape=19, size=1.5) +
    coord_flip() +
    ggtitle("Plot of MaxDiff Item Counts") +
    ylab("Times chosen as Best and Worst (point=net)") +
    xlab(ifelse(item.disguise, "Item (Disguised)", "Item"))
  p
}


