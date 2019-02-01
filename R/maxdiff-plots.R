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
#  item.disguise : default FALSE. Should the labels be replace with generic
#                  names? Useful for redacted presentations.
#  plot.zero     : whether to plot dotted vertical line at X=0 (default FALSE)
#                  (was the default as plotted through v0.70)
#

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
#  item.disguise : default FALSE. Should the labels be replace with generic
#                  names? Useful for redacted presentations.
#  plot.zero     : plot dotted line at X-axis preference == 0 ?
#  plot.density  : whether to use ggridges to draw density curves (default)
#                : FALSE --> draw strip plot of individuals w/o density curve
#  plot.mean     : add the mean beta as a red square? (default TRUE)
#  ran.seed      : random number seed, if you don't want the default
#                  used to make the point jitter/fill consistent
#

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
#   item.disguise  : optional. Replace actual item names with numbers?
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
                             code.rel=2, code.unimp=1)        # change these if your data are coded in reverse order, etc.
{

  if (is.null(md.define$tasks.rel) | is.null(md.define$tasks.unimp)) {
    stop("Relevant and Important tasks (tasks.rel, tasks.unimp) not defined in md.define.")
  }
  tasks.rel   <- md.define$tasks.rel     # checkboxes for relevant
  tasks.unimp <- md.define$tasks.unimp   # checkboxes for "important to me"

  tasks.grid.rel   <- colMeans(na.omit(md.define$md.csvdata[ , tasks.rel]==code.rel))
  tasks.grid.irrel <- 1-tasks.grid.rel
  tasks.grid.unimp <- colMeans(na.omit(md.define$md.csvdata[ , tasks.unimp]==code.unimp))
  tasks.grid.imp   <- tasks.grid.rel - tasks.grid.unimp

  if (item.disguise) {
    tasks.grid <- data.frame(Task=paste0("i", 1:length(md.define$md.item.names)),
                             Irrelevant=tasks.grid.irrel, Relevant.but.notImportant=tasks.grid.unimp, Important.to.Job=tasks.grid.imp )
  } else {
    tasks.grid <- data.frame(Task=md.define$md.item.names,
                             Irrelevant=tasks.grid.irrel, Relevant.but.notImportant=tasks.grid.unimp, Important.to.Job=tasks.grid.imp )
  }

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
#  item.disguise : default FALSE. Should the labels be replace with generic
#                  names? Useful for redacted presentations.

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


