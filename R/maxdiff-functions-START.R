# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

#############################################################
# maxdiff-functions-START.R
#
# Author: Chris Chapman
# November 2022
#
# NOTE ON OPEN SOURCE
# These functions are patches to the open source Rcbc.R (https://goo.gl/oK78kw)
# and will be reverse-integrated into Rcbc.R periodically.
#
# LICENSE (Apache 2.0)
# This software is free and open source under Apache License 2.0
# "You may reproduce and distribute copies of the Work or Derivative Works thereof
#    in any medium, with or without modifications ..."
# See the file "LICENSE" for complete Apache license.

#############################################################


#############################################################
#############################################################
# DOCUMENTATION
#
# OVERVIEW:
# These functions provide complete handling of MaxDiff data from Sawtooth Software
# and Qualtrics survey platforms. See function intros for most documentation.
#
# In particular, they take a CHO file exported from Sawtooth Software Lighthouse Studio,
# or a CSV file from Qualtrics for a MaxDiff exercise, reshape the data,
# estimate multinomial logit or Hierarchical Bayes models, and create various plots.
#
# Additionally the code implements an experimental "augmented maxdiff" model,
# -- only for Sawtooth Software studies with a specific setup --
# where respondents may respond to a subset of items. Ask cchapman@ or
# bahna@ for details (and see section on "Augment" below).
#

#
# SETUP: REQUIRED LIBRARIES (depending on which functions you use)
#   reshape2, ggplot2, mlogit, ChoiceModelR, Rmisc, matrixStats, superheat,
#   corrplot, and ggridges [only if drawing density plots]
#   ==> be sure to install these first, e.g.:
#
if (FALSE) {
  required.packages <- c("reshape2", "ggplot2", "mlogit", "ChoiceModelR",
                         "Rmisc", "matrixStats", "superheat", "corrplot",
                         "ggridges")
  needed.packages <- required.packages[!(required.packages %in% installed.packages())]
  if (length(needed.packages) > 0) {
    install.packages(needed.packages)
  }
}


# ################
# PROCEDURE TO USE:
#   0. Field a MaxDiff survey and get its data
#      This involves using Sawtooth Software and exporting the "CHO file" for the MaxDiff block.
#      You will need that CHO file as a minimum to use the remainder of this script.
#      Alternatively, use Qualtrics, but be very careful about export format (ask cchapman@: legacy exporter with random order exported)
#
#   1. Define an "md.define" object (or whatever name you want) below. In particular, define:
#      .. where your data files are
#      .. the design parameters of your MaxDiff task (items, tasks, and items shown per task)
#      .. any "friendly" item names that you want to use as labels on plots
#      .. whether you want to use the augmented method (NOT unless survey was designed for it; see notes)
#      ==> see Step 3 for more pointers
#
#   2. After that, use the functions defined below to read data, estimate a model, and plot.
#      Here's a complete example, using a specified "md.define" study object as shown below:
#
       if (FALSE) {
         test.read                <- read.md.cho(md.define)    # read the CHO data [Sawtooth]
         # test.read              <- read.md.qualtrics(md.define) # read Qualtrics data
         md.define$md.block       <- test.read$md.block        # save the data back into our study object

         md.define$md.model.logit <- md.quicklogit(md.define)  # aggregate logit model (fast check)
         summary(md.define$md.model.logit)
         md.plot.logit(md.define)                              # plot the logit model estimates
         # YES! Really, that's all -- top-level answers with 5 lines of code (after model setup)

         # Hierarchical Bayes, individual-level estimation
         test.hb <- md.hb(md.define)                           # estimation (note: set mcmc.iters appropriately)
         md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
         md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw utilities by individual
         md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)
         rm(test.hb)
         summary(md.define$md.hb.betas.zc)

         p <- plot.md.indiv(md.define)                         # create plot of HB model with individual mean betas
         p + theme_minimal()                                   # can use other ggplot functions like ggtitle()
       }

#
#############################################################

#############################################################
#############################################################

# EXAMPLE STUDY DEFINITIONS
# You MUST set up an object with:
#    1. the locations of your files (Sawtooth and/or Qualtrics)
#    2. the overall design of your MaxDiff task
#    3. options such as whether to do adaptive MaxDiff and short/friendly names you prefer
# ==> Read each entry and update it as needed.

#########################
# SAWTOOTH SOFTWARE VERSION FOR STUDY SETUP
# See Step 3 about ("maxdiff-examples.R") for more details
if (FALSE) {
  md.define.saw <- list(
    # DATA SETUP - REQUIRED
    # Working directory and file locations
    file.wd          = "~/Documents/Chris Documents/Virtual Machines.localized/shared-from-vm/cloud-CUJ-May2017/Job/",
    file.cho         = "MaxDiffExport/MaxDiffExport.cho",  # CHO export from Sawtooth export jobs, required
    file.lab         = "MaxDiffExport/MaxDiffExport.val",  # VAL file from that same export location, optional
    file.all         = "Export/Export.csv",                # full CSV export, only needed if using adaptive
    # resp.rows        = NULL,                             # NOT YET IMPLEMENTED FOR SAWTOOTH [which rows of file to keep]
    #                                                      # numbered as *line number in the CSV* (not resp number)

    # MAXDIFF DESIGN - REQUIRED
    md.item.k        = 33,                                 # total # of items on maxdiff list
    md.item.tasks    = 10,                                 # num of choice trials per respondent (max)
    md.item.pertask  = 5,                                  # num of concepts shown in each trial

    # ITEM NAMES - OPTIONAL BUT RECOMMENDED
    # Friendly names to use for the item labels in plots, etc.
    # Set this to NULL if you want simpler "i1", "i2", etc, or want to read them from the VAL file
    # Important: Order must exactly match the order in the data file, or things will be mislabeled!
    md.item.names    = c('My.name.1',
                         paste0("My.name.", 2:32),         # etc.
                         'My.name.33'),

    # OPTIONAL: SETUP FOR AUGMENTED MAXDIFF (ADAPTIVE METHOD)
    # these are magic numbers and must be selected to match your data
    # nonsequential vectors (e.g., c(23, 24, 28, 32, 33)) are believed to work, but are not tested
    #
    tasks.rel        = 252:284,                            # columns of file.all with checkboxes for relevant
    tasks.unimp      = 285:317,                            # columns of file.all with checkboxes for "important to me"
    md.adapt         = TRUE,                               # use adaptive method to supplement choices?
    md.adapt.Imp     = 493:525,                            # columns of file.all that list items selected as "Important"
    md.adapt.NotImp  = 592:624,                            # columns of file.all that list "not important" items

    # REFERENCE: CHOICE DATA USED IN ESTIMATION
    md.block         = NULL,                               # where we'll put choice data as it's read / augmented
    md.csvdata       = NULL,                               # where we'll hold other survey data, if needed
    md.nrow.preadapt = NULL,

    # REFERENCE: STATISTICAL RESULTS
    md.model.logit   = NULL,                               # hold aggregate mlogit estimates
    md.model.hb      = NULL,                               # hold HB model results
    md.hb.betas      = NULL,                               # individual-level raw betas from HB model
    md.hb.betas.zc   = NULL                                # individual-level zero-centered diffs from HB model
  )
}


#########################
# QUALTRICS VERSION FOR STUDY SETUP
#
if (FALSE) {
  md.define.qt <- list(
    # DATA SETUP - REQUIRED: Working directory and file locations
    file.wd          = NULL,   # not generally relevant for Qualtrics because only one data file (next line)
    file.qsv         = "~/Downloads/Developer_Experience_Priorities (2).csv",   # Qualtrics export file
    resp.rows        = NULL,                               # which rows of file to keep. NULL == all.
                                                           # numbered as *line number in the CSV* (not resp number)

    # REQUIRED: MAXDIFF DESIGN
    md.item.k        = 22,                                 # total # of items on maxdiff list
    md.item.tasks    = 5,                                  # num of choice trials per respondent (max)
    md.item.pertask  = 5,                                  # num of concepts shown in each trial

    # OPTIONAL BUT RECOMMENDED: FRIENDLY ITEM NAMES
    # Friendly names to use for the item labels in plots, etc. See Sawtooth object above for example.
    # Set this to NULL if you want simpler "i1", "i2", etc, or want to read them from the VAL file
    # Important: Order must exactly match the order in the data file, or things will be mis-labeled!
    md.item.names    = NULL,

    # REQUIRED (PROBABLY): QUALTRICS FILE LAYOUT
    # required if you're going to use the read.md.qualtrics() function to get Qualtrics data
    #
    q.startDesCol     = 117,                               # the first column with design matrix (e.g., "6|3|2|9" or whatever)
    q.startMDcol      =   7,                               # the column where the MaxDiff responses begin. ASSUMES continuous in current version
    q.endMDcol        = 116,                               # where the MaxDiff items end. See not #7 above!
    q.itemSplit       = "...-",                            # separator between Qualtrics header & actual MaxDiff item label
    q.removeInc       = TRUE,                              # remove respondents with any missing MaxDiff observations?
    q.codeMDpos       = 3,  # (usually 2, but varies)      # the code used for the "winning" MD item, from Qualtrics
    q.codeMDneg       = 1,  # (usually 1, but check)       # the code used for the "losing" MD item

    # REFERENCE: CHOICE DATA USED IN ESTIMATION
    md.block         = NULL,                               # where we'll put choice data as it's read / augmented
    md.csvdata       = NULL,                               # where we'll hold other survey data, if needed
    md.nrow.preadapt = NULL,

    # REFERENCE: STATISTICAL RESULTS
    md.model.logit   = NULL,                               # hold aggregate mlogit estimates
    md.model.hb      = NULL,                               # hold HB model results
    md.hb.betas      = NULL,                               # individual-level raw betas from HB model
    md.hb.betas.zc   = NULL                                # individual-level zero-centered diffs from HB model
  )
}


#############################################################
#############################################################
#
# FUNCTION INDEX
#
# OVERVIEW:
# You should NOT need to change anything inside the functions themselves.
#
# Note that each function is followed by a unit test section. You can
# use those tests as skeletons for your analyses.
#
#
# READING DATA
#   parse.md.cho(filename, ...)      # set up an md.define object from Sawtooth Software CHO [and VAL, and CSV] file
#   read.md.cho(md.define)           # read a Sawtooth Software CHO file
#   parse.md.qualtrics(filename)     # diagnoses a Qualtrics export file and creates md.define object
#   read.md.qualtrics(md.define)     # read data from a Qualtrics export

# AUGMENTING FOR ADAPTIVE METHOD
#   md.augment(md.define)            # add coded choice tasks if using the "chapman/bahna" adaptive method

# ESTIMATING MODELS
#   md.quicklogit(md.define)         # multinomial aggregate logit model estimation (fast)
#   md.hb(md.define, mcmc.iters)     # hierarchical Bayes estimation with individual-level estimates (rec'd but slow)

# WORKING WITH ESTIMATES
#   plot.md.range(md.define)         # plot overall mean & CI by item
#   plot.md.indiv(md.define)         # plot individual betas + overall mean
#   plot.md.heatmap(md.define)       # heatmap of utilities with biclustering
#   plot.md.group(md.define, var.grouping)     # compare mean utilities & CI by a grouping factor such as role
#
# MISCELLANEOUS
#   plot.md.relevant(md.define)      # if using "adaptive" method, plot relevant vs. unimportant items


#############################################################
#############################################################
#
# FUNCTIONS FOLLOW
#
# The functions should not require any editing.
#
# Note on plots: most plots here are ggplot2 objects. You can change their titles, labels,
# themes, etc., by adding standard ggplot2 elements to them (as shown in "maxdiff-examples.R").
#
#############################################################
#############################################################



###### TESTS for parse.md.cho
#
if (FALSE) {
  cho.file <- paste0("/Users/cchapman/Documents/Chris Documents/Virtual Machines.localized/",
                     "shared-from-vm/AutoFeatureMD/Job/",
                     "MaxDiffExport/MaxDiffExport.cho")
  csv.file <- paste0("/Users/cchapman/Documents/Chris Documents/Virtual Machines.localized/",
                     "shared-from-vm/AutoFeatureMD/Job/",
                     "Export/Export.csv")

  md.define           <- parse.md.cho(filename.cho=cho.file,
                                      filename.csv=csv.file)    # set up md.define from CHO file
  # check import of the CSV data
  str(md.define$md.csvdata)

  # fix missing item label (omitted from VAL file)
  md.define$md.item.names[15] <- "Assisted.Braking"
  test.read           <- read.md.cho(md.define)    # read the CHO data [Sawtooth]
  md.define$md.block  <- test.read$md.block        # save the data back into our study object

  md.define$md.model.logit <- md.quicklogit(md.define)  # aggregate logit model (fast check)
  summary(md.define$md.model.logit)
  md.plot.logit(md.define)                              # plot the logit model estimates

  test.hb <- md.hb(md.define, mcmc.iters = 5000)       # estimation (note: set mcmc.iters appropriately)
  for (i in 1:10) {
    Sys.sleep(1)     # let HB restart file catch up on disk
    test.hb <- md.hb(md.define, mcmc.iters = 2500, restart=TRUE)
  }
  md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
  md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw utilities by individual
  md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)
  rm(test.hb)
  summary(md.define$md.hb.betas.zc)

  plot.md.range(md.define)
  p <- plot.md.indiv(md.define)                         # create plot of HB model with individual mean betas
  p + theme_minimal()                                   # can use other ggplot functions like ggtitle()

}









#############################################################
# MAXDIFF CODE HISTORY
#
# started tracking with 0.51, February 14, 2018
#
# version 0.51
#    1. added optional restart capability to HB estimation -- will add new iterations to where previous run ended
# version 0.57
#    1. fixed issue with q.mdcols, where interstitial item columns were not correctly omitted from MaxDiff design matrix
# version 0.58
#    1. added item.order option to plot.md.group()
#    2. fixed bug where respondent ID showed up as an "item" in plot.md.group()
# version 0.59
#    1. added "item.order=[group]" option for plot.md.group()
# version 0.60
#    1. change default augmentation method to threshold method instead of grid expansion
# version 0.61
#    1. added function documentation to prep for reverse integration with Rcbc.R
# version 0.62
#    1. prepped for RI with Rcbc.R for OSS release
# version 0.63
#    1. detect and report row numbers of incomplete respondents in parse.md.qualtrics()
#    2. added parse.warnings to md.define, so errors in parse.md.qualtrics() can be reviewed later
#    3. color code final "ok" / "warning" message in parse.md.qualtrics()
# version 0.64
#    1. fixed calls to "error()" function
#    2. added density plots as default in plot.md.indiv()
# version 0.70
#    0. increment to 0.7 in light of significant sawtooth support added
#    1. work on read.md.sawtooth() to automate and check more for val/cho import
#    2. added parse.md.sawtooth() to create md.define automatically from cho file
#    3. minor updates to plot.md.indiv() for density plots
# version 0.71
#    1. locked default RNG seed in plot.md.indiv() to make jitter fill reproducible
#    2. updated some axis labels to be more consistent
#    3. removed dotted line at zero as defaults in plot.md.range(), plot.md.indiv()
#    4. fixed zero-coding bug for Qualtrics data import (when a Maxdiff item is coded as "0" in design == "worst")
#    5. fixed bug that referred to full data set instead of completes only, leading to errors with incomplete respondents

