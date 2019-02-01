# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0
#
###################################
# choicetools 0.10
# Copyright 2013-2019 Google Inc.
#
# Marketing research tools for Choice-Based Conjoint Analysis and MaxDiff
# incorporates and deprecates previous "Rcbc.R" code release (to version 0.32)
#
# Authors: Christopher N. Chapman    Eric Bahna         James L. Alford          Steven Ellis
#          cchapman@google.com       bahna@google.com   jalford0974@yahoo.com
#
# Last update: January 23, 2019
# Version: 0.10



#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'



#################################################################
# BRIEF HOW TO USE
# 1. Source this whole file in R. It is not yet a formal package.
# 2. Search for "if (FALSE)" in this file and inspect those examples.
# 3. Be sure to read thoroughly, esp. the license and warnings, and test
#    thoroughly before using in any project. There is no warranty of any kind!
# 4. Most functions are documented with comments preceding their declarations.
#################################################################

# LICENSE (Apache 2.0)
# This software is licensed as free and open source under Apache License 2.0
# "You may reproduce and distribute copies of the Work or Derivative Works thereof
#    in any medium, with or without modifications, and in Source or Object form,
#    provided that ... "
# See the file "LICENSE for Rcbc.txt" for complete Apache license.

# REQUEST
# The authors kindly request citation when used for published work, either:
#   Chapman, C.N., Bahna, E., Alford, J.L., and Ellis, S. (2018). Rcbc: Marketing
#     research tools for choice-based conjoint analysis and maxdiff,
#     version 0.32. [R code]

rcbc.citation.string <- paste("Citation:\nChapman, C.N., Bahna, E., Alford, J.L., and",
    "Ellis, S. (2018). Rcbc: Marketing research tools for choice-based",
    "conjoint analysis and maxdiff, version 0.32. [R code]\n")

# TRANSITION FROM PREVIOUS LICENSE
#   This version of Rcbc is a derivative of previous versions that were offered
#   by the first two authors under the Creative Commons 3.0 license, per
#   citation and permission as provided here:
#     http://creativecommons.org/licenses/by/3.0/legalcode
#   Citation for previous version:
#     Chapman, C.N., and Alford, J.L. (2010). Rcbc: Choice-based conjoint and
#     multinomial logit estimation in R. Version 0.194. [R code]

# AUTHORS' CAUTION:
#   This code is for RESEARCH PURPOSES only and has no warranty.
#   It almost certainly contains large and small errors.
#   Evaluate it thoroughly for your own purposes. Read the code!

# REFERENCES:
# 1. Multinomial logit model maximum likelihood estimation algorithm based on:
#    Sawtooth Software (2009). Introduction to multinomial logit analysis.
#		 	[unpublished]
# 2. Composite product mapping based on:
#    Sawtooth Software (2004). The CPM system for composite product mapping,
#    technical paper series. Sawtooth Software, Sequim, WA.
# 3. Bahna, E, and Chapman, C. (2018). "Constructed, Augmented MaxDiff." In
#    Proceedings of the 2018 Sawtooth Software Conference, Orlando, FL.

# CHANGES IN VERSION 0.2
# 1. Added wrapper functionality to ChoiceModelR for easy hierarchical Bayes
#    estimates from regular-shaped (respondent*trial*concept) CBC data
# 2. Added writeCBCdesignCSV() and readCBCchoices() to mock up surveys easily
# 3. Fixed bug in market simulations with "use.error=TRUE" parameter, where
#      the added Gumbel error was too large in previous versions
# 4. Documentation for estimateMNLattrImpact() to estimate attribute impact in
#      CBC model
# 5. Renamed market simulation routine from "prodSelect()" to "marketSim()"


#######################
### FUNCTIONS INDEX ###
#
# PRIMARY FUNCTIONS:
#
#   CORE CONJOINT ESTIMATION
#
#     estimateMNLfromDesign()  :
#       estimates aggregate multinomial logit model from design matrix + winners
#       results should match estimation of "logit utility run" within
# 				Sawtooth Software SMRT
#       LIMITS: (a) only equal-sized blocks (cards*trials).
#               (b) must mark every card won/lost
#
#     estimateMNLfromDesignHB() :
#       estimates hierarchical Bayes model for design matrix + winners
#       yielding mean betas and individual draws (uses "ChoiceModelR"
#         package's adaptation of "bayesm" package for estimation)
#
#     extractHBbetas() :
#       gets individual-level mean beta from the HB estimation object's draws,
#         contained in the output object from estimateMNLfromDesignHB().
#       For market simulation with HB data, you could use either these
#         mean betas, or even better, use the full matrix of draws (collapse
#         the estimateMNLfromDesignHB() $betadraw object from 3d to 2d)
#
#     bootstrapMNLfromDesign() :
#       runs "estimateMNLfromDesign()" many times using resampled data
#       to establish the distribution of likely *aggregate* MNL models
#
#     marketSim()  :
#				Given part worths and a list of product definitions, estimate
#         preference share with first-choice or logit rule
#
#     estimateMNLattrImpact()  :
#       Estimate the effect that each attribute has on respondent choices
#       (technically, on the ability to predict those choices from utilities)
#       NOTE: experimental. See Chapman's poster from 2010 A/R/T Forum.
#
#     md.hb() :
#       estimate hierarchical Bayes model for MaxDiff data, with optional
#       data augmentation (see md.augment() and md.plot.* functions for more)
#
#   SURVEY ASSISTANCE
#     writeCBCdesignCSV()		:
#				Create a CSV version of a CBC survey for easy mockup and testing
#
#     readCBCchoices() :
#				Read the choices from a CSV file in the "writeCBCdesignCSV()" format
#
#   !!!!!!!!
#   WARNINGS: READ THESE
#     1. estimateMNLfromDesign() has been extensively tested for designs with
#				 k=3 concepts per trial. It is believed to work for k=2,4,5 but has not
#        been tested in as much depth.
#
#     2. Most functions here assume that ALL respondents have the SAME number
#        of trials and concepts per trial, i.e., "rectangular" data,
#        (which is generally the case in basic CBC models, e.g., from Sawtooth
#        Software CBC). In particular, this is a silent assumption of the
#        estimateMNLfromDesign() (i.e., aggregate logit model) function. If
#        this is not true for your data, then inspect the code for
#        estimateMNLfromDesignHB() and use it as a template to do
#        choicemodelr() in a way that fits your data. Test thoroughly!
#
#   OTHER UTILITY AND HELPER FUNCTIONS
#     convertSSItoDesign()     :
#       Converts Sawtooth Software choice-based conjoint (CBC) "TAB" file to
#       dummy-coded design matrix for estimateMNLfromDesign() aggregate model
#     generateMNLrandomTab()   :
#       Creates random CBC trials given a list of attribute levels
#       NOTE: designs are not "industry-strength". OK for demo/learning, but
#         are only minimally balanced.
#     pickMNLwinningCards()    :
#       Given design +  part worths, mark cards according to which win or lose
#     generateRNDpws()         :
#       Given list of attributes, return randomly generated zero-sum partworths
#     findSSIattrs()           :
#       Finds attribute structure list from a design file
#     fillCBCwinners()			   :
#       replaces any missing CBC choices with random choices (help with
#       estimateMNLfromDesign() if you have some incomplete cases)
#     expandCBCwinners()       :
#       converts (1:K) format to extended 1/0 vector of choices by card
#     cpm.plot() :
#       Plots a "perceptual map" style plot from multivariate brand ratings.
#			cpm.rotate()  				   :
#       rotates plot points around (0, 0)
#     cpm.se()								 :
#       simple utility function to calculate SEs and make code more compact
#     read.md.cho() :
#       read MaxDiff data from Sawtooth CHO format
#     read.md.qualtrics() :
#       read MaxDiff data from Qualtrics legacy export + random design format
#     md.augment() :
#       augment MaxDiff data per Bahna & Chapman (2018), Sawtooth Software Conf.
#     md.hb() :
#       estimate hierarchical Bayes model for MaxDiff data
#     md.quicklogit() :
#       fast estimation of aggregate logit model for MaxDiff, for quick test
#     plot.md.* () :
#       various plotting routines for MaxDiff results
#

#########################
### OVERVIEW OF USAGE ###
#
# 0. Source this file in R
#    -- AND THEN --
#    --  EITHER  --
# 1. Design & field CBC study in Sawtooth Software SSI/Web and import into
#      Sawtooth Software SMRT
# 2. From Sawtooth Software SMRT, export the design to a "TAB" file
# 3. Read TAB file into R as a simple CSV file
# 4. Use "convertSSItoDesign()" to convert that to a Dummy-coded design matrix
#
#    -- OR --
# 1-4. Supply your own design-coded file from another source --

#    -- OR --
# 1-4. Use "generateMNLrandomTab()" to create a random design simulation
#
#    -- AND AFTER ONE OF THOSE --
#    -- EITHER --
# 5. Use "estimateMNLfromDesign()" to estimate aggregate MNL part worths
#         ** NOTE: only works for 2-5 concepts per trial & same number of
#         **       trials for every respondent
#    -- OR --
# 5. Use "estimateMNLfromDesignHB()" to estimate part worths for HB model
#      if desired, or as needed for incomplete or variable length cases
#
#    -- AND THEN OPTIONALLY --
# 6. Once "estimateMNLfromDesign" works on your code, use
#      "bootstrapMNLfromDesign()" to estimate confidence intervals
#    -- AND / OR --
# 7. Use "marketSim()" to estimate preference share for product combinations
#
# For full examples, see sample code immediately below in "if (FALSE)" block,
# and in the "if (FALSE)" block following "readCBCchoices()"
#

###
### WISH LIST / TO DO LIST for Future versions ###
###
#
# DONE
# 1. Handle CBC designs for k=2-5 concepts per trial (works; needs more testing)
# 6. Bootstrap estimation of partworths with resampling
# 8. Provide a wrapper for HB estimation using Rossi/Allenby "bayesm" package

# FUTURE WISHES
# 2. Clean up coding and namespace usage
# 3. Include "none" parameter in HB (available in ChoiceModelR but not here yet)
# 4. Package as true R "package" instead of raw code
# 5. (deprecated as unnecessary)
# 7. Include error options elsewhere when relevant (e.g. "pickMNLwinningCards")
# 9. update generateMNLrandomTab() to be smarter about finding balanced vectors
# 10. refactor to use nice list structures instead of matrices
# 11. refactor to handle respondent/task labels instead of assuming them
#




###############################################
###############################################
#
#  END OF CBC FUNCTIONS
#
#  START OF MAXDIFF FUNCTIONS
#
###############################################
###############################################


#############################################################
# maxdiff-functions.R
#   (MaxDiff code subversion 0.62 for August 2018 patch)
#
# Chris Chapman, cchapman@google.com
# August 2018
#

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
# REQUIRED LIBRARIES (depending on which functions you use)
#   reshape2, ggplot2, mlogit, ChoiceModelR, Rmisc, matrixStats, superheat, corrplot
#   ==> be sure to install these first, e.g.:
#
if (FALSE) {
  install.packages(c("reshape2", "ggplot2", "mlogit", "ChoiceModelR", "Rmisc",
                     "matrixStats", "superheat", "corrplot"))
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
#


###############################################
###############################################
#
#  END OF CPM & CBC FUNCTIONS
#  END OF FILE FOR PATCHING/SHARING/RELEASE
#
###############################################
###############################################
