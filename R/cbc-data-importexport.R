# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# CBC data import-export

# FUNCTIONS
#   convertSSItoDesign()   : Import choices from Sawtooth Software TAB format
#   findSSIattrs()         : convenience function
#   writeCBCdesignCSV()    : Save a design matrix to a CSV file ()
#   readCBCchoices()


#############################################
# convertSSItoDesign()
#############################################
# function(df.in)
#
# Takes input in the form of a "TAB" matrix, e.g., from Sawtooth Software SMRT
# and converts it to a design-coded matrix
#
# E.g.:
# TAB INPUT
#   Att1    Att2    Att3
#   3       2       2
#   2       3       1
# DESIGN-CODED OUTPUT
#   Att1    Att2    Att3
#   0 0 1   0 1 0   0 1
#   0 1 0   0 0 1   1 0
#
# Parameters
#   df.in = matrix from TAB output of Attribute codes
#           do not include other columns such as ID and answer columns
#
# Sample code
#   # tab representation of levels for 2 attributes * 3 trials of 3 concepts
#   att.list <- data.frame(cbind(c(1,2,3,3,1,3,1,2,1), c(1,2,3,2,3,3,1,2,1)))
#   # get the dummy coded version
#   convertSSItoDesign(att.list)

convertSSItoDesign <- function(df.in, no.output=FALSE, none.col=NULL) {
  all.mat <- NULL
  name.vec <- NULL
  for (i in 1:ncol(df.in)) {                 # iterate all cols (attributes)
    # convert attribute column to a binary matrix of what was shown
    #
    # first, create a matrix of the right size
    att.max <- max(df.in[ , i])              # assumes levels occur as 1..max
    attmat1 <- matrix(0, nrow=length(df.in[,i]), ncol=att.max)

    # replace the matching elements with 1 to signify attribute presence
    attmat1[cbind( (1:length(df.in[,i])), df.in[,i]) ] <- 1

    # and add those columns to the master design matrix
    all.mat <- cbind(all.mat,attmat1)

    # create matching column names
    att.prefix <- paste0("ATT", i)
    if (!is.null(names(df.in))) {
      att.prefix <- names(df.in)[i]
    }
    att.names <- paste0(rep(att.prefix, att.max), "-", 1:att.max)
    name.vec <- c(name.vec,att.names)
  }
  all.mat <- data.frame(all.mat)
  names(all.mat) <- name.vec
  return(all.mat)
}


#############################################
# findSSIattrs
#############################################
# function(df.in)
#
# Takes input in the form of a "TAB" matrix, e.g., from Sawtooth Software SMRT
# and returns a list of attribute sizes (==max level in each attribute column)
# (written as a function here primarily for readability and convenience)
#
findSSIattrs <- function(df.in) {
  return(apply(df.in, 2, max))
}




#########################
# writeCBCdesignCSV()
#########################
#
# Takes a tab-formatted CBC design and writes it out to a CSV (actually tab-separated) file
# NOTE: current version assumes/requires that the design has equal trials & cards for all respondents
#
# PARAMETERS
#   tab.in     = the design matrix, e.g., as produced from generateMNLrandomTab()
#   filename   = the CSV to write out
#   overwrite  = whether to overwrite filename if it exists
#   cards      = number of concepts shown per trial
#   trials     = number of trials shown per respondent
#   n.resp     = number of respondents in the design file
#   start.resp = the first respondent to write to the file
#   end.resp   = the last respondent to write to the file
#   attr.list  = vector of the attribute sizes, e.g., c(3, 2, 4) for a CBC with 3 attributes and 3, 2, 4 levels for them respectively
#   lab.attrs  = vector of text labels to display for the attributes
#   lab.levels = vector of text labels for the attribute levels. Must have length == sum(attr.list)
#
# OUTPUT
#   the formatted CSV written to stdout() or filename
#
writeCBCdesignCSV <- function(tab.in=NULL, filename="", overwrite=FALSE, cards=3, trials=12,
                              n.resp=NULL, start.resp=1, end.resp=NULL,
                              attr.list=NULL, lab.attrs=NULL, lab.levels=NULL,
                              delimeter="\t") {

  require(digest) # to create a digest hash of the design matrix so we can make sure it matches when reading choices

  if (is.null(tab.in)) {
    stop ("writeCBCdesign: must supply a design to write out.")
  } else {
    if (is.null(n.resp)) {
      n.resp <- floor(nrow(tab.in) / trials / cards)
    }
  }
  if (filename=="") {
    file.con <- stdout()
  } else {
    if (!overwrite && file.exists(filename)) {
      stop("Output file already exists. Use 'overwrite=TRUE' if you wish to replace it.")
    } else {
      file.con <- file(filename, "w")
    }
  }
  if (is.null(end.resp)) {
    end.resp <- n.resp
  }
  if (is.null(attr.list)) {
    attr.list <- findSSIattrs(tab.in)
  }
  if (is.null(lab.attrs)) {
    lab.attrs <- paste("Attr", 1:length(attr.list))
  }
  if (is.null(lab.levels)) {
    lab.levels <- paste0("level", rep(1:length(attr.list), attr.list), "-", unlist(lapply(attr.list,seq)))
  }
  label.offsets <- c(0, cumsum(attr.list))[1:length(attr.list)]

  writeLines(paste("##############################\n","CBC response file for design: ", digest(tab.in), "\n", sep=""), file.con)

  for (resp in start.resp:end.resp) {
    # write respondent header
    writeLines("##############################", file.con)
    writeLines(paste("Respondent", resp, "\n"), file.con)

    # write each trial
    for (trial in 1:trials) {
      # construct the lines
      writeLines(paste("TRIAL:", trial), file.con)
      writeLines(delimeter, sep="", file.con)
      writeLines(paste("    ", 1:cards, delimeter), sep="", file.con)
      writeLines("", file.con)
      for (line in 1:length(attr.list)) {
        line.text <- paste0(lab.attrs[line], ":")
        for (card in 1:cards) {
          line.cardtext <- lab.levels[label.offsets[line] + tab.in[(resp-1)*cards*trials+(trial-1)*cards+card, line] ]
          line.text <- paste(line.text, delimeter, line.cardtext)
        }
        writeLines(line.text, file.con)
      }
      writeLines(paste0("\nCHOICE for Trial ", trial, ":\n\n"), file.con)
    } # for trial
  } # for resp
  if (filename != "") {
    close(file.con)
  }
}



#########################
# readCBCchoices()
#########################
#
# Reads the CBC winning concepts from a CSV file, in the format created by writeCBCdesignCSV()
#
# OVERVIEW:
# Reads a text file to extract CBC choices according to the following format:
# 	1. Design hash that matches the provided design matrix. Optional; silent if not present.
# 	2. Repondents identified by: "Respondent [x]"  (where [x] is an integer)
#   3. Trial idenfied by: "TRIAL: [t]"
#      Each trial identifier must be preceded (at some point) by an identified respondent [x]
#   4. Choice identified by: "CHOICE for Trial [t]: [c]"
#      Each choice must be preceded (since the previous choice) by an identified trial [t]
# All other content in the file (blank lines, attribute/feature lines, etc) is ignored
#
# PARAMETERS
# 	tab.in    = the tab-format design matrix that matches the text file
#   filename  = the text file to read choices from
#   cards     = number of concepts per CBC trial
#   trials    = number of responses per respondent in design block
#   cards.win = vector of winning cards (e.g., if you want to merge the results here with previous files)
#   verbose   = whether to echo various things along the way
# OUTPUT
#   vector of winning cards with one entry per CBC block (trial) in the text file

readCBCchoices <- function(tab.in=NULL, filename="", cards=3, trials=12,
                           cards.win=NULL, verbose=TRUE) {
  require(digest)
  require(stringr)
  if (is.null(tab.in)) {
    stop("Must supply a CBC tab format design matrix that matches the input file.")
  }
  if (filename=="") {
    stop("Must specify a file to read with CBC choices, as created by writeCBCdesignCSV()")
  }

  # preallocate matrix for choice winners, and integrate with cards.win if provided
  if (is.null(cards.win)) {
    n.choices <- nrow(tab.in) / cards
    new.cardswin <- rep(NA, n.choices)
  } else {
    n.choices <- length(cards.win)
    if (length(cards.win) != nrow(tab.in)/cards) {
      warning("Length of cards.win doesn't match length of design/cards.")
    }
    new.cardswin <- cards.win
  }

  # grab the entire set of responses
  file.con <- file(filename,"r")
  lines.in <- readLines(file.con)
  close(file.con)

  # read/dispatch loop to process the file
  resp.id  <- NA
  trial.id <- NA
  for (i in 1:length(lines.in)) {
    lines.in[i] <- gsub(","," ",lines.in[i])   # replace commas with whitespace, so will work with classic CSVs
    if (grepl("CBC response file for design:",lines.in[i], fixed=TRUE)) {
      # check digest
      digest.code <- str_trim(strsplit(lines.in[i], "CBC response file for design:", fixed=TRUE)[[1]][2])
      if (digest.code != digest(tab.in)) {
        warning("Design version (digest code) in response file does not match the provided Design matrix [", digest.code, "::", digest(tab.in),"].")
      }
    }
    if (grepl("Respondent",lines.in[i])) {
      # set resp.id
      resp.id  <- as.numeric(strsplit(lines.in[i], "Respondent", fixed=TRUE)[[1]][2])
      trial.id <- NA
    }
    if (grepl("TRIAL:",lines.in[i])) {
      trial.id <- as.numeric(strsplit(lines.in[i], "TRIAL:", fixed=TRUE)[[1]][2])
    }
    if (grepl("CHOICE for Trial",lines.in[i])) {
      # read trial, compare to trial.id
      # read choice, save to proper loc in new.cardswin
      choice.in <- as.numeric(strsplit(lines.in[i], ":", fixed=TRUE)[[1]][2])
      if (verbose) {
        cat("resp ", resp.id, ", trial ", trial.id, " == ",choice.in,"\n")
      }
      if (!is.na(choice.in)) {
        choice.index <- (resp.id-1) * trials + trial.id
        if (choice.index <= length(new.cardswin)) {
          # index is good, so check for existing data and then set the observed choice value
          if (!is.na(new.cardswin[choice.index])) {
            warning("Choice exists in provided cards.win for resp ",resp.id," trial ",trial.id,". Overwriting it.")
          }
          new.cardswin[choice.index] <- choice.in
        } else {
          warning("Choice index exceeds length of cards.win, at resp ",resp.id," trial ",trial.id,". Ignoring it.")
        }
      }
    }
  }

  return(new.cardswin)
}
