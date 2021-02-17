# Copyright 2019 Google LLC.
# SPDX-License-Identifier: Apache-2.0

# MaxDiff data parsing and input

# FUNCTIONS
#   preprocess.md.qualtrics : reformats data from various Qualtrics CSV formats to standard ("legacy") format
#   parse.md.qualtrics      : attempts to set up an "md.define" object automatically (alternative is to hand code one)
#                             based on reading and inferring structure from a Qualtrics CSV export
#   parse.md.cho            : attempts to set up "md.define" based on Sawtooth Software "CHO file" export
#   read.md.cho             : given an md.define object for Sawtooth Software, read its linked data and format appropriately for estimation
#   read.md.qualtrics       : given an md.define object for Qualtrics data, read its linked data and format appropriately for estimation


#############################################################
#
#   preprocess.md.qualtrics(dat)
#
#   checks for the qualtrics data format of "dat"
#   and returns a version that is formatted according to a standard format
#   specifically, what Qualtrics used to call "legacy, randomization" view
#
#   also to serve as a single placeholder for future updates & processing
#   to do: handle loop & merge data
#
# PARAMETERS
#   dat : data frame with raw CSV read from the parse.md.qualtrics() function
#
# RETURN
#   a data frame where column names & positions are reorgazized to
#   the standard format for parse.md.qualtrics()
#
# DETAILS
#   for all formats
#       1. replace item delimiter "-" or " - " with standardized "%-%"
#          which will avoid incorrectly parsing item text that includes hyphens
#   for "legacy + randomized" data:
#       1. no change, return as is after above
#   for the "modern + randomized" data
#       1. for all question text (row 2) with "* - Display Order", replace with "Display Order: *"
#       2. for all IDs (row 3) with "QID##_DO", replace with "DO-Q-Q##"
#       3. in all question text (row 2) replace " - " with "-" [should not be needed due to standard delimiter]
#       4. in all item IDs (row 3) replace "_" with "-"
#       5. find all columns with ID "DO-Q-Q*" and move to the right hand side of data frame
#   for files with loop and merge data:
#       to do, not supported currently
#############################################################

preprocess.md.qualtrics <- function(dat) {

  # constants used by Qualtrics that should be stable and not require change
  rowNames    <- 1         # line in the CSV with Qualtrics's names
  rowItems    <- 2         # line in the CSV with actual MaxDiff item text
  rowIntern   <- 3         # line in the CSV with Qualtrics's internal reference names

  #### updates that might apply to any format version
  #
  # 1. Delimiter
  #
  # Find item delimiter and replace it with a standard (and unlikely-to-be-used) delimiter, "%-%"
  # first, get a table of all the columns that have "Display Order" in them
  # so we can identify the most likely MaxDiff item wording (which should be repeated in multiple design order columns)
  do.tab <- table(as.character(sapply(dat[rowItems, grepl("Display Order", dat[rowItems, ], fixed=TRUE)], function(x) strsplit(x, "Display Order"))))

  # did we find anything?
  if (length(do.tab) < 1) {
    # we did not find any "Display Order" at all
    cat("Could not detect MaxDiff Display Order columns. Data may be missing the design matrices.\n")
  } else {
    # we did find at least one, so let's process the items
    # we will find the item names for the MaxDiff items and update those columns
    if (max(do.tab) < 2) {
      # we did not find any *repeated* items so give a warning. MaxDiff usually has multiple screens.
      cat("Maximum times any item was randomized was 1 time. This is not usually consistent with MaxDiff multiple tasks.\n")
    }
    # following is the item most likely to be MaxDiff, because its prefix occurs the most times
    #   (or is the first item among any that are tied for "most")
    # get its item prefix (everything before the "Display Order" part)
    max.item <- names(do.tab)[which(do.tab==max(do.tab))[1]]
    # now, which other columns have that item prefix in them?
    # those are presumably the MaxDiff item columns
    which.items <- grepl(max.item, dat[rowItems, ], fixed=TRUE)
    item.tails  <- gsub(max.item, "", dat[rowItems, which.items], fixed=TRUE)
    # from the item header, trim trailing spaces and '-' so we can replace
    # the separator between heading text and specific items using a standard delimiter
    max.item <- trimws(max.item, whitespace = "[ \t\r\n-]")
    # format the new item wording as the leading question, then delimiter, then the variable trailing items (MaxDiff list items)
    new.items   <- paste0(max.item, "%-%", item.tails)
    # and finally update our item text
    dat[rowItems, which.items] <- new.items
  }


  #
  # 2. Question numbering
  #
  # Use the question numbering as output by Qualtrics (ignore user question names for maxdiff items)
  # Replace labels in rowNames with those from rowIntern, for the MaxDiff items detected above.
  #
  # We do this because users may have chosen their own names for the MaxDiff items,
  # and that could confuse the parser. Safer to use the internal Qualtrics numbering.
  #
  # The code varies for "modern" and "legacy" formats.
  # We could do this below after detecting the format modern & legacy portions, but it is safe
  # to do it here, and isolates the code a bit more.

  # get the internal strings
  q.labels <- dat[rowIntern, which.items]

  # now remove the parts we don't need to get just the item names
  #
  # following will apply to "modern" format (and no effect otherwise)
  q.labels <- gsub('{"ImportId":"', "", q.labels, fixed=TRUE)
  q.labels <- gsub('"}', "", q.labels, fixed=TRUE)

  # following will apply to "legacy" format (and no effect otherwise)
  q.labels <- gsub("{'ImportId': '", "", q.labels, fixed=TRUE)
  q.labels <- gsub("}'", "", q.labels, fixed=TRUE)

  # applies to both formats
  q.labels <- gsub("QID", "Q", q.labels, fixed=TRUE)

  # replace the nominal item headers with the Qualtrics internal ones
  dat[rowNames, which.items] <- q.labels


  #### updates that vary according to the format
  # start by assuming that the format is unknown
  qt.version <- "unknown"

  # identify the Qualtrics data version
  # legacy == "{'ImportId': 'QID" is present in rowIntern, AND
  #           "{'ImportId': 'responseId'}" is present in rowIntern
  # modern == "{"ImportId":"_recordId"}" is present in rowIntern AND
  #           "{"ImportId":"QID" is present in rowIntern
  # backup if none of the above: assume based on 'QID vs "QID

  # legacy
  if (any(grepl("{'ImportId': 'QID", dat[rowIntern, ], fixed = TRUE)) &
      any(grepl("{'ImportId': 'responseId'}", dat[rowIntern, ], fixed = TRUE))) {
    qt.version <- "legacy"
    cat("Qualtrics 'legacy' file format detected. Parsing.\n")
  }
  # modern
  if (any(grepl('{"ImportId":"_recordId"}', dat[rowIntern, ], fixed = TRUE)) &
      any(grepl('{"ImportId":"QID', dat[rowIntern, ], fixed = TRUE))) {
    qt.version <- "modern"
    cat("Qualtrics 'modern' file format detected. Parsing.\n")
  }

  # for unknown format
  if (qt.version == "unknown") {
    # legacy
    if (any(grepl("{'ImportId': 'QID", dat[rowIntern, ], fixed = TRUE))) {
      qt.version <- "legacy"
      cat("Unclear format, but appears likely to be Qualtrics 'legacy' file format. Parsing.\n")
    }
    # modern
    if (any(grepl('{"ImportId":"QID', dat[rowIntern, ], fixed = TRUE))) {
      qt.version <- "modern"
      cat("Unclear format, but appears likely to be Qualtrics 'modern' file format. Parsing.\n")
    }

    # placeholder, nothing in particular to do for now; assume it is newly downloaded
    cat("Unclear file format. CSV may be missing too many columns or headers. Attempting to parse as modern format.\n")
    qt.version <- "modern"
  }

  # legacy format
  if (qt.version == "legacy") {
    # placeholder, nothing to do for now
    # in current version, we rewrite the data to match the legacy format
  }

  # modern format
  if (qt.version == "modern") {

    # 1. for all question text (row 2) with "* - Display Order", replace with "Display Order: *"
    # first identify
    # first split to get the question text
    newtext <- dat[rowItems, ]
    newtext <- sub("(.+) - Display Order", "Display Order: \\1", newtext)
    dat[rowItems, ] <- newtext

    # 2. for all IDs (row 3) matching "QID##_DO", replace with "DO-Q-Q##"
    newtext <- dat[rowIntern, ]
    newtext <- sub("QID([0-9]+)_DO", "DO-Q-Q\\1", newtext)
    dat[rowIntern, ] <- newtext

    # 3. do the same in column names (row 1), replacing "Q##*" with "DO-Q-Q##"
    newtext <- dat[rowNames, ]
    newtext <- sub("Q([0-9]+)_DO", "DO-Q-Q\\1", newtext)
    dat[rowNames, ] <- newtext

    # 4. in all question text (row 2) replace " - " with "-"
    # (should not be needed with the new standard delimiter, but just in case)
    newtext <- dat[rowItems, ]
    newtext <- sub(" - ", "-", newtext)
    dat[rowItems, ] <- newtext

    # 5. in all item IDs (row 3) replace "_" with "-"
    newtext <- dat[rowIntern, ]
    newtext <- sub("_", "-", newtext)
    dat[rowIntern, ] <- newtext

    # 6. replace all " with ' in rowIntern
    newtext <- dat[rowIntern, ]
    newtext <- gsub('"', "'", newtext, fixed=TRUE)
    dat[rowIntern, ] <- newtext

    # 7. replace "'ImportId':" with "'ImportId': " in rowIntern
    newtext <- dat[rowIntern, ]
    newtext <- sub("'ImportId':", "'ImportId': ", newtext)
    dat[rowIntern, ] <- newtext

    # 8. find all columns with ID "DO-Q-Q*" and move to the right hand side of data frame
    DOcols <- which(grepl("DO-Q-Q*", dat[rowIntern, ]))
    dat    <- cbind(dat[ , -DOcols], dat[ , DOcols])

  }
  return(dat)
}


#############################################################
#
# parse.md.qualtrics(file.qsv, itemSplit = "%-%")
#
# Status: incomplete, especially for handling incomplete or missing data.
#         believed to work OK for legacy-exported complete cases.
#
# Attempts to identify MaxDiff structure from a Qualtrics export file, and
# optionally returns an "md.define" object that you can use as a template
# for a study definition.
#
# Note: read the output carefully! If it doesn't match your expectation, you
#       will need to debug your data. See notes above about the common errors
#       in parsing Qualtrics files.
#
# PARAMETERS
#   file.qsv      : Qualtrics export file to process. Required.
#   itemSplit     : token used to identify MaxDiff items
#                   as of version 0.0.0.9076, this is set in preprocess.md.qualtrics()
#                   to a standard value of "%-%"
#   designHead    : a token Qualtrics uses in MaxDiff design matrices (and elsewhere)
#                     used with other indicators to infer MaxDiff columns
#   itemConfirm   : disambiguator in case "itemSplit" & designHead don't work;
#                     a string in your MD items, but not other items with "itemSplit"
#                     optional, change as needed
#   friendly.names: if you use this function to construct an md.define object,
#                     the short items names will be set to this vector;
#                     caution, must exactly match the # of MaxDiff items
#   returnList    : whether to create a list you can assign to md.define
#                     optional, will output a code snippet you can use if preferred

parse.md.qualtrics <- function(file.qsv=NULL,
                               itemSplit = "%-%",
                               designHead = "Display Order",
                               itemConfirm = NULL,
                               friendly.names = NULL,
                               returnList=FALSE) {

  library(matrixStats)
  all.OK <- TRUE
  if(is.null(file.qsv)) {
    stop("Must specify QSV filename to process.")
  }

  parse.warnings <- NULL
  file.name   <- file.qsv

  # constants used by Qualtrics but should be stable and not require change
  rowNames    <- 1         # line in the CSV with Qualtrics's names
  rowItems    <- 2         # line in the CSV with actual MaxDiff item text
  rowIntern   <- 3         # line in the CSV with Qualtrics's internal reference names

  # first read the data itself
  cat("Reading file:", file.name, "\n")
  md.all.raw <- read.csv(file.name, header=FALSE, stringsAsFactors=FALSE)

  # pre-process, to map Qualtrics updates to a single canonical format
  md.all.raw <- preprocess.md.qualtrics(md.all.raw)

  # which row has item labels?
  rowItems.found <- which(apply(md.all.raw, 1, function(x) { any(grepl(designHead, x, fixed=TRUE)) } ))[1]
  if (length(rowItems.found) < 1) {
    stop("No row with design matrices '", designHead, "' found in", file.qsv)
  }
  # print(head(md.all.raw))
  # print(rowItems.found)
  if (rowItems.found != rowItems) {
    warning("Row with item names, row =", rowItems.found, "does not match Qualtrics defined standard, row =", rowItems)
    all.OK <- FALSE
    parse.warnings <- c(parse.warnings, paste("Row with item names, row =", rowItems.found, "does not match Qualtrics defined standard, row =", rowItems))
  }


  # get plausible MD columns
  # ... 1: which cols have "Display Order" in label?
  # ... 2: extracting the "Qn" from #1, do the results have (mode) exactly 2 unique responses per respondent?
  # ... 3: extracting the item label (using itemSplit), are there 4 or more copies of the headers?

  design.cols <- which(sapply(md.all.raw[rowItems.found, ],
                              function(x) grepl(designHead, x, fixed=TRUE)))

  if (length(design.cols) < 1) {
    stop("No design columns found.")
  }
  design.df            <- data.frame(design.cols = design.cols)
  design.names         <- md.all.raw[rowNames, design.df$design.cols]
  # do any of the design matrix names lack the expected "-" ? (old files used ".")
  # if so, replace . with -
  if (any(!grepl("-", design.names, fixed=TRUE))) {
    cat("Found some design column headers with legacy Qualtrics names ... attempting to correct:\n")
    cat("Old:\n")
    fix.cols <- !grepl("-", design.names, fixed=TRUE)
    print(design.names[fix.cols])
    design.names         <- gsub(".", "-", design.names, fixed=TRUE)
    cat("New:\n")
    print(design.names[fix.cols])
    cat("\n")
  }

  # do any of the design matrix names begin with non-alphabetic character?
  #
  # TO DO
  #

  design.df$item.names <- unlist(sapply(design.names,
                                        function (x) lapply(strsplit(x, "-"),
                                                            function (x) x[length(x)])))
  design.df$col.count  <- NA

  # among all of the design columns, how often does each corresponding item header?
  for (i in 1:nrow(design.df)) {
    col.regex    <- paste0("^", design.df[i, "item.names"], "_")
    col.matching <- which(grepl(col.regex, md.all.raw[rowNames, ] ))
    # if not found, try alternative regex
    if (length(col.matching) == 0) {
      col.tail     <- tail(strsplit(design.df[i, "item.names"], "_")[[1]], 1)
      col.regex    <- paste0("^", col.tail, "_")
      col.matching <- which(grepl(col.regex, md.all.raw[rowNames, ] ))
    }
    design.df[i, "col.count"] <- length(col.matching)
  }

  find.mode <- function(x) {
    x.unique <- unique(x)
    x.unique[which.max(tabulate(match(x, x.unique)))]
  }

  md.items.n.inferred <- find.mode(design.df$col.count)

  # method above may fail, depending on format. If so, try different regex to match
  if (md.items.n.inferred == 0) {
    # TO DO
  }

  design.cols         <- design.df$design.cols[design.df$col.count==md.items.n.inferred]

  cat("File structure implies", md.items.n.inferred, "MaxDiff items.\n")

  item.names  <- design.df$item.names[design.df$col.count==md.items.n.inferred]
  item.regex <- paste0(item.names, "_", collapse="|")
  md.cols <- which(grepl(item.regex, md.all.raw[rowNames, ] ))

  # remove any columns where item text !grep "itemConfirm"
  if (!is.null(itemConfirm)) {
    md.cols <- md.cols[grepl(itemConfirm, md.all.raw[rowItems.found, md.cols], fixed = TRUE)]
  }
  if (!all(diff(md.cols) == 1)) {
    cat("==> Found non-sequential MaxDiff columns. Keeping following as MaxDiff columns:\n")
    cat(md.cols, "\n")
    warning("==> Columns with MaxDiff items were not sequential. Check output above. If you have interstitial items, this may be no problem.")
    all.OK <- FALSE
    parse.warnings <- c(parse.warnings, paste("==> Columns with MaxDiff items were not sequential. Check output above. If you have interstitial items, this may be no problem."))
  }

  # print(md.cols)
  # get the item names from those columns
  # determine the number of items
  md.labels <- unique(as.character(md.all.raw[rowItems.found, md.cols]))
  md.labels <- gsub("[[:space:]]", " ", md.labels)     # fix non-printing character issue

  # check labels to separate recurring from non-recurring portions
  # ... easiest if itemSplit occurs, so let's check that first
  if(all(grepl(itemSplit, md.labels, fixed=TRUE))) {
    md.labels <- unlist(lapply(md.labels, function(x) strsplit(x, itemSplit, fixed=TRUE)[[1]][2]))
  } else {
    # didn't find itemSplit everywhere, so we need to infer the splitting point
    # get first 2 adjacent item names
    cat("Item splitting token [", itemSplit, "] not found. Inferring MaxDiff item list.\n", sep="")
    cut.points <- rep(NA, length(md.labels)-1)
    for (j in 1:(length(md.labels)-1)) {
      name.1 <- md.labels[j]
      name.2 <- md.labels[j+1]
      i <- 1
      while (i < nchar(name.1) & i < nchar(name.2)) {
        if (substr(name.1, i, i+1) != substr(name.2, i, i+1)) {
          break
        }
        i <- i + 1
      }
      cut.points[j] <- i + 1
    }
    cut.point     <- min(cut.points)

    cut.problem <- cut.point > as.numeric(sapply(md.labels, function(x) nchar(x)))
    if (any(cut.problem)) {
      cat("Item split is dubious: Position", i, "in:", name.1, "--VS--", name.2, "")
      warning("Item label split is extremely dubious")
      parse.warnings <- c(parse.warnings, paste("Item split is dubious: Position", i, "in:", name.1, "--VS--", name.2))
    }
    md.labels <- as.character(sapply(md.labels, function(x) substr(x, cut.point, nchar(x))))
  }

  md.labels <- trimws(md.labels)    # fix bugs in case someone edits and reverses, but leaves whitespace
  md.labels <- unique(md.labels)                       # and then update the labels

  cat("Found K = ", length(md.labels), " MaxDiff items (unique column headers)\n", sep="")
  if (length(md.labels) != md.items.n.inferred) {
    cat("==>WARNING! File structure and item labels imply different item list length.\n")
    warning("File structure and item labels imply different item list length.")
    parse.warnings <- c(parse.warnings, paste("File structure and item labels imply different item list length."))
    all.OK <- FALSE
  }
  if (length(md.labels) < 10) {
    warning("Seemingly short list of labels (K=", length(md.labels), " items in list). Review item headers for correctness.")
    all.OK <- FALSE
    parse.warnings <- c(parse.warnings, paste("Seemingly short list of labels (K=", length(md.labels), " items in list). Review item headers for correctness."))
  }
  print(md.labels)

  # determine the number of sets (choice tasks) and report that
  # design.cols <- which(sapply(md.all.raw[rowItems.found, ],
  #                            function(x) grepl(headOrder, x, fixed=TRUE)))

  # remove any design matric entries where item text !grep "itemConfirm"
  if (!is.null(itemConfirm)) {
    design.cols <- design.cols[grepl(itemConfirm, md.all.raw[rowItems.found, design.cols], fixed = TRUE)]
  }

  cat("\nFound M =", length(design.cols), "screens of MaxDiff items per respondent. Columns with experimental design matrices are:\n")
  print(design.cols)
  if (length(design.cols) < 1 | length(design.cols) > 20) {
    warning("Questionable number of MaxDiff screens (is usually 5-15 tasks). Please confirm the design matrix.")
    all.OK <- FALSE
    parse.warnings <- c(parse.warnings, paste("Questionable number of MaxDiff screens (is usually 5-15 tasks)."))
  }
  if (!all(diff(design.cols) == 1)) {
    warning("==> Columns with design matrices (see above) are not sequential. Current code expects sequential columns.")
    parse.warnings <- c(parse.warnings, paste("Columns with design matrices (see above) are not sequential."))
    all.OK <- FALSE
  }

  # determine number of apparent responses and report that
  rowStart <- max(rowNames, rowItems, rowIntern) + 1
  rowEnd   <- nrow(md.all.raw)
  while(md.all.raw[rowEnd , 1] < " ") {     # wind back any blank lines at end
    rowEnd <- rowEnd - 1
  }

  cat("\nObservations are in rows ", rowStart, " to ", rowEnd, ". ", sep="")
  if (rowEnd < rowStart) {
    warning("Data rows are blank; end row is earlier than starting row.")
    all.OK <- FALSE
    parse.warnings <- c(parse.warnings, paste("Data rows are blank; end row is earlier than starting row."))
  }
  resp.rows <- rowStart:rowEnd

  cat("Found N =", rowEnd-rowStart+1, "respondents. Checking design matrices ...")

  # appears to be rowEnd - rowStart + 1 responses ... do they all have design matrices?
  design.df  <- data.frame(sapply(md.all.raw[rowStart:rowEnd, design.cols],
                                  function(x) strsplit(x, "|", fixed = TRUE)))

  design.len <- apply(design.df, c(1,2), function(x) length(unlist(x)))
  design.len <- data.frame(design.len)

  iPerTaskMin <- min(rowMins(as.matrix(design.len)))
  iPerTaskMed <- median(apply(design.len, 1, median))
  iPerTaskMax <- max(rowMaxs(as.matrix(design.len)))
  cat("\nFound ", iPerTaskMed, " (double-check: ", ifelse(iPerTaskMax==iPerTaskMin, "OK)", "ERROR)"), " items shown per task.", sep="")
  resp.anybad <- FALSE
  if (iPerTaskMin != iPerTaskMax) {
    cat("\nFound min", iPerTaskMin, "median", iPerTaskMed, "and max", iPerTaskMax, "entries in design matrices.\n")
    cat("Check the following respondents' design matrix entries. Suggest deleting these respondents: \n")
    bad.des <- apply(design.len, 1, function(x) any(x != iPerTaskMed))
    bad.df  <- design.len[bad.des, ]
    which.bad <- which(bad.des) + rowIntern
    resp.rows <- setdiff(resp.rows, which.bad)
    resp.anybad <- TRUE
    rownames(bad.df) <- which.bad
    print(bad.df)
    all.OK <- FALSE
    warning("Error: Design matrices are not all of equal length. Check FALSE entries in design columns as printed.")
    parse.warnings <- c(parse.warnings, paste("Error: Design matrices are not all of equal length."))
  }

  # determine the Qualtrics best and worst codes and report them
  codeMin <- min(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.cols])), na.rm = TRUE)
  codeMax <- max(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.cols])), na.rm = TRUE)
  codeLen <- length(unique(na.omit(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.cols])))))

  cat("\n\nReviewing coded answers. Found min code (worst?) =", codeMin, "and max (best?) = ", codeMax, "\n")
  if (codeLen != 2) {
    warning("Found too many coded responses for Best/Worst; expecting only 2 different responses.")
    cat("WARNING on coded responses. Expecting 2 unique. Found:\n  ",
        unique(na.omit(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.cols])))),
        ".\nReview responses in columns:\n")
    print(md.cols)
    parse.warnings <- c(parse.warnings, paste("Found too many coded responses for Best/Worst"))
    all.OK <- FALSE
  }

  # check number of responses per respondentiPerTaskMed
  best.count  <- apply(md.all.raw[rowStart:rowEnd , md.cols], 1,
                       function(x) sum(na.omit(x)==codeMax))
  worst.count <- apply(md.all.raw[rowStart:rowEnd , md.cols], 1,
                       function(x) sum(na.omit(x)==codeMin))
  cat("Found average =", mean(best.count), "'best' answers, and average =", mean(worst.count), "'worst' answers.\n")

  count.OK    <- best.count==length(design.cols) & worst.count==length(design.cols)
  if (any(!count.OK)) {
    cat("\nRespondents with incomplete data (by respondent row number): ")
    inc.resp <- which(best.count != length(design.cols) | worst.count != length(design.cols))
    cat(paste(inc.resp))
    warning("You have some sets that don't match the expected number of answers! (respondents:", paste(inc.resp), ")")
    parse.warnings <- c(parse.warnings, paste("You have some sets that don't match the expected number of answers! (respondents:", paste(inc.resp), ")"))
    all.OK <- FALSE
  }
  cat("\nOf N =", length(count.OK), "total:\n  Found N =", sum(count.OK), "complete responses, and N =", sum(!count.OK),
      "with missing observations.\n")


  # check whether there are items without any best or worst responses (and thus
  # a non-computable utility) and report how many, if so

  # TO DO


  # final report
  cat("\n=======\nSUMMARY\nReviewed file:", file.qsv)
  library(crayon)
  if (all.OK) {
    cat(green("\n\n==> Your data appear OK in this check."))
    cat("\n==> Following are suggested md.design entries:\n\n")
  } else {
    cat(red("\n==> WARNING!\nYour data failed some checks."))
    cat("Please check the warnings() and output as above.\nPending that, here are suggested md.design entries:\n")
  }

  cat("  file.wd          = NULL,\n")
  cat("  file.qsv         =", file.qsv,",\n")
  if(resp.anybad) {
    cat("  resp.rows        = c(", paste0(resp.rows, ","), "),\n")
  } else {
    cat("  resp.rows        = NULL ,\n")
  }

  cat("  md.item.k        =", length(md.labels), ",\n")
  cat("  md.item.tasks    =", length(design.cols),",\n")
  cat("  md.item.pertask  =", iPerTaskMed,",\n")

  cat("  q.startDesCol    =", min(design.cols),",\n")
  cat("  q.startMDcol     =", min(md.cols),",\n")
  cat("  q.endMDcol       =", max(md.cols),",\n")
  if(!all(diff(md.cols) == 1)) {
    cat("  q.mdcols         =c(", paste0(md.cols, ","), "),\n")
  }
  cat("  q.codeMDpos      =", codeMax,", # [double-check!]\n")
  cat("  q.codeMDneg      =", codeMin,"  # [double-check!]\n")

  if (is.null(friendly.names)) {
    friendly.names <- md.labels
  }

  if (!resp.anybad) {
    resp.rows <- NULL
  }

  if(returnList) {
    md.define.qt <- list(
      file.qsv         = file.qsv,
      resp.rows        = resp.rows,
      # REQUIRED: MAXDIFF DESIGN
      md.item.k        = length(md.labels),                  # total # of items on maxdiff list
      md.item.tasks    = length(design.cols),                # num of choice trials per respondent (max)
      md.item.pertask  = iPerTaskMed,                        # num of concepts shown in each trial
      # REQUIRED (PROBABLY): QUALTRICS FILE LAYOUT
      q.startDesCol     = min(design.cols),                  # the first column with design matrix (e.g., "6|3|2|9" or whatever)
      q.startMDcol      = min(md.cols),                      # the column where the MaxDiff responses begin. ASSUMES continuous in current version
      q.endMDcol        = max(md.cols),                      # where the MaxDiff items end. See not #7 above!
      q.mdcols          = md.cols,
      q.itemSplit       = itemSplit,                         # separator between Qualtrics header & actual MaxDiff item label
      q.removeInc       = TRUE,                              # remove respondents with any missing MaxDiff observations?
      q.codeMDpos       = codeMax,  # (usually 2, but varies) #  code for "winning" MD item, from Qualtrics
      q.codeMDneg       = codeMin, # (usually 1, but check)  #  code for "losing" MD item
      md.item.names     = friendly.names,
      parse.warnings    = parse.warnings
    )
    if(all(diff(md.cols) == 1)) {
      md.define.qt$q.mdcols <- NULL
    }

    cat("\nThis function is returning an md.define object\nExample code snippet to use it:\n")
    cat("  test.read.q        <- read.md.qualtrics(md.define)\n")
    cat("  md.define$md.block <- test.read.q$md.block\n")
    cat("  mod.logit          <- md.quicklogit(md.define)\n")
    cat("  summary(mod.logit)\n")
    return(md.define.qt)
  } else {
    if (all.OK) {
      cat("\nNext step: consider 'returnList=TRUE' parameter to create an md.define object from this function.\n")
      cat("Example code:\n")
      cat("  md.define          <- parse.md.qualtrics('", file.qsv, "', returnList=TRUE)\n", sep="")
    }
  }
  if (all.OK) {
    cat(green("\n\n==> Your data appear OK in this check. "))
    cat("See next steps listed above.\n")
  } else {
    cat(red("\n==> WARNING! Your data failed some checks."))
    cat("\nCheck the warnings() as shown, and check the detailed output above.\n")
  }
}


#############################################################
#
#   parse.md.cho(filename.cho [,]
#               [filename.val, filename.csv, friendly.names] )
#
#   reads a CHO file as exported by Sawtooth Software Lighthouse Studio
#   and imputes the structure for "md.define"
#
#   also attempts to
#
#   md.define            : the study definition object, used to locate the data file
#   opt.last.item.label  : optional name for the final item, if you're not using friendly names but
#                          have decided instead to use the names provided in a VAL file
#
#   Note: read the output carefully! If it doesn't match your expectation, you
#         will need to debug your data.
#
# PARAMETERS
#   filename.cho  : Sawtooth Software CHO file to process. Required.
#   filename.csv  : Sawtooth Software CSV export file. Optional.
#   friendly.names: if you use this function to construct an md.define object,
#                     the short items names will be set to this vector;
#                     caution, must exactly match the # of MaxDiff items


parse.md.cho <- function(filename.cho,
                         filename.val=NULL, filename.csv=NULL,
                         friendly.names=NULL,
                         opt.last.item.label="LAST ITEM (label not in VAL file") {

  all.ok <- TRUE                           # flag we'll set if errors are found

  ##################
  # 1. get CHO data
  cat("Reading CHO file:", filename.cho,"\n")
  md.all.raw  <- read.csv(filename.cho, nrows=50,
                          header=FALSE, stringsAsFactors=FALSE)

  ##################
  # 2. set up item names

  # do we have item names already specified in md.define?
  if (!is.null(friendly.names)) {     # item names are predefined, so ...
    md.names <- friendly.names

    # no, we don't, so we have to read or create them
  } else {                                     # read item names from VAL if possible

    # do we have a VAL filename specified? If not then ...
    if (is.null(filename.val)) {         # no we don't, so guess it from the CHO file name
      # determine VAL file name from CHO file name
      filename.val <- filename.cho
      filename.val <- gsub(".cho", ".val", filename.val, fixed=TRUE)
      filename.val <- gsub(".CHO", ".val", filename.val, fixed=TRUE)
    }

    # Now, does that file exist?
    if (file.exists(filename.val)) {     # YES, it does

      # YES, it does, so read the labels from it
      md.name.raw <- read.csv(filename.val, header=FALSE, stringsAsFactors=FALSE, sep="~")    # "sep="~" because want to read commas, etc.
      md.names    <- as.character(md.name.raw[seq(from=1, to=nrow(md.name.raw), by=2), 1])
      # note that the VAL file does not provide the label for the final MD item
      # so if you're relying on the VAL file labels, you may want to define this one yourself
      # ... but really it's better to define all the friendly names in setup (md.define$md.item.names) !
      md.names    <- c(md.names, opt.last.item.label)

      # NO, that file doesn't exist, so set placeholder names instead
    } else {                                  # if not defined and no VAL, just assign numbers to the names
      warning("Could not find VAL file: ", paste0(filename.val))
      md.names     <- NULL
      all.ok       <- FALSE
      filename.val <- NULL
    }
  }

  ##################
  # 3. add CSV if specified
  if (!is.null(filename.csv)) {
    if (file.exists(filename.csv)) {
      temp.csv <- read.csv(filename.csv)
      if (!is.null(temp.csv$sys_RespNum)) {
        temp.csv$resp.id <- temp.csv$sys_RespNum
      }
    } else {
      all.ok <- FALSE
      warning("Could not read CSV file:", filename.csv)
    }
  }

  ##################
  # 3. Infer MaxDiff setup from CHO header
  # NOTE: assumes all respondents are the same (as assumed throughout these scripts)

  # CHO file constants (cf. https://www.sawtoothsoftware.com/help/issues/ssiweb/online_help/hid_web_cbc_choformat.htm)
  cho.head.len <- 5      # vars on first line of a CHO. pos1=resp ID. pos3=kMDitems-1. pos4=kMDsets

  parse.warnings <- NULL
  # iterate over CHO until we find a header line (should be line 1, but just in case)
  i <- 1
  found.info <- FALSE
  while (!found.info & (i <= nrow(md.all.raw))) {
    # strip leading white space from line
    line.trim <- gsub("^\\s+|\\s+$", "", md.all.raw[i, ])

    # break the line into separate numbers, space-delimited
    line.data <- as.numeric(strsplit(line.trim, "[[:space:]]+")[[1]])

    if (length(line.data) == cho.head.len) {
      # found respondent header. Get the MaxDiff setup info and exit
      md.item.k     <- line.data[3] + 1
      md.item.tasks <- line.data[4] / 2
      found.info <- TRUE
    } else {
      i <- i + 1
    }
  }
  if (!found.info) {
    warning("Could not find MaxDiff setup info in cho file, ", filename.cho)
    all.ok <- FALSE
  } else {
    cat("\nFound", md.item.k, "MaxDiff items in item list.\nFound", md.item.tasks, "tasks per respondent.\n")
  }

  # now let's read 1 respondent and get the items per task
  if (found.info) {
    i <- 1
    found.info  <- FALSE
    lines.done  <- FALSE
    lines.found <- 0
    while (!lines.done & (i <= nrow(md.all.raw))) {
      # strip leading white space from line
      line.trim <- gsub("^\\s+|\\s+$", "", md.all.raw[i, ])

      # break the line into separate numbers, space-delimited
      line.data <- as.numeric(strsplit(line.trim, "[[:space:]]+")[[1]])

      if (length(line.data) == (md.item.k-1)) {
        # found respondent header. Get the MaxDiff setup info and exit
        found.info <- TRUE
        lines.found <- lines.found + 1
      } else if (found.info) {       # have found some, and now in new section, so we're done
        lines.done <- TRUE
      }
      i <- i + 1
    }
  }
  if (!found.info) {
    warning("Could not find MaxDiff setup info in cho file, ", filename.cho)
    all.ok <- FALSE
  } else {
    cat("Found", lines.found, "items per task.\n")

  }

  ##################
  # 4. set up the item names for md.define

  # check length of md.names to make sure it's what we expect
  md.names    <- gsub("<b>", "", md.names)
  md.names    <- gsub("</b>", "99", md.names)
  md.names    <- gsub("[[:punct:]]+", " ", md.names)
  md.names    <- gsub("[[:cntrl:]]+", " ", md.names)
  md.names    <- gsub("[[:space:]]+", ".", md.names)
  md.names    <- gsub("[[:punct:]]+", ".", md.names)     # remove multiple periods
  md.names    <- gsub("99.", "_", md.names)

  cat("\nItems found:\n")
  for (i in seq_along(md.names)) {
    cat(" ", md.names[i], "\n")
  }
  ##################
  # 5. format and return an md.define object with all the relevant info

  md.define.saw <- list(
    file.cho         = filename.cho,                       # CHO export from Sawtooth export jobs, required
    file.lab         = filename.val,                       # VAL file from that same export location, optional
    file.all         = filename.csv,                       # full CSV export, only needed if using adaptive

    # REQUIRED: MAXDIFF DESIGN
    md.item.k        = md.item.k,                          # total # of items on maxdiff list
    md.item.tasks    = md.item.tasks,                      # num of choice trials per respondent (max)
    md.item.pertask  = lines.found,                        # num of concepts shown in each trial
    md.item.names     = md.names,
    parse.warnings    = parse.warnings,

    # REFERENCE: CHOICE DATA USED IN ESTIMATION
    md.block         = NULL,                               # where we'll put choice data as it's read / augmented
    md.csvdata       = temp.csv,                           # where we'll hold other survey data, if needed
    md.nrow.preadapt = NULL
  )

  cat("\nThis function is returning an md.define object\nExample code snippet to use it:\n")
  cat("  test.read          <- read.md.cho(md.define)\n")
  cat("  md.define$md.block <- test.read$md.block\n")
  cat("  mod.logit          <- md.quicklogit(md.define)\n")
  cat("  summary(mod.logit)\n")
  return(md.define.saw)
}


#############################################################
#
#   read.md.cho(md.define, opt.last.item.label)
#
#   reads a CHO file as exported by Sawtooth Software Lighthouse Studio
#   and recodes it into the "md.block" format used by estimation functions here
#
#   md.define            : the study definition object, used to locate the data file
#   opt.last.item.label  : optional name for the final item, if you're not using friendly names but
#                          have decided instead to use the names provided in a VAL file

read.md.cho <- function(md.define,
                        opt.last.item.label="LAST ITEM (label not in VAL file") {

  ######
  # 1. get CHO data
  cat("Reading CHO file:", md.define$file.cho,"\n")
  md.all.raw  <- read.csv(paste0(md.define$file.wd, md.define$file.cho), header=FALSE, stringsAsFactors=FALSE)

  ######
  # 2. set up item names

  # do we have item names already specified in md.define?
  if (!is.null(md.define$md.item.names)) {     # item names are predefined, so ...
    md.names <- md.define$md.item.names

    # no, we don't, so we have to read or create them
  } else {                                     # read item names from VAL if possible

    # do we have a VAL filename specified?
    if (is.null(md.define$file.lab)) {         # no we don't, so guess it from the CHO file name
      # determine VAL file name from CHO file name
      val.filename <- md.define$file.cho
      val.filename <- gsub(".cho", ".val", val.filename, fixed=TRUE)
      val.filename <- gsub(".CHO", ".val", val.filename, fixed=TRUE)

      # yes we do have a VALU filename, so use that
    } else {
      val.filename <- md.define$file.lab
    }

    # Now, does that file exist?
    if (file.exists(paste0(md.define$file.wd, val.filename))) {     # YES, it does

      # YES, it does, so read the labels from it
      md.name.raw <- read.csv(paste0(md.define$file.wd, val.filename), header=FALSE, stringsAsFactors=FALSE, sep="~")    # "sep="~" because want to read commas, etc.
      md.names    <- as.character(md.name.raw[seq(from=1, to=nrow(md.name.raw), by=2), 1])
      # note that the VAL file does not provide the label for the final MD item
      # so if you're relying on the VAL file labels, you may want to define this one yourself
      # you can add it directly to the VAL file in last position ...
      # ... but really it's better to define all the friendly names in setup (md.define$md.item.names) !
      #
      # pad md.names IF the VAL file is missing 1 line
      # which is the default by Sawtooth
      #
      if (length(md.names) == md.define$md.item.k-1) {
        md.names    <- c(md.names, opt.last.item.label)
      }

      # NO, that file doesn't exist, so set placeholder names instead
    } else {                                  # if not defined and no VAL, just assign numbers to the names
      warning("Could not find VAL file: ", paste0(md.define$file.wd, val.filename), " so will use item names i1, i2, etc.")
      md.names <- paste0("i", 1:md.define$md.item.k)
    }
  }

  # check length of md.names to make sure it's what we expect
  if (length(md.names) != md.define$md.item.k) {
    warning("Expected ", md.define$md.item.k, " item labels in VAL file, but found ", length(md.names),
            ". Setting to proper length (REQUIRES CHECK).")
    md.names <- c(md.names, paste0("i", (length(md.names)+1):(length(md.names+1+md.define$md.item.k))))
    md.names <- md.names[1:md.define$md.item.k]
  }

  md.names    <- gsub("<b>", "", md.names)       # remove bold, italic, underline HTML tags
  md.names    <- gsub("</b>", "99", md.names)
  md.names    <- gsub("<i>", "", md.names)
  md.names    <- gsub("</i>", "99", md.names)
  md.names    <- gsub("<u>", "", md.names)
  md.names    <- gsub("</u>", "99", md.names)
  md.names    <- gsub("[[:punct:]]+", " ", md.names)
  md.names    <- gsub("[[:cntrl:]]+", " ", md.names)
  md.names    <- gsub("[[:space:]]+", ".", md.names)
  md.names    <- gsub("[[:punct:]]+", ".", md.names)     # remove multiple periods
  md.names    <- gsub("99.", "_", md.names)

  ######
  # 3. reshape CHO data to wide format
  md.block <- NULL         # where we will hold the data

  #    state variables -- we use a state machine to process the CHO
  #
  i             <- 1       # which row of the file are we on?
  data.line     <- 1       # where are we in the data frame we're creating?
  state.new     <- TRUE    # are we at the start of a respondent? (after one or before any)
  state.head    <- FALSE   # are we in a block header before any choice tasks?
  resp.num      <- NA      # which respondent are we processing, if any?
  state.block   <- FALSE   # are we in a BW choice block for the respondent?
  block.line    <- NA      # which line of a block are we on
  block.count   <- NA
  expect.attrs  <- NA      # how many attrs the CHO tells us to expect for a respondent
  expect.trials <- NA      # how many B/W trials a respondent block will have
  expect.conc   <- NA      # number of concepts in each trial
  saw.inc.block <- 0       # have we seen an incomplete block in the CHO? (warn and set)
  resp.counter  <- 0       # number of respondents processed
  warn.trials   <- FALSE   # have we warned about expected number of trials not matching CHO ?

  #    CHO file constants (cf. https://www.sawtoothsoftware.com/help/issues/ssiweb/online_help/hid_web_cbc_choformat.htm)
  cho.head.len <- 5      # vars on first line of a CHO. pos1=resp ID. pos3=kMDitems-1. pos4=kMDsets
  cho.task.len <- 2      # " " on line 3. pos1=kMDperset (concepts per trial)
  cho.choi.len <- 2      # " " for final line of CHO trial block (best or worst). pos1=concept that "won"

  #    iterate over CHO and build up choices, processing each line by state
  #
  cat("Reformatting respondent records ...\n")
  while (i <= nrow(md.all.raw)) {
    # strip leading white space from line
    line.trim <- gsub("^\\s+|\\s+$", "", md.all.raw[i, ])

    # break the line into separate numbers, space-delimited
    line.data <- as.numeric(strsplit(line.trim, "[[:space:]]+")[[1]])

    if (length(line.data) == cho.head.len) {
      # found respondent header. Save respondent number and continue.
      if (state.new) {
        state.new     <- FALSE
        state.head    <- TRUE
        resp.num      <- line.data[1]
        expect.attrs  <- line.data[3]
        expect.trials <- line.data[4]
        if (!warn.trials & expect.trials != md.define$md.item.tasks*2) {
          warning("Expected ", md.define$md.item.tasks, " trials but CHO defines ", expect.trials/2)
          warn.trials <- TRUE
        }
        state.block   <- TRUE
        resp.counter  <- resp.counter + 1
        if (resp.counter==1 | resp.counter %% 20 == 0) {
          cat("Reformatting respondent: ", resp.counter, " expecting ", expect.trials / 2, "trials.\n")
        }
      } else {
        warning("Line ", i, " apparent new respondent ", line.data[1], ", but not expected.")
      }

    } else if (state.block & state.head & length(line.data) == cho.task.len) {
      # start of best block, pick out the number of trials
      expect.conc   <- line.data[1]
      block.line    <- 0
      block.count   <- 1
      state.head    <- FALSE
      best.block    <- NULL

    } else if (state.block & (length(line.data) == (expect.attrs))) {
      # inside a best block
      block.line      <- block.line + 1
      if (block.line > expect.conc) {
        warning(paste("Line", i, "too many concepts observed",
                      expect.conc, "expected but", block.line, "observed : "),
                paste(line.data, collapse=" "))
      }
      best.block      <- rbind(best.block, line.data)

    } else if (state.block & !state.head & length(line.data) == cho.choi.len) {
      # get the winning concept from end of block
      block.data <- data.frame(win=0, resp.id=resp.num, best.block,
                               row.names=paste0("p", resp.num, "r", 1:nrow(best.block))) # FIX row names here
      best.win   <- line.data[1]
      if (best.win > 0) {
        block.data[best.win, "win"] <- 1      # mark the actual winning row, if there is one
      } else if (saw.inc.block==0) {
        warning(paste("Line", i, "marks an incomplete block (no winning concept). \nNormal if incomplete respondents area included. Suppressing additional warnings."))
        saw.inc.block <- 1
      } else if (saw.inc.block > 0) {
        saw.inc.block <- saw.inc.block + 1
      }

      if (is.null(md.block)) {      # first time, so create it and pre-allocate more than we need
        cat ("Preallocating data frame ...\n")
        md.block <- block.data
        md.preal <- block.data[rep(1, nrow(md.all.raw)), ]
        # str(md.preal)
        md.preal[1:nrow(md.preal), 1:ncol(md.preal)] <- NA
        # str(md.preal)
        md.block <- rbind(md.block, md.preal)
        # str(md.block)
        data.line <- data.line + nrow(block.data)
      } else {
        if (best.win > 0) {          # only add data for complete blocks with winners
          md.block[data.line:(data.line+nrow(block.data)-1), ] <- block.data
          data.line <- data.line + nrow(block.data)
        }
      }

      best.block <- NULL
      state.head <- TRUE
      block.line <- 0
      if (block.count >= expect.trials) {      # should get expect.trials
        # last trial
        state.block  <- FALSE
      } else {
        block.count <- block.count + 1
      }

    } # end of all possible states

    i <- i + 1         # advance to next line
    state.new <- TRUE  # for now. To do: switch after updating block.data; helpful for error check
  }

  cat("Done. Read",resp.counter,"total respondents.\n")

  if (saw.inc.block > 0) {
    warning("Observed ", saw.inc.block, " incomplete maxdiff response blocks. \nNormal if you've included incomplete respondents.")
  }

  # cut the over-allocated data frame down to the actually observed size
  cut.data.at <- min(which(is.na(md.block[ , 1])))   # the first row that has NA (preallocated) data
  md.block    <- md.block[1:(cut.data.at-1), ]       # take the data frame up to there

  # # basic data checks
  #   to do:
  #           automate these and warn
  # str(md.block)         # should have expected columns / items
  # head(md.block, 20)    # check choice structure, starting with 1s etc
  # tail(md.block, 20)    # check structure, ending with final respondent correctly

  # check structure is what we expect from constants
  #
  total.resps <- length(unique(md.block$resp.id))
  if (nrow(md.block) == total.resps * expect.trials * expect.conc) {
    cat("Woohoo! Your data with", nrow(md.block), "rows matches expected rows, concepts, & trials for complete data :)\n")
  } else {
    cat("WARNING: Expected", total.resps * expect.trials * expect.conc,
        "rows, but found", nrow(md.block), ".\n",
        "This is normal in case of incomplete respondents; otherwise should be investigated.\n")
  }

  # now add the omitted final column where needed (CHO for K items has only K-1 columns exported)
  #

  # which rows need a final column? those with no other attribute specified
  md.last.rows <- which(apply(md.block, 1, function(x) sum(x[3:length(x)]))==0)   # all 0 --> row for dropped level
  summary(md.last.rows)

  # now figure out whether it should be 1 or -1 ...
  # NB: this only works because we assume exact & identical TRIALS per block
  #
  # TO DO: if exact block structure above is refactored, would need to change this
  #        (presumably add state for best/worst and use that within block processing)
  md.posneg    <- rep(rep(c(rep(1, expect.conc), rep(-1, expect.conc)), expect.trials/2), total.resps)
  md.last.col  <- rep(0, nrow(md.block))
  md.last.col[md.last.rows] <-  md.posneg[md.last.rows]
  md.block <- cbind(md.block, md.last.col)

  # set names
  rownames(md.block) <- NULL
  md.block <- data.frame(md.block)
  if (FALSE) {                            # just keep "i.." names for now
    names(md.block)[3:ncol(md.block)] <- paste0("i",(1:md.define$md.item.k))
  } else {
    names(md.block)[3:ncol(md.block)] <- md.names[1:(length(md.names))]
  }
  # head(md.block, 40)
  # library(car)
  # car::some(md.block, 40)

  # TO DO: make sure md.block names are all legal R variable names

  # check (possible) Best / Worst levels in the data
  # --> should be *exact* match within level for -1 vs 1; and similar frequencies across levels
  #
  # to do:
  #         automate this, check equivalence and warn if not

  # apply(md.block, 2, table)   # see if we're balanced within & across levels

  # now cast the blocks into conditional format
  md.block$choice.coded                         <- md.block$win
  md.block$choice.coded[which(md.block$win==1)] <- 'yes'   # recode 1's into yes
  md.block$choice.coded[which(md.block$win==0)] <- 'no'    # recode 0's into no
  md.block$choice.coded                         <- as.factor(md.block$choice.coded)

  return(list(md.block=md.block, md.item.names=md.names))
}


#############################################################
#############################################################

#############################################################
#
#   read.md.qualtrics(md.define, use.wd)
#
#   reads a Qualtrics "Legacy Exporter | Legacy view off | Export Randomized Order" CSV file
#   and recodes the data into "md.block" format suitable for later estimation.
#
#   NOTE: there are two extremely common problems with the Qualtrics data exports:
#         1. not exporting the randomized order correctly (must be sparse data format from legacy exporter)
#         2. including incomplete respondents. currently this script breaks for incompletes. Exclude them beforehand
#
#   md.define            : the study definition object, used to locate the data file
#   use.wd               : whether to prepend the defined working directory to the file name
#

read.md.qualtrics <- function(md.define, use.wd=FALSE) {

  file.name   <- ifelse(use.wd, paste0(md.define$file.wd, md.define$file.qsv),
                        md.define$file.qsv)

  # other constants used by Qualtrics but should be stable and not require change
  rowNames    <- 1         # line in the CSV with Qualtrics's names
  rowItems    <- 2         # line in the CSV with actual MaxDiff item text
  rowIntern   <- 3         # line in the CSV with Qualtrics's internal reference names
  n.resp      <- NULL      # number of respondents; leave NULL to set this automatically (or set lower if desired)

  # first read the data itself
  # md.all.raw <- read.csv(file.name, skip=max(rowItems, rowIntern), header=FALSE, stringsAsFactors=FALSE)
  md.all.raw <- read.csv(file.name, header=FALSE, stringsAsFactors=FALSE)

  # pre-process, to map Qualtrics updates to a single canonical format
  md.all.raw <- preprocess.md.qualtrics(md.all.raw)
  # now drop the header rows, don't need them after pre-process, because
  # they are in md.define
  md.all.raw <- md.all.raw[-(1:max(rowItems, rowIntern)), ]

  # keep only the headers plus designated respondent rows
  if (!is.null(md.define$resp.rows)) {
    md.all.raw <- md.all.raw[md.define$resp.rows-max(rowItems, rowIntern), ]
    cat("Retaining only specified resp.rows =", md.define$resp.rows, "\n")
  }
  if (!is.null(md.define$q.mdcols)) {
    md.all.raw <- md.all.raw[ , c(md.define$q.mdcols,
                                  md.define$q.startDesCol:(md.define$q.startDesCol+md.define$md.item.tasks-1))]
    cat("Retaining only specified MaxDiff columns =", md.define$q.mdcols, "\n")
    md.define$q.startMDcol <- 1
    md.define$q.endMDcol   <- length(md.define$q.mdcols)
  }

  # use friendly names if they're provided
  if (!is.null(md.define$md.item.names)) {
    md.item.text <- md.define$md.item.names
  } else {
    # if item names are undefined, get them from the file (splitting on "itemSplit")
    # read the item labels from file
    md.item.text.raw <- read.csv(file.name, skip=rowItems-1, nrows=1, header=FALSE, stringsAsFactors=FALSE)
    if (!is.null(md.define$q.mdcols)) {
      md.item.text.raw <- md.item.text.raw[, c(md.define$q.mdcols,
                                               md.define$q.startDesCol:(md.define$q.startDesCol+md.define$md.item.tasks-1))]
    }

    md.item.text     <- md.item.text.raw[md.define$q.startMDcol:(md.define$q.startMDcol+md.define$md.item.k-1)]
    # split them and assign as item names
    md.item.text <- unlist(lapply(md.item.text, function(x) strsplit(x, md.define$q.itemSplit, fixed=TRUE)[[1]][2]))
  }
  # names(md.item.text) <- paste0("v", 1:length(md.item.text))   # ??????

  # check for complete response sets and optionally drop incompletes
  #
  best.count  <- apply(md.all.raw[ , md.define$q.startMDcol:md.define$q.endMDcol], 1,
                       function(x) sum(na.omit(x)==md.define$q.codeMDpos))
  worst.count <- apply(md.all.raw[ , md.define$q.startMDcol:md.define$q.endMDcol], 1,
                       function(x) sum(na.omit(x)==md.define$q.codeMDneg))
  cat("Found average =", mean(best.count), "'best' answers, and average =", mean(worst.count), "'worst' answers.\n")

  count.OK    <- best.count==md.define$md.item.tasks & worst.count==md.define$md.item.tasks
  if (any(!count.OK)) warning("You have some sets that don't match the expected number of answers!")
  cat("Of N =", length(count.OK), "total:\nFound N =", sum(count.OK), "complete responses, and N =", sum(!count.OK),
      "with missing observations.\n")

  if (md.define$q.removeInc) {
    cat("Recoding", sum(count.OK), "complete responses. (Dropping", sum(!count.OK), "incomplete.) ... ")
    if (sum(count.OK) != length(count.OK)) warning ("Incomplete respondents dropped.")
  } else {
    cat("Recoding", sum(count.OK), "responses (including", sum(!count.OK), "incomplete.) ... ")
    count.OK <- rep(TRUE, nrow(md.all.raw))
  }

  # if you want to do anything else to check the data, go ahead here by adjusting "count.OK" ...
  # .. and then:
  md.data <- md.all.raw[count.OK, ]    # keep the good responses (hopefully all of them!)

  if (is.null(n.resp)) {
    n.resp <- nrow(md.data)            # set the N of responses
  }

  # STEP 3: RESHAPE THE DATA TO HAVE VERBOSE STACKED FORMAT
  # see http://surveyanalysis.org/wiki/Analyzing_Max-Diff_Using_Standard_Logit_Models_Using_R for reference on format

  # set up a matrix to hold the recoded data
  # first, identfiers. If you change these, also change "k.startCol" below:
  md.data.stack <- data.frame(win      = 0,
                              resp.id  = rep(1:n.resp, each=md.define$md.item.tasks*md.define$md.item.pertask*2),
                              Block    = rep(1:md.define$md.item.tasks, n.resp, each=md.define$md.item.pertask*2),
                              Set      = factor(rep(c("Best", "Worst"), each=md.define$md.item.pertask, times=n.resp*md.define$md.item.tasks)),
                              sys.resp = "")
  # second, placeholder for the design matrix
  md.data.stack <- cbind(md.data.stack[ , 1:2], matrix(0, ncol=md.define$md.item.k, nrow=nrow(md.data.stack)), md.data.stack[ , 3:ncol(md.data.stack)])
  k.startCol    <- 3    # where the MaxDiff matrix actually starts in md.data.stack; will be OK unless you change -3 lines above

  md.data.zero <- md.data                  # a copy where "nothing" is shown; starting point for actual design matrix
  md.data.zero[is.na(md.data.zero)] <- -Inf   # dummy code for missing data (need a value we can run with a comparator but QT doesn't export)

  ### create match for design matrix coding into actual columns within a task set
  # 1. what are the question names that appear in the file?
  md.item.qnames <- read.csv(file.name, skip=rowNames-1, nrows=1, header=FALSE, stringsAsFactors=FALSE)
  if (!is.null(md.define$q.mdcols)) {
    md.item.qnames <- md.item.qnames[, c(md.define$q.mdcols,
                                         md.define$q.startDesCol:(md.define$q.startDesCol+md.define$md.item.tasks-1))]
  }
  # print(md.item.qnames)

  # 2. which of those are MD items?
  md.item.qnames <- md.item.qnames[md.define$q.startMDcol:(md.define$q.startMDcol+md.define$md.item.k-1)]
  # print(md.item.qnames)

  # of those, what are the design column indicators?
  md.item.qnames.des <- as.numeric(unlist(sapply(md.item.qnames, function(x) tail(unlist(strsplit(x, "_")), 1))))
  # print(md.item.qnames.des)
  if (anyDuplicated(md.item.qnames.des) > 0) {
    cat("Found duplicated MaxDiff item suffixes (e.g., like Q11_2 and Q11_2 again).\nData probably incorrectly read here.\n")
    cat("Duplicates start at putative MaxDiff item:", anyDuplicated(md.item.qnames.des), "\n")
    warning("Duplicated MaxDiff item suffixes. Read diagnostics above.")
  }

  if (!is.null(md.define$q.mdcols)) {
    md.define$q.startDesCol <- length(md.define$q.mdcols) + 1
  }


  for (i in 1:nrow(md.data.zero)) {                  # iterate over respondents
    if (i %% 100 == 0) {
      # show progress
      cat (i, " ", sep="")
    }
    for (j in 1:md.define$md.item.tasks) {                           # iterate over the randomized design sets shown, within respondent
      desSpec   <- md.define$q.startDesCol + j - 1
      desCols   <- as.numeric(unlist(strsplit(md.data.zero[i, desSpec], "|", fixed=TRUE)))

      offsetCol <- md.define$q.startMDcol + (j-1) * md.define$md.item.k - 1

      # cat(desCols, "::", offsetCol, "::", desCols + offsetCol, "\n",  sep=" ")
      # cat(unlist(md.data[i , desCols + offsetCol]), "\n")

      for (k in seq_along(desCols)) {         # code "best" cases
        rowOffset <- (i-1)*md.define$md.item.tasks*md.define$md.item.pertask*2 + (j-1)*md.define$md.item.pertask*2 + k
        md.data.stack[rowOffset, 2+which(md.item.qnames.des==desCols[k])] <- 1
        if (md.data.zero[i, which(md.item.qnames.des==desCols[k])+offsetCol] == md.define$q.codeMDpos) {
          md.data.stack[rowOffset, "win"] <- 1
        }
      }
      for (k in seq_along(desCols)) {         # code "worst" cases
        rowOffset <- (i-1)*md.define$md.item.tasks*md.define$md.item.pertask*2 + (j-1)*md.define$md.item.pertask*2 + k + md.define$md.item.pertask
        md.data.stack[rowOffset, 2+which(md.item.qnames.des==desCols[k])] <- -1
        if (md.data.zero[i, which(md.item.qnames.des==desCols[k])+offsetCol] == md.define$q.codeMDneg) {
          md.data.stack[rowOffset, "win"] <- 1
        }
      }
    }
  }

  # assign names
  md.item.text <- gsub("[[:space:][[:punct:]]", ".", md.item.text)
  md.item.text <- gsub("[[:punct:]]+", ".", md.item.text)     # remove multiple periods

  names(md.data.stack)[k.startCol:(k.startCol-1+md.define$md.item.k)] <- md.item.text

  # now cast the blocks into conditional format
  md.data.stack$choice.coded                       <- md.data.stack$win
  md.data.stack$choice.coded[md.data.stack$win==1] <- 'yes'   # recode 1's into yes
  md.data.stack$choice.coded[md.data.stack$win==0] <- 'no'    # recode 0's into no
  md.data.stack$choice.coded                       <- as.factor(md.data.stack$choice.coded)

  cat("done.\n")

  # make sure md.data.stack names are all legal R variable names
  # to do: shorten into a single sub() line :)  not enough time today
  check.names <- names(md.data.stack)
  if (any(!grepl("^[a-zA-Z\\.]", check.names))) {
    names.fix <- which(!grepl("^[a-zA-Z\\.]", check.names))
    check.names[names.fix] <- paste0("Q", check.names[names.fix])
  }
  names(md.data.stack) <- check.names

  return(list(md.block=md.data.stack))
}
