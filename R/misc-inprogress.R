# EXPERIMENTAL Functions only


#############################################################
#
# EXPERIMENTAL VERSION
#
# parse.md.qualtrics.exp(file.qsv, itemSplit = "...-")
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
#   itemSplit     : token used to identify MaxDiff items in Qualtrics file
#                     required, but default "...-" should be OK
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

parse.md.qualtrics.exp <- function(file.qsv=NULL,
                               itemSplit = "...-",
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

  # which row has item labels?
  rowItems.found <- which(apply(md.all.raw, 1, function(x) { any(grepl(designHead, x, fixed=TRUE)) } ))[1]
  if (length(rowItems.found) < 1) {
    stop("No row with items names, separated", itemSplit, "found in", file.qsv)
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

  # in the MD labels, count how many times the post-split text appears.
  # For MD, must be 2 or more appearances (i.e., == # of task screens)
  # we will exclude all the ones that only appear once
  # a. get the post-split item labels

  md.itemraw  <- as.character(md.all.raw[rowItems.found, md.cols])
  md.itemtext <- strsplit(md.itemraw, itemSplit, fixed = TRUE)
  md.itemhead <- unlist(lapply(md.itemtext, function(x) x[[1]]))
  md.itemtext <- unlist(lapply(md.itemtext, function(x) x[[2]]))

  # b. count them
  md.tasksinferred <- as.numeric(names(which(table(table(md.itemtext))==md.items.n.inferred)))[1]
  md.whichtasks    <- names(which(table(md.itemtext)==md.tasksinferred))

  # c. keep only ones that match the inferred count of items
  md.labels <- unique(as.character(md.all.raw[rowItems.found, md.cols]))
  md.labels <- gsub("[[:space:]]", " ", md.labels)     # fix non-printing character issue

  md.labels.selected <- rep(FALSE, length(md.labels))
  for (j in seq_along(md.whichtasks)) {
    md.labels.selected <- md.labels.selected | grepl(md.whichtasks[j], md.labels, fixed = TRUE)
  }
  md.labels <- md.labels[md.labels.selected]

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
  md.labels <- unique(md.labels)    # and then update the labels

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

  # get the col names for the rows matching md.labels
  md.colnames.sel <- rep(FALSE, ncol(md.all.raw))
  for (j in seq_along(md.labels)) {
    md.colnames.sel <- md.colnames.sel | grepl(md.labels[j], md.all.raw[rowItems.found, ], fixed=TRUE)
  }
  md.colnames <- md.all.raw[rowItems.found, md.colnames.sel]

  # now just keep design.cols who match the md.colnames
  design.cols.keep <- rep(FALSE, length(design.cols))
  for (j in seq_along(design.cols)) {
    des.text <- sub("Display Order:", "", md.all.raw[rowItems.found, design.cols[j]], fixed = TRUE)
    des.text <- trimws(des.text)
    design.cols.keep[j] <- any(grepl(des.text, md.colnames, fixed=TRUE))
  }
  design.cols <- design.cols[design.cols.keep]

  # determine the number of sets (choice tasks) and report that
  # design.cols <- which(sapply(md.all.raw[rowItems.found, ],
  #                            function(x) grepl(headOrder, x, fixed=TRUE)))

  # remove any design matrix entries where item text !grep "itemConfirm"
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
  design.len <- design.len[rowSums(design.len) > 0, ]

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
  codeMin <- min(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.colnames.sel])), na.rm = TRUE)
  codeMax <- max(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.colnames.sel])), na.rm = TRUE)
  codeLen <- length(unique(na.omit(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.colnames.sel])))))

  cat("\n\nReviewing coded answers. Found min code (worst?) =", codeMin, "and max (best?) = ", codeMax, "\n")
  if (codeLen != 2) {
    warning("Found too many coded responses for Best/Worst; expecting only 2 different responses.")
    cat("WARNING on coded responses. Expecting 2 unique. Found:\n  ",
        unique(na.omit(as.numeric(as.matrix(md.all.raw[rowStart:rowEnd , md.colnames.sel])))),
        ".\nReview responses in columns:\n")
    print(which(md.colnames.sel))
    parse.warnings <- c(parse.warnings, paste("Found too many coded responses for Best/Worst"))
    all.OK <- FALSE
  }

  # check number of responses per respondentiPerTaskMed
  best.count  <- apply(md.all.raw[rowStart:rowEnd , md.colnames.sel], 1,
                       function(x) sum(na.omit(x)==codeMax))
  worst.count <- apply(md.all.raw[rowStart:rowEnd , md.colnames.sel], 1,
                       function(x) sum(na.omit(x)==codeMin))
  cat("Found average =", mean(best.count), "'best' answers, and average =", mean(worst.count), "'worst' answers.\n")

  count.OK    <- best.count==length(design.cols) & worst.count==length(design.cols)
  if (any(!count.OK)) {
    cat("\nRespondents with incomplete data (by respondent row number): ")
    inc.resp <- which(best.count != length(design.cols) | worst.count != length(design.cols))
    if (length(inc.resp) < 20) {
      cat(paste(inc.resp))
    } else {
      cat("[many respondents, starting with]", paste(head(inc.resp, 20)))
    }
    warning("You have some sets that don't match the expected number of answers! (respondents:", paste(inc.resp), ")")
    parse.warnings <- c(parse.warnings, paste("You have some sets that don't match the expected number of answers! (respondents:", paste(inc.resp), ")"))
    all.OK <- FALSE
  }
  cat("\nOf N =", length(count.OK), "total:\n  Found N =", sum(count.OK), "complete responses, and N =", sum(!count.OK),
      "with missing observations.\n")

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
    if (length(resp.rows) <= 50) {
      cat("  resp.rows        = c(", paste0(resp.rows, ","), "),\n")
    } else {
      cat("  resp.rows        = FIX DATA FILE ?,\n")
    }
  } else {
    cat("  resp.rows        = NULL ,\n")
  }

  cat("  md.item.k        =", length(md.labels), ",\n")
  cat("  md.item.tasks    =", length(design.cols),",\n")
  cat("  md.item.pertask  =", iPerTaskMed,",\n")

  cat("  q.startDesCol    =", min(design.cols),",\n")
  cat("  q.startMDcol     =", min(which(md.colnames.sel)),",\n")
  cat("  q.endMDcol       =", max(which(md.colnames.sel)),",\n")
  if(!all(diff(md.cols) == 1)) {
    cat("  q.mdcols         =c(", paste0(which(md.colnames.sel), ","), "),\n")
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
      q.startMDcol      = min(which(md.colnames.sel)),                      # the column where the MaxDiff responses begin. ASSUMES continuous in current version
      q.endMDcol        = max(which(md.colnames.sel)),                      # where the MaxDiff items end. See not #7 above!
      q.mdcols          = which(md.colnames.sel),
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

