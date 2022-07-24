# tests and examples for choicetools package
# work in progress

library(choicetools)

##### IN PROGRESS TESTS

################### CBC TESTS

set.seed(4567)       # make it reproduceable

# PART 1: create a CBC design and simulate some choices with it
# define a CBC attribute structure with 5 attributes, each with 2-7 levels
tmp.attrs  <- c(4, 4, 5, 2, 7)

# Create a CBC design matrix for that attribute structure.
# Not needed if you are importing data from Sawtooth Software or elsewhere
tmp.tab    <- generateMNLrandomTab(
  tmp.attrs,                       # our attribute structure
  respondents=200,                 # how many design blocks to create
  cards=3,                         # number of concepts per trial
  trials=8)                        # N of trials per design block

# convert those cards to a design-coded dummy matrix
tmp.des    <- convertSSItoDesign(tmp.tab)
# Now let's set up a test to see if we can recover the part worths ...
# make up some zero-sum part worths that we'll test against.
# This is not needed if you're using respondent data because the whole
# point then is to estimate the part worths, not make them up!
tmp.pws    <- generateRNDpws(tmp.attrs)
# Use those known part worths to pick winning cards in the design matrix
# i.e., simulate fake respondent choices so we can see if we recover the
# part worths in estimation. Again, only needed in simulation purposes.
# This uses a single vector of part worths for all respondents.
tmp.win    <- pickMNLwinningCards(tmp.des, tmp.pws, noise=TRUE)

# PART 2: estimate aggregate logit model and compare to known part worths
# estimate the MNL aggregate model
tmp.logit  <- estimateMNLfromDesign(tmp.des,tmp.win)
# compare raw values of PWs side by side
cbind(t(tmp.logit),tmp.pws)
# simple check of estimated PWs against the "real" pws; potential rescaling
cor.test(t(tmp.logit),tmp.pws)
# simple estimate of scaling factor between real and estimated PWs
tmp.scale <- median(t(tmp.logit)/tmp.pws)
# compare raw scores with simple adjustment of scale factor
cbind(t(tmp.logit),tmp.pws*tmp.scale)
plot(t(tmp.logit),tmp.pws*tmp.scale)

# PART 3: estimate individual-level HB estimates
# note that this is less automatable than aggregate models
# so you may want to dive into the estimateMNLfromDesignHB() code
#
# run HB model for the design & its winning cards
tmp.logitHB  <- estimateMNLfromDesignHB(tmp.tab, tmp.win,
                                        kCards=3, kTrials=8, kResp=200,
                                        mcmcIters=5000)
# get mean beta from the draws
tmp.meanbeta <- apply(tmp.logitHB$betadraw, 2, mean)
# check against the "real" pws (omit final zero-sum level of each attribute)
cor.test(tmp.meanbeta, tmp.pws[-cumsum(tmp.attrs)])
cbind(tmp.meanbeta, tmp.pws[-cumsum(tmp.attrs)],
      t(tmp.logit[-cumsum(tmp.attrs)]))
plot(tmp.meanbeta, tmp.pws[-cumsum(tmp.attrs)])

# PART 4: look at how the market simulation works.
# Use those part worths to estimate preference share for two products.
# First let's do fake "individual level" part worths based on the true values
#   (if you have real data, you'd estimate these with HB of course. This is
#   just for simulation purposes.)
# Set up the matrix for them ...
tmp.ind.pws <- matrix(0,ncol=sum(tmp.attrs),nrow=200)
# and then fill with part worths plus noise
for (i in 1:200) { tmp.ind.pws[i,] <- tmp.pws + rnorm(sum(tmp.attrs)) }
# and now the market simulation
# define the products according to their levels within attribute
tmp.prod1 <- c(1, 5, 10, 14, 17)     # define product 1
tmp.prod2 <- c(2, 6, 11, 15, 20)     # define product 2
# estimate share for those using the individual part worths created above
# in practice, you would use part worths from HB estimation instead
#   (from "estimateMNLfromDesignHB()" here, or from Sawtooth Software)
tmp.pref  <- marketSim(
  tmp.ind.pws,                     # matrix of individual-level utilities
  list(tmp.prod1, tmp.prod2),      # list of products to compare
  use.none=FALSE,                  # we have no "none" column
  use.error=TRUE, draws=20,        # add some noise and resample
  style="first")                   # estimate share by MNL approach
# see the overall preference share for the two products
colMeans(tmp.pref)
# result with seed 4567 as run directly from "set.seed" line above:
# 0.27  0.73

# Now the same thing with REAL HB data from HB estimation above
# get the individual-level data from the ChoiceModelR object
tmp.ind.pws2 <- extractHBbetas(tmp.logitHB, tmp.attrs)
# and repeat the simulation with that data
tmp.pref  <- marketSim(
  tmp.ind.pws2,                    # matrix of individual-level utilities
  list(tmp.prod1, tmp.prod2),      # list of products to compare
  use.none=FALSE,                  # we have no "none" column
  use.error=TRUE, draws=20,        # add some noise and resample
  style="first")                   # estimate share by MNL approach
# see the overall preference share for the two products
colMeans(tmp.pref)
# result:
# 0.32 0.68   # varies from above due to vagueries of ind-level estimation




## marketSim() demonstration code -- change the first line (and three lines after that, if needed) and then run the code inside the {} to see how the function works
if (FALSE) {
  some.pw.data <- matrix(rnorm(33*100), ncol=33)
  MY.PWS <- some.pw.data   ### REPLACE THE RIGHT-HAND SIDE WITH YOUR OWN PART WORTH SET

  # test/demonstrate IIA problem
  p1a <- c(3,11,21,31)   # "Red bus" defined as part worth columns 3, 11 etc
  p1b <- c(3,11,21,31)   # "Blue bus", identical to the "Red bus"
  p2  <- c(4,12,22,32)   # Competition

  ## logit shares -- show IIA problem when an identical product is introduced and takes too much share
  redbus     <- marketSim(MY.PWS,list(p1a, p2), use.none=FALSE, style="logit");          colMeans(redbus);
  redbluebus <- marketSim(MY.PWS,list(p1a, p1b, p2), use.none=FALSE, style="logit");     colMeans(redbluebus);

  ## first choice shares -- no IIA problem; share is split between identical products
  redbus     <- marketSim(MY.PWS,list(p1a, p2), use.none=FALSE, style="first");          colMeans(redbus);
  redbluebus <- marketSim(MY.PWS,list(p1a, p1b, p2), use.none=FALSE, style="first");     colMeans(redbluebus);

  ## roulette shares -- has some regression to IIA vs. first choice model and not deterministic due to random roulette draws
  redbus     <- marketSim(MY.PWS,list(p1a, p2), use.none=FALSE, style="roulette");       colMeans(redbus);
  redbluebus <- marketSim(MY.PWS,list(p1a, p1b, p2), use.none=FALSE, style="roulette");  colMeans(redbluebus);
}





#########################
# example code for writeCBCdesignCSV and readCBCchoices
#
if (FALSE) {
  # first let's source this file to get all the functions in memory
  if (FALSE) {   # DEV version
    current.wd <- "~/Documents/R/Rcbc/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
    source(paste(current.wd, "Rcbc-DEV.r", sep=""))
  } else {
    current.wd <- "~/Downloads/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
    source(paste(current.wd, "Rcbc.r", sep=""))
  }

  set.seed(4567)
  # define a CBC structure and create an experimental design matrix
  attr.list   <- c(3, 3, 5, 5, 4)   # note that this matches the list of labels below, so we know the structure
  tmp.tab     <- generateMNLrandomTab(attr.list,respondents=3,cards=3,trials=12)    # create random CBC design for the given list of attributes/levels
  tmp.des     <- convertSSItoDesign(tmp.tab)   # extended design file we'll use later for estimation

  # this example imagines we're doing a "designer USB flash drive"
  #
  # assign friendly names to the attributes and levels
  attr.names  <- c("Size", "Performance", "Design", "Memory", "Price")

  # suggest: avoid using commas in the strings. Seems OK but not thoroughly tested in CSV upload/download/reading/Drive/LibreOffice/R
  attr.labels <- c(
    "Nano", 			"Thumb", 				"Full-length",
    "Low speed",  "Medium speed", "High speed",
    "Tie-dye",    "Silver",       "Black",  		"White", 		"Prada",
    "1.0GB",      "8.0GB",        "16GB",   		"32GB",  		"256GB",
    "$9",   		 	"$29",  				"$59",   			"$89"
  )

  # write the CBC "survey" to a CSV
  current.wd <- "~/Desktop/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
  writeCBCdesignCSV(tmp.tab, attr.list=attr.list, lab.attrs=attr.names, lab.levels=attr.labels,
                    filename=paste(current.wd,"writeCBCtest.csv",sep=""), delimeter=",")

  # to upload the CSV to Drive spreadsheet:
  # 1. Go to Drive, and upload. Be sure to turn "Conversion" on.
  # 2. Open it. Select first column, and then shift+rightarrow to highlight others. Expand the column widths
  # 3. Suggest to center the text in columns B, C, D for easy reading

  ########### ... now go off and make some choices in the CSV file
  ########### save it and then come back here  to read your choices ...

  # to download from Drive:
  # 1. File | Download as ... Comma Separated Values
  current.wd <- "~/Downloads/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
  # from Google Docs:
  (tmp.win <- readCBCchoices(tmp.tab, filename=paste(current.wd, "writeCBCtest - Sheet 1.csv",sep="")))
  # from other CSV writer:
  (tmp.win <- readCBCchoices(tmp.tab, filename=paste(current.wd, "writeCBCtest.csv",sep="")))

  # expand to full length and fill in any missing values with random choices
  (tmp.win.exp <- expandCBCwinners(tmp.win))

  # estimate the utilities from the data
  tmp.pws <- estimateMNLfromDesign(tmp.des, tmp.win.exp)
  (tmp.res <- data.frame(attr=attr.labels, util=t(tmp.pws)[,1]))  # nicer formatting to import to a spreadsheet

  # bootstrap it to get some empirical confidence
  tmp.bs.log <- bootstrapMNLfromDesign(tmp.des,tmp.win.exp,cards=3,trials=12,bs.reps=20,bs.rescale=TRUE)       # estimate 20 bootstrap models  [in reality, should be 100-1000, not 10 !]

  # jitter the results to make better density plotting on chart
  # jitter the results aggressively since we have a small sample
  tmp.bs.log <- data.frame(apply(tmp.bs.log,2,jitter, factor=40))   # for larger sample, try jittering with factor=2 to 5

  colnames(tmp.bs.log) <- attr.labels    # make the var. names match the attribute friendly names
  summary(tmp.bs.log)

  # and plot the bootstrap
  require(ggplot2)
  require(reshape2)

  tmp.bs.log.melt <- melt(tmp.bs.log)    # reformat the data to be ggplot friendly

  # plot it!
  (p <- ggplot(tmp.bs.log.melt, aes(x=variable, y=value)) +
      geom_point(colour="dark blue", alpha=0.1, size=4) +
      stat_summary(fun.data = "mean_cl_normal", geom="linerange",
                   size=4.5, colour="darkred") +
      theme(axis.text.x=element_text(angle=-90, hjust=0, size=18))
  )
  # if you want to import it into a preso or something ...
  png(paste(current.wd,"p.png",sep=""), width=1200, height=400)  # or pdf, tiff, jpeg
  p
  dev.off()
}




#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

# CPM TESTS

# EXAMPLE CODE:
#   Suppose iris species are our "brands" and we examine their "positioning"
#     with regards to the other predictor variables:
if (FALSE) {
  data(iris)
  cpm.plot(iris, "Species", names(iris)[1:4], zoom.out=10,
           title.legend="Species")
  # the same thing rotated, in case you want a different orientation
  cpm.plot(iris, "Species", names(iris)[1:4], zoom.out=10,
           title.legend="Species", rotate = 90)
}


#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

### MAXDIFF TESTS

### Read sample "Pizza" data
library(choicetools)
md.define <- parse.md.qualtrics("./inst/extdata/qualtrics-pizza-maxdiff.csv",
                                returnList = TRUE)

test.read          <- read.md.qualtrics(md.define) # read Qualtrics data
md.define$md.block <- test.read$md.block        # save the data back into our study object

# check some data
md.define$md.block[1:10, c(1:3, 5, 6, 10, 13:16)]

### HB estimation and save the results
set.seed(98121)
test.hb <- md.hb(md.define, mcmc.iters = 25000)       # estimation (note: set mcmc.iters appropriately)
md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw utilities by individual
md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)
rm(test.hb)
summary(md.define$md.hb.betas.zc)


### Plots

# plot -- sample averages & CIs
plot.md.range(md.define) +
  theme_minimal() +
  xlab("Pizza Toppings")

# plot -- individual-level estimates & distribution
plot.md.indiv(md.define) +                       # create plot of HB model with individual mean betas
  theme_minimal() +
  ylab("Pizza Toppings")

