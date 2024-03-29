% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxdiff-plots.R
\name{md.plot.logit}
\alias{md.plot.logit}
\title{Plot aggregate logit model from quick estimation of MaxDiff utilities}
\usage{
md.plot.logit(md.define, item.disguise = FALSE)
}
\arguments{
\item{md.define}{A MaxDiff study object that contains aggregate
utility estimates in an object member called \code{$md.model.logit},
as estimated by \code{md.quicklogit()} in this package.}

\item{item.disguise}{Remove the item labels and number them generically.
This is included to make it easy to share and show results at a conference
or other audience when the underlying details are confidential.}
}
\value{
A ggplot2 object plotting the CI ranges for each item, as
estimated by \code{md.quicklogit()}.
}
\description{
This gives a quick plot of results from \code{md.quicklogit()} to check
whether one's data looks reasonable before running hierarchical Bayes
estimation. See \code{md.quicklogit()} for details.
A good alternative is a simple plot of best and worst counts,
as done by \code{plot.md.counts()}
}
