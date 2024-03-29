% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxdiff-data-importexport.R
\name{read.md.qualtrics}
\alias{read.md.qualtrics}
\title{Read MaxDiff data for a study object from a Qualtrics QSV file}
\usage{
read.md.qualtrics(md.define, use.wd = FALSE)
}
\arguments{
\item{md.define}{A study object created by \code{parse.md.qualtrics} or
by hand}

\item{use.wd}{Whether the filename in \code{parse.md.qualtrics} needs to
be prepended with a working directory (also in \code{parse.md.qualtrics})
as opposed to being a fully specified path (FQDN).}
}
\value{
A list with \code{md.block} that contains appropriately formatted
data to be inserted back into the \code{md.design} study object.
}
\description{
This function will bring in MaxDiff data from a Qualtrics QSV file and
prepare it for plotting and estimation by other routines in this package.
However, Qualtrics's MaxDiff format is brittle and the data must be
exported following a very specific regimen.
See Chapter 10 of Chapman and Rodden, "Quantitative User Experience Research"
for more details, and see the data set in this package,
"qualtrics-pizza-maxdiff.csv" for an example.
}
\details{
Qualtrics can export data in many different ways, and in general it does
not label MaxDiff data in a systematic way.
To process such data, we need a standard format to be able to infer the
data setup.
This should be followed precisely. The requirements are:

1. When fielding the survey, all MaxDiff items must be REQUIRED so the
data are complete (respondents should not be able to get away from
making difficult tradeoffs; that's part of the choice model process).

2. In Qualtrics, choose to export the data as a CSV.

3. Convert categorical responses (e.g., "best") to numeric (e.g., 2).

4. Tell it include the randomized design order. This is required to
estimate the model and parse the data input.

5. Check that those all succeeded and that the data generally have a
sparse matrix (most columns are empty), that the "DISPLAY ORDER" columns
with the experimental design are present, and that the "best" and "worst"
choices are numerically coded.

Be sure to read all output carefully to make sure it matches your
expectations.
If the import fails, check the above requirements carefully and remove
any respondents with incomplete data. Check that the exported data
set matches the requirements.
Compare the "qualtrics-pizza-maxdiff.csv" data set in this package to
see whether your data appear similar in structure.
}
\seealso{
[parse.md.qualtrics] for details on setting up an
  \code{md.define} study object.
}
