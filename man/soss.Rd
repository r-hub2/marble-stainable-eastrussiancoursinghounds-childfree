% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soss.R
\name{soss}
\alias{soss}
\title{Read and recode Michigan State of the State (SOSS) data}
\usage{
soss(waves, extra.vars = NULL, progress = TRUE)
}
\arguments{
\item{waves}{vector: a numeric vector containing the SOSS waves to include (currently available: 79, 82, 84, 85, 86)}

\item{extra.vars}{vector: a character vector containing the names of variables to be retained from the raw data}

\item{progress}{boolean: display a progress bar}
}
\value{
A data frame containing variables described in the codebook available using \code{vignette("codebooks")}
}
\description{
Read and recode Michigan State of the State (SOSS) data
}
\details{
The \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss}{State of the State Survey} (SOSS) is
regularly collected by the Institute for Public Policy and Social Research (IPPSR) at Michigan State
University (MSU). Each wave is collected from a sample of 1000 adults in the US state of Michigan, and
includes sampling weights to obtain a sample that is representative of the state's population with respect
to age, gender, race, and education. The \code{soss()} function reads the raw data from IPPSR's website, extracts
and recodes selected variables useful for studying childfree adults and other family statuses, then returns a
single data frame. Questions necessary for identifying childfree adults were asked in five waves, which each
include unique questions that may be of interest:
\itemize{
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-79b-spring-2020}{Wave 79} (May 2020) - Neighborhoods, Health care, COVID, Personality
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-82-fall-2021}{Wave 82} (September 2021) - Trust in government, Critical Race Theory
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-84-spring-2022}{Wave 84} (April 2022) - Trust in scientists, Autonomous vehicles, Morality
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-85-fall-2022}{Wave 85} (September 2022) - Reproductive rights, Race equity
\item \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-86-winter-2022}{Wave 86} (December 2022) - Education, Infrastructure
}

\strong{Weights}

The \href{https://cran.r-project.org/web/packages/survey/index.html}{\code{survey}} package can be used to incorporate sampling weights
and obtain population-representative estimates by wave. After using \code{soss()} to obtain data for a given wave (see example below), use
\code{dat <- svydesign(data = dat, ids = ~1, weights = ~weight)} to incorporate information about the survey design.

\strong{Known issues}
\itemize{
\item Wave 79 did not include a "do not know" option for selected questions. Therefore, it is not possible to identify
"undecided" or "ambivalent non-parent" respondents. This may lead other family status categories to be inflated.
\item Wave 82 originally included a 500 person oversample of parents. These respondents are omitted if \code{wave == 82}.
}
}
\examples{
data <- soss(waves = 84, extra.vars = c("neal1"))
}
