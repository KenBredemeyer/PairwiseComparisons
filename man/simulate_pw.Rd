% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{simulate_pw}
\alias{simulate_pw}
\title{Simulate pairwise judgements}
\usage{
simulate_pw(
  performances,
  pairs,
  judges,
  criteria = 1,
  judge_alpha = rep(1, length(pairs))
)
}
\arguments{
\item{performances}{data frame containing \code{performance} and \code{location}.}

\item{pairs}{List of pairs, one list element for each judge.}

\item{judges}{integer specifying the number of judges, or character vector
giving the names of judges.}

\item{criteria}{integer specifying the number of criteria, or character
vector giving the names of criteria.}

\item{judge_alpha}{Vector equal in length to the number of judges, providing
the discrimination parameter as a property of judges.}
}
\description{
Simulate pairwise judgements
}
