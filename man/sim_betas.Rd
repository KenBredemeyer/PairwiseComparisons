% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{sim_betas}
\alias{sim_betas}
\title{Allocate locations to performance names, using random distributions}
\usage{
sim_betas(performances, distribution = "normal", mean, sd, min, max)
}
\arguments{
\item{performances}{Character vector of performance names}

\item{distribution}{Character, either \code{"normal"} or \code{"uniform"}.}
}
\value{
data.frame of performances and locations.
}
\description{
Allocate locations to performance names, using random distributions
}
