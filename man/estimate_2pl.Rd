% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_2pl.R
\name{estimate_2pl}
\alias{estimate_2pl}
\title{Estimate parameters of the two parameter logistic model}
\usage{
estimate_2pl(
  x,
  convergence_criteria = c(0.01, 0.01, 0.01, 0.01, 0.001),
  max_iterations = c(outer_loop = 20, beta_loop = 20, alpha_loop = 20, inner_loop = 20)
)
}
\description{
Estimate parameters of the two parameter logistic model
}
\references{
Birnbaum, A. (1968) Some Latent Trait Models and Their Use in
  Inferring an Examinee’s Ability. In: Lord, F.M. and Novick, M.R., Eds.,
  Statistical Theories of Mental Test Scores, Addison-Wesley, Reading,
  397-479.
}
