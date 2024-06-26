% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_gen.R
\name{data_gen}
\alias{data_gen}
\title{Generating simulated data}
\usage{
data_gen(size, center, sd = 1)
}
\arguments{
\item{size}{The number of observations to generate}

\item{center}{The mean of the normal distribution}

\item{sd}{The standard deviation of the normal distribution}
}
\value{
A data frame with columns Y and X
}
\description{
Utilizes a random normal generation with mean, sd set by user. The function
passes the generated data through a logistic function to output
probabilities, which are then passed into the rbinom function to generate
binary responses motivated by the predictor variable, X ~ N(mean, sd).
}
