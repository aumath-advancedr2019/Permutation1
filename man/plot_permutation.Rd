\name{plot_permutation}
\alias{plot_permutation}
\title{
plot_permutation
}
\description{
This function is mostly for plotting the permutations in this package, but can still be used for making a nice plot, if you have the values of a distribution you want to plot nicely and an observed value. To use this you need to install ggplot2.
}
\usage{
plot_permutation(data, observed, title)
}
\arguments{
  \item{data}{vector of values, that can be used for a distribution (or that you just want plotted)}
  \item{observed}{observed value}
  \item{title}{title of the plot (string)}
}

\value{
plot_permutaion returns a ggplot.
}

\author{  
Adrián Gómez & Solveig Østergaard
}
