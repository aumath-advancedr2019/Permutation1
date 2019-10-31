\name{permutation_median}
\alias{permutation_median}
\title{
permutation_median
}
\description{
Performs a permutation test on a dataset (dataframe) testing if the median of two different groups are significantly different (the difference in medians are significantly different from 0). It prints a permutation-plot with the permuted null-distribution and the observed value (simple or nice depending on choice) and gives a p-value of the medians being the same.
}
\usage{
permutation_median(dataset, name, count, group1, group2, no_perm, nice_plot)
}
\arguments{
  \item{dataset}{the dataset containing the data you want to test in the form of a data frame – the data needs to contain (at least) a column with the names of the two groups you want to test against each other and a column with the values of the different cases for these groups.}
  \item{name}{the name of the column with the names of the two groups (string)}
  \item{count}{the name if one of the groups (string)}
  \item{group1}{the name of the other group (string)}
  \item{group2}{the name of the other group (string)}
  \item{no_perm}{the number of permutations to make, default=10000}
  \item{nice_plot}{a logic value indicating whether a nice plot should be printed (requires ggplot2), default=F (as opposed to a simple plot)}
}
\details{
The calculations that are done in this function are the difference in means (mean(group1)-mean(group2)). This will be the observed value. The dataset values are then randomly shuffled and the difference in mean are calculated again and saved. This is done as many times as the no_perm is defined as, and thereby a null-distribution is made. A p-value is then calculated as the amount of times the absolute value of the permuted values are bigger or equal to the absolute value of the observed + 1 divided by the total amount of permutations + 1:

\deqn{p_val = (\sum(|perm_val| \ge |observed|)+1) / (#permutation+1)}

We add one as a precaution, since we do not know the value of the next permutation and thereby always should expect the “next” value to be at least as extreme as the observed.
This test is two-sided.
}
\value{
permutation_mean returns a list with the class “htest” containing the following:
  \item{method}{the method of the test}
  \item{p.value}{the p-value of the test}
  \item{estimate}{the values the observed means for the different groups and the difference}
  \item{data.name}{the names of the two groups tested}
}
\references{
}
\author{
Adrián Gómez & Solveig Østergaard
}
\note{
}
\seealso{
}
\examples{
}
