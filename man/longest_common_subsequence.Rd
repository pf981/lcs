% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{longest_common_subsequence}
\alias{longest_common_subsequence}
\title{Longest common subsequence}
\usage{
longest_common_subsequence(str1, str2)
}
\arguments{
\item{str1}{A character vector}

\item{str2}{A character vector}
}
\value{
A character vector of the same length as \code{str1} and 
  \code{str2}.
}
\description{
Find the longest common substring of two strings.
}
\details{
Implemented in C++. The algorithm runs in \code{O(n*r)} time where \code{n} 
and \code{r} are the lengths of the two strings.
}
\examples{
longest_common_substring('abcdefgh', 'xxxbcdexxx')
}
