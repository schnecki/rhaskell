% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Data.List.r
\name{splitAt}
\alias{splitAt}
\title{Split a list into two separate lists wrapped in a tuple. The first list will be until inclusive
the provided index, the second list the rest.}
\usage{
splitAt(idx, xs)
}
\arguments{
\item{idx}{index}

\item{xs}{list}
}
\description{
\code{splitAt :: Int -> [a] -> ([a], [a])}
}
