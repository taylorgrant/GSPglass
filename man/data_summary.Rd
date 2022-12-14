% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{data_summary}
\alias{data_summary}
\title{Summarise Glassdoor data by timeframe}
\usage{
data_summary(dat, time, url)
}
\arguments{
\item{dat}{Glassdoor data}

\item{time}{Timeframe (Year, Quarter, Month)}

\item{url}{Original Glassdoor url}
}
\value{
DT datatable and Highcharter Plot
}
\description{
Summarise Glassdoor data by timeframe
}
\examples{
\dontrun{
data_summary(data, 'year' url)
}
}
