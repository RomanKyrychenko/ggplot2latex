% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_floats_in_text.R
\name{round_floats_in_text}
\alias{round_floats_in_text}
\title{Round Floats in Text}
\usage{
round_floats_in_text(text, digits = 0)
}
\arguments{
\item{text}{A character string containing the text with floating-point numbers.}

\item{digits}{The number of decimal places to round to. Default is 0.}
}
\value{
A character string with rounded floating-point numbers.
}
\description{
Rounds the floating-point numbers in a given text to a specified number of digits.
}
\examples{
library(ggplot2latex)
text <- "Some values are ( 12.7 ,45.3) and also (-3.6,  7.9 )."
new_text <- round_floats_in_text(text)
print(new_text)
}
