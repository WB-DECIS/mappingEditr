% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{append_empty_row}
\alias{append_empty_row}
\title{Append an empty row to a data frame}
\usage{
append_empty_row(input_df)
}
\arguments{
\item{input_df}{A data frame. The data frame to which an empty row should be appended.}
}
\value{
A data frame with one additional row, where the new row's values are \code{NA}.
}
\description{
This function takes an input data frame and appends a new row containing
\code{NA} values for each of its columns. This can be useful for initializing
a new entry in a table or ensuring that a minimum row structure is maintained.
}
\examples{
# Example usage:
df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
df_updated <- append_empty_row(df)
df_updated

}
