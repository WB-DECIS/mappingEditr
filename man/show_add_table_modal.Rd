% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_add_table.R
\name{show_add_table_modal}
\alias{show_add_table_modal}
\title{Show Add Table Modal}
\usage{
show_add_table_modal(
  ns,
  title = "Add New Table",
  table_types = c("Fixed", "Mapping"),
  selected_type = "Fixed"
)
}
\arguments{
\item{ns}{A namespace function for unique IDs.}

\item{title}{Title of the modal dialog.}

\item{table_types}{A vector of table type options.}

\item{selected_type}{The default selected table type.}
}
\description{
This function shows a modal dialog for adding a new table, using `generate_add_table_modal`.
}
