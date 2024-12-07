% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{parse_dsd_id}
\alias{parse_dsd_id}
\title{Parse DSD ID to URL Format}
\usage{
parse_dsd_id(dsd_id)
}
\arguments{
\item{dsd_id}{A character string representing the DSD ID, e.g., "WB.DATA360:DS_DATA360(1.2)".}
}
\value{
A character string with the parsed DSD ID in the format "AgencyID.ArtifactID/ID/Version".
}
\description{
Parses a DSD ID of the form "AgencyID.ArtifactID:ID(Version)" and converts it into a URL-like format "AgencyID.ArtifactID/ID/Version".
}
\examples{
parse_dsd_id("WB.DATA360:DS_DATA360(1.2)")
# Returns "WB.DATA360/DS_DATA360/1.2"

}
