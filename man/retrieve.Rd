% Generated by roxygen2 (4.0.1): do not edit by hand
\name{retrieve}
\alias{retrieve}
\title{retrieve}
\usage{
output = retrieve(object,id)
}
\arguments{
\item{object}{A Vtiger object}

\item{id}{A Vtiger id (of type character) of the element to be retrieved}
}
\value{
A list containing all details of the retrieved element
}
\description{
Retrieve an existing entry from the Vtiger system
}
\examples{
\dontrun{
output = retrieve(object,id="12x3") ## Make sure this is a valid id}
}

