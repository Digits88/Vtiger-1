% Generated by roxygen2 (4.0.1): do not edit by hand
\name{create}
\alias{create}
\title{create}
\usage{
output = create(object,element,elementType)
}
\arguments{
\item{object}{A Vtiger object}

\item{element}{A json format character string describing the entry to be
added}

\item{elementType}{A string for valid Vtiger component}
}
\value{
A list containing all details of the created element
}
\description{
Create a new entry in the Vtiger system
}
\examples{
\dontrun{
element1 = toJSON(list("firstname"="testfname","lastname"="testlname",
"assigned_user_id"="testat","label"="Contacts"))
output = create(object,element1,"Contacts")}
}

