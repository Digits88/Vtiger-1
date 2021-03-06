% Generated by roxygen2 (4.0.1): do not edit by hand
\docType{class}
\name{Vtiger}
\alias{Vtiger}
\title{Vtiger}
\usage{
obj = new("Vtiger", webServiceUrl, accessKey, userName)
}
\value{
Object of class Vtiger
}
\description{
The Vtiger class
}
\section{Slots}{

\describe{
\item{\code{userName}}{Vtiger userName. Must be character. Provided by User.}

\item{\code{accessKey}}{Vtiger accessKey. Must be character. Provided by User.}

\item{\code{webServiceUrl}}{Vtiger Url. Must be character. Provided by User.}

\item{\code{sessionName}}{Vtiger sessionName. Must be character. NOT provided by User
- generated at login.}
}}
\examples{
\dontrun{
object = new(Class = "Vtiger",
webServiceUrl = "http://vtiger.EXAMPLE.com/webservice.php?",
accessKey = "0123456789abcde",
userName = "MYUSERNAME")}
}
\seealso{
Vtiger References:

1) \url{https://wiki.vtiger.com/index.php/Main_Page},

2) \url{http://community.vtiger.com/help/}.

Vtiger Webservices References:

1) \url{https://wiki.vtiger.com/index.php/Webservices_tutorials},

2) \url{https://wiki.vtiger.com/index.php/Webservice_reference_manual}
}

