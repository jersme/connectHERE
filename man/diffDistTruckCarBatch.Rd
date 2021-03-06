% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRoutes.R
\name{diffDistTruckCarBatch}
\alias{diffDistTruckCarBatch}
\title{Batch input (data.frame) for the diffDistTruckCar() function.}
\usage{
diffDistTruckCarBatch(url = setURL(type = "routing"), app_id, app_code,
  df, waypoint0 = "waypoint0", waypoint1 = "waypoint1")
}
\arguments{
\item{url}{The base url as required to connect to the HERE API. If no value
is passed in this parameter the standard parameter from the setURL() is
used. See the documentation for the setURL() for more information about
the base url.}

\item{app_id}{App id for the HERE REST API call. See the vignette: Getting
Started how to get an App Id.}

\item{app_code}{App Code for the HERE REST API call. See the vignette: Getting
Started how to get an App Code.}

\item{df}{Dataframe where for the distances will be calculated based on the
geocoordinates.}

\item{waypoint0}{Name of the column for the waypoint 0.}

\item{waypoint1}{Name of the column for the waypoint 0.}
}
\value{
The truck and car distances for each each row in the inputted
dataframe.
}
\description{
Batch input (data.frame) for the diffDistTruckCar() function.
}
\examples{
diffDistTruckCarBatch(
df = data.frame(waypoint0 = c("xxx", "yyy"), waypoint1 = c("yyy", "xxx")),
app_id = "xxx",
app_code = "xxx"
)
}
