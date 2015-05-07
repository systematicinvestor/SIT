\name{XTSFunctions}
\alias{XTSFunctions}
\title{The timezone is set to 'GMT' by defult}
\usage{
  XTSFunctions()
}
\description{
  The reason for setting the default timezone is because
  the following code produces different results if the
  timezone is NOT set and if timezone has a value.
}
\examples{
\dontrun{
# We want to set the timezone, so that following code produces expected results
Sys.getenv('TZ')
test = as.POSIXct('2012-10-31', format='\%Y-\%m-\%d')
 as.numeric(test)
 as.numeric(as.POSIXct(as.Date(test)))
as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
test == as.POSIXct(as.Date(test))

# Set Time Zone
Sys.setenv(TZ = 'GMT')
Sys.getenv('TZ')
test = as.POSIXct('2012-10-31', format='\%Y-\%m-\%d')
 as.numeric(test)
 as.numeric(as.POSIXct(as.Date(test)))
as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
test == as.POSIXct(as.Date(test))

}
}

