.onLoad = function
(
	libname = find.package("SIT"), 
	pkgname = "SIT"
) 
{
	# must set timezone before any calls to xts
	# [timezone warning message](https://github.com/joshuaulrich/xts/issues/113)
	if( nchar(Sys.getenv('TZ')) == 0 ) Sys.setenv(TZ=Sys.timezone())
	#Sys.setenv(TZ = 'GMT')
	#Sys.setenv(TZ = 'EST')

	###############################################################################
	# The timezone is set to 'GMT' by default
	#
	# The reason for setting the default timezone is because the following code 
	# produces different results if the timezone is NOT set and if timezone has a value.
	# 
	# @examples
	# \dontrun{ 
	# 
	# # We want to set the timezone, so that following code produces expected results
	# Sys.getenv('TZ')
	# test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
	#	as.numeric(test)
	#	as.numeric(as.POSIXct(as.Date(test)))
	# as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
	# test == as.POSIXct(as.Date(test))
	#
	# # Set Time Zone
	# Sys.setenv(TZ = 'GMT')
	# Sys.getenv('TZ')
	# test = as.POSIXct('2012-10-31', format='%Y-%m-%d')
	#	as.numeric(test)
	#	as.numeric(as.POSIXct(as.Date(test)))
	# as.numeric(as.POSIXct(as.Date(test))) - as.numeric(test)
	# test == as.POSIXct(as.Date(test))
	#
	# }
	###############################################################################

	# setup default plota color scheme
	library(grDevices)
	plota.control <<- new.env()
		plota.control$col.border = 'black'
		plota.control$col.up = 'green'
		plota.control$col.dn = 'red'
		plota.control$col.x.highlight = 'orange'
		plota.control$col.y.highlight = 'orange'
		plota.control$xaxis.ticks = c()

	# set default theme	
	plota.theme.green.orange();
}

