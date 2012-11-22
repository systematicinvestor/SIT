###############################################################################
# Setup
###############################################################################
library(devtools)
library(roxygen)
library(roxygen2)

package.name = 'sit'

###############################################################################
# Create folders, copy files
###############################################################################
shell('rmdir /S /Q SIT', wait = TRUE)
shell('mkdir SIT', wait = TRUE)
shell('copy Readme.txt SIT\\*.*', wait = TRUE)
shell('copy Readme.pkg.txt SIT\\*.*', wait = TRUE)

shell('mkdir SIT\\R', wait = TRUE)
shell('copy R\\*.* SIT\\R\\*.*', wait = TRUE)

###############################################################################
# Create DESCRIPTION files
###############################################################################
write.dcf(list(
	Package = toupper(package.name), 
	Type = 'Package',
	Title = 'Systematic Investor Toolbox', 
	Description = 'Systematic Investor Toolbox is a collection of tools that\n I use in my investment research.', 
    Version = format(Sys.Date(),'%Y.%m.%d'), 
    Date = Sys.Date(),
    License = 'GPL-3', 
    LazyLoad = 'yes',
    Author = 'Michael Kapler <TheSystematicInvestor@gmail.com>', 
    Maintainer = 'Michael Kapler <TheSystematicInvestor@gmail.com>'
    ), 
    file = file.path(package.name, "DESCRIPTION")
)

cat("
#' Systematic Investor Toolbox.
#' 
#' Systematic Investor Toolbox is a collection of tools that 
#' I use in my investment research.
#' 
#' @name SIT-package
#' @aliases SIT
#' @docType package
#' @title Systematic Investor Toolbox.
#' @author Michael Kapler \\email{TheSystematicInvestor@@gmail.com}
NULL
", file = file.path(package.name, 'R', paste(package.name,'package.R',sep='-')))

###############################################################################
# Create documentaion and build package
###############################################################################
roxygenize(package.name, copy.package = F, unlink.target = F, overwrite = T)

pkg <- as.package(package.name)
name = devtools:::build(pkg, package.name)
shell(paste('copy /Y /B', gsub('/','\\\\',name), 'SIT.tar.gz'), wait = TRUE)


###############################################################################
# Usage
###############################################################################
# install.packages('SIT.tar.gz', repos = NULL, type='source')
# library(SIT)
# ?spl


