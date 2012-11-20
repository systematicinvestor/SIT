library(devtools)
library(roxygen)
library(roxygen2)

package.name = 'sit'

# DESCRIPTION file
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

roxygenize(package.name, copy.package = F, unlink.target = F, overwrite = T)

pkg <- as.package(package.name)
name = devtools:::build(pkg, package.name)
shell(paste('copy /Y /B', gsub('/','\\\\',name), 'SIT.tar.gz'), wait = TRUE)

