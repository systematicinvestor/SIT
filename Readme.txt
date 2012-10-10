###############################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
###############################################################################
# Systematic Investor Toolbox (SIT)
# Copyright (C) 2012  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################
# Systematic Investor Toolbox (SIT)
#
# Systematic Investor Toolbox is a collection of tools that I use
# in my investment research. I will demonstrate and document 
# various uses of toolbox in the Systematic Investor blog at
#	www.SystematicInvestor.wordpress.com
#
#
###############################################################################
# Example Usage:
###############################################################################
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
#con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
#    source(con)
#close(con)
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT): Windows only
############################################################################### 
# Load Systematic Investor Toolbox (SIT)
#setInternet2(TRUE)
#con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
#	source(con)
#close(con)
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT): from file, if for example you saved sit.gz to c:/temp/sit.gz
############################################################################### 
#con = gzcon(file('c:/temp/sit.gz', 'rb'))
#	source(con)
#close(con)
#
#
###############################################################################
# Load Systematic Investor Toolbox (SIT): Requires RCurl package
############################################################################### 
#require(RCurl)
#sit = getURLContent('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', binary=TRUE, followlocation = TRUE, ssl.verifypeer = FALSE)
#	con = gzcon(rawConnection(sit, 'rb'))
#	source(con)
#close(con)
#
#
###############################################################################
# Example Usage:
############################################################################### 
# Run plota test
#plota.test()
#
#
#
#
#
#
#More to come,
#
#Michael Kapler
#TheSystematicInvestor at gmail
#

