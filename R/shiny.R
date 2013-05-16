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
# Repository of Helper Functions for Shiny Framework
# Copyright (C) 2013  Michael Kapler
#
# For more information please visit my blog at www.SystematicInvestor.wordpress.com
# or drop me a line at TheSystematicInvestor at gmail
###############################################################################





###############################################################################
#' Add color highlighting to the \code{\link{reactiveTable}} function
#'
#' @param data table data
#' @param header.col background color for the header row
#' @param row.col background color for alternative rows
#' @param negative.col text color for negative values
#' @param ... additional parameters to the \code{\link{reactiveTable}} fucntion
#'
#' @return HTML table
#'
#' @export 
############################################################################### 
tableColor <- function(data, header.col='LightGray', row.col='yellow', negative.col='red', ...) {
    if (is.null(data))
      return("")
    
    add.to.row= NULL
    if( nrow(data) > 1) {
    	temp = as.list(seq(1,nrow(data)-1,by=2))
    	add.to.row=list(temp, rep("XXX", len(temp)))
    }

    # get HTML table
	temp = renderTable(data, add.to.row=add.to.row, ...)    
	temp = temp()
	
    # negative numbers
    if(!is.na(negative.col) && !is.null(negative.col))
    	temp = gsub("(-\\d*\\.\\d*)", paste("<font color=", negative.col, ">\\1</font>"), temp)
    
    # rows
    if(!is.na(row.col) && !is.null(row.col))
    	temp = gsub("XXX<TR>", paste("<TR bgcolor=", row.col, ">"), temp)
    temp = gsub("XXX", "", temp)
          
    # header
    if(!is.na(header.col) && !is.null(header.col))
    	temp = gsub("<TR>\\s*<TH>\\s*</TH>",paste("<TR bgcolor=", header.col, "><TH></TH>"), temp)
        
    return(temp)
} 


###############################################################################
# take reactive function and extract underlying function
###############################################################################
#extractReactiveFunction <- function(temp) {
#	environment(temp@.Data)[["func"]]
#}


###############################################################################
#' Create a non reactive version of textInput. 
#' 
#' The updated value only send to R when user clicks the Button or hits {Enter}
#' 
#' @param id id of \code{\link{textInput}}
#' @param label label of \code{\link{textInput}}
#' @param value initial value of \code{\link{textInput}}
#' @param button.label optional button label, if button.label = '', the button is not created
#'
#' @return non reactive version of textInput
#'
#' @export 
############################################################################### 
#		# make a non reactive text input
#		#textInput("symbols", "Yahoo Ticker(s) separated by comma:", value = "SPY,IBM"),
#		tagList(tags$label( "Yahoo Ticker(s) separated by comma:"), 
#			tags$input(id = "symbols", type = "text", value = "SPY,IBM",style="display:none;"),
#		   	tags$input(id = "symbolsTemp", type = "text", value = "SPY,IBM", style="display:inline;",
#		   	                onkeypress = "{if (event.keyCode==13) $('#symbolsTempChange').click()}")),
#		div(tags$button(id = "symbolsTempChange", type = "button", class = "btn btn-primary",
#            onclick = "$('#symbols').val($('#symbolsTemp').val()).change();",
#            "Update"                
#            )), 	
############################################################################### 
createNonReactiveTextInput <- function(id, label, value, button.label='') {
if(button.label != '')
	list(
	tagList(tags$label(label), 
		tags$input(id = id, type = "text", value = value, style="display:none;"),
		tags$input(id = paste(id,"Temp",sep=''), type = "text", value = value, style="display:inline;",
			onkeypress = paste("{if (event.keyCode==13) $('#", id, "TempChange').click()}",sep=''))
	),
	div(tags$button(id = paste(id, "TempChange",sep=''), type = "button", class = "btn btn-primary",
    	onclick = paste("$('#",id,"').val($('#", id, "Temp').val()).change();",sep=''),
        button.label))
	)
else
	list(
	tagList(tags$label(label), 
		tags$input(id = id, type = "text", value = value, style="display:none;"),
		tags$input(id = paste(id,"Temp",sep=''), type = "text", value = value, style="display:inline;",
			onkeypress = paste("{if (event.keyCode==13) $('#",id,"').val($('#", id, "Temp').val()).change()}",sep=''))
	)
	)	
}



############################################################################### 
# createNonReactiveTextInputCustom("symbol1", "Yahoo Ticker:", "input", "Update",  opts=list(value = "GOOG", type = "text")),
# createNonReactiveTextInputCustom("symbol2", "Yahoo Ticker:", "textarea", "Update", opts=list(rows=10, cols=10, "GOOG")),
# createNonReactiveTextInputCustom("symbol3", "Yahoo Ticker:", "textarea", "Update", enableEnter=F, opts=list(rows=10, cols=10, "GOOG")),
#' @export 
############################################################################### 	
createNonReactiveTextInputCustom <- function(id, label, tag.label = 'input', button.label='', enableEnter=TRUE,
	 opts) {
onkeypress = ''

if(button.label != '') {
	if(enableEnter)
		onkeypress = paste("{if (event.keyCode==13) $('#", id, "TempChange').click()}",sep='')

	list(
	tagList(tags$label(label), 
		tag(tag.label, c(id = id, style="display:none;", opts)),
		tag(tag.label, c(id = paste(id,"Temp",sep=''), style="display:inline;",
			onkeypress = onkeypress,
			opts))
	),
	div(tags$button(id = paste(id, "TempChange",sep=''), type = "button", class = "btn btn-primary",
    	onclick = paste("$('#",id,"').val($('#", id, "Temp').val()).change();",sep=''),
        button.label))
	)
} else {
	if(enableEnter)
		onkeypress = paste("{if (event.keyCode==13) $('#",id,"').val($('#", id, "Temp').val()).change()}",sep='')
	
	list(
	tagList(tags$label(label), 
		tag(tag.label, c(id = id, style="display:none;", opts)),
		tag(tag.label, c(id = paste(id,"Temp",sep=''), style="display:inline;",
			onkeypress = onkeypress,
			opts))
	)
	)	
}	
}




   
