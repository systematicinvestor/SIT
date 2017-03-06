\name{createNonReactiveTextInput}
\alias{createNonReactiveTextInput}
\title{Create a non reactive version of textInput.}
\usage{
  createNonReactiveTextInput(id, label, value,
    button.label = "")
}
\arguments{
  \item{id}{id of \code{\link{textInput}}}

  \item{label}{label of \code{\link{textInput}}}

  \item{value}{initial value of \code{\link{textInput}}}

  \item{button.label}{optional button label, if
  button.label = '', the button is not created}
}
\value{
  non reactive version of textInput
}
\description{
  The updated value only send to R when user clicks the
  Button or hits {Enter}
}

