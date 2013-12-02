\name{plota.theme}
\alias{plota.theme}
\alias{plota.theme.blue.red}
\alias{plota.theme.gray.orange}
\alias{plota.theme.green.orange}
\title{Plota Control Parameters & Themes}
\usage{
  plota.theme(col.border = "black", col.up = "green",
    col.dn = "red", col.x.highlight = "orange",
    col.y.highlight = "orange", alpha = NA)

  plota.theme.blue.red(alpha = NA)

  plota.theme.green.orange(alpha = NA)

  plota.theme.gray.orange(alpha = NA)
}
\arguments{
  \item{col.border}{border color}

  \item{col.up}{up color}

  \item{col.dn}{down color}

  \item{col.x.highlight}{x highlight color}

  \item{col.y.highlight}{y highlight color}

  \item{alpha}{alpha level}
}
\value{
  nothing
}
\description{
  Set the color theme, the defaul theme is
  plota.theme.green.orange()
}
\examples{
\dontrun{
plota.theme.blue.red()
}
}

