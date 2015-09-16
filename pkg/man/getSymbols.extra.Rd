\name{getSymbols.extra}
\alias{getSymbols.extra}
\title{Helper function to extend functionality of getSymbols}
\usage{
  getSymbols.extra(Symbols = NULL, env = parent.frame(),
    getSymbols.fn = getSymbols, raw.data = new.env(),
    set.symbolnames = F, auto.assign = T, try.extend = T,
    ...)
}
\description{
  Syntax to specify tickers: * Basic : XLY * Rename:
  BOND=TLT * Extend: XLB+RYBIX * Mix above:
  XLB=XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX Symbols = spl('XLY,
  BOND=TLT,XLY+RYBIX,XLB=XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX')
  tickers=spl('XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX,
  XLE+RYEIX+VGENX+FSENX+PRNEX+DREVX, XLF+FIDSX+SCDGX+DREVX,
  XLI+FSCGX+VFINX+FEQIX+DREVX,
  XLK+RYTIX+KTCBX+FSPTX+FTCHX+FTRNX+DREVX,
  XLP+FDFAX+FSPHX+OARDX+DREVX, XLU+FSUTX+DREVX,
  XLV+VGHCX+VFINX+DREVX, XLY+FSRPX+DREVX,
  BOND+IEI+VFIUX+VFITX+FSTGX+FGOVX+STVSX+FGMNX+FKUSX')
}
\details{
  data <- new.env() getSymbols.extra(tickers, src =
  'yahoo', from = '1980-01-01', env = data, auto.assign =
  T) bt.start.dates(data)
}

