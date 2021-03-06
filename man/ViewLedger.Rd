% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ViewLedger.R
\name{ViewLedger}
\alias{ViewLedger}
\alias{saveLedger}
\alias{viewLedger}
\alias{viewLedgerFile}
\title{Load, save, and view a ledger}
\usage{
saveLedger(ledger, file = viewLedgerFile())

viewLedger(ledger = read.csv(file), file = viewLedgerFile(),
  from = as.Date("1900-01-01"), to = Sys.Date(), on = NULL,
  descriptions = NULL, budgets = NULL, amount.ops = NULL,
  amounts = NULL, accounts = NULL, suppress = TRUE)

viewLedgerFile(file = NULL, suppress = TRUE)
}
\arguments{
\item{ledger}{Data frame ledger. The ledger where each row is a transaction
and with columns for \code{year}, \code{month}, \code{day},
\code{description}, \code{budget}, and one column for each
financial account. All columns should be numeric except for
\code{description} and \code{budget}.}

\item{file}{The path for the ledger for loading and saving. Has an underlying
default that needs to be changed in the \code{ViewLedger} file.}

\item{from, to}{Boundaries on the ledger dates. Requires Date class.}

\item{suppress}{Prevents it from printing to the console. All low-level
functions default to \code{TRUE}, while high-level functions that do not
have callers and exist for the sole purpose of printing out information
default to \code{FALSE}.}

\item{yr, mo, dy}{Year, month, day. When one of these is \code{NULL}, function
behavior is to allow all possible values.}
}
\value{
\code{viewLedger} and each of its date components return a data frame
  ledger. \code{viewLedgerFile} returns the character path to the ledger.
}
\description{
These are the lowest level functions that have the default
  ledger locations for loading, saving, and viewing. Higher functions call
  these to perform these functions or by passing down the chain custom
  ledgers and file paths or by allowing these to use the defaults.
}
\details{
\code{viewLedger} does the loading of the ledger and is called by
  many other functions. Can be used to manually manipulate by calling, and to
  view it by calling with \code{suppress = FALSE}. Therefore, calling
  \code{viewLedger} with \code{suppress = FALSE} is not recommended, as it
  will print out a large dataset to the console.
}
