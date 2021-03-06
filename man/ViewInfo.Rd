% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ViewInfo.R
\name{ViewInfo}
\alias{ViewInfo}
\alias{viewAssets}
\alias{viewBudgets}
\alias{viewBudgetCategoriesAll}
\alias{viewBudgetCategoriesSpendingSmall}
\alias{viewBudgetCategoriesSpending}
\alias{viewBudgetCategoriesReal}
\alias{viewAccountCategories}
\title{Print out reference financial info}
\usage{
viewAssets(ledger = viewLedger(file = file), file = viewLedgerFile(),
  from = as.Date("1900-01-01"), to = componentsToDate(),
  suppress = TRUE)

viewBudgets(suppress = FALSE)

viewBudgetCategoriesAll(suppress = TRUE)

viewBudgetCategoriesSpendingSmall(suppress = TRUE)

viewBudgetCategoriesSpending(suppress = TRUE)

viewBudgetCategoriesReal(suppress = TRUE)

viewAccountCategories(ledger = viewLedger(file = file),
  file = viewLedgerFile(), accounts = NULL, formal = FALSE,
  suppress = TRUE)
}
\arguments{
\item{ledger}{Data frame ledger. The ledger where each row is a transaction
and with columns for \code{year}, \code{month}, \code{day},
\code{description}, \code{budget}, and one column for each
financial account. All columns should be numeric except for
\code{description} and \code{budget}.}

\item{file}{The path for the ledger for loading and saving. Has an underlying
default that needs to be changed in the \code{ViewLedger} file.}

\item{from}{Boundaries on the ledger dates. Requires Date class.}

\item{to}{Boundaries on the ledger dates. Requires Date class.}

\item{suppress}{Prevents it from printing to the console. All low-level
functions default to \code{TRUE}, while high-level functions that do not
have callers and exist for the sole purpose of printing out information
default to \code{FALSE}.}
}
\value{
Character vector.
}
\description{
Grabs information from the ledger.
}
\details{
\code{viewAssets} simply prints the current values for each account
  by summing the values of each account.\cr \code{viewBudgets} returns the
  budget categories and their allowed amounts per month.\cr
  \code{viewBudgetCategories} grabs the levels of the factor column
  \code{budget.category}, while \code{viewBudgetCategoriesReal} ignores the
  categories without explicit budget levels, like discretionary spending, and
  \code{viewBudgetCategoriesSpending} is the same, but also discards expected
  income, and \code{viewBudgetCategoriesSpendingSmall} elminiates categories
  with spending outside one standard deviation of all the budgeted
  amounts.\cr \code{viewAccountCategories} provides the factor names from the
  ledger that correspond to the account categories.
}
