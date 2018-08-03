#' @title Print out reference financial info
#'
#' @name ViewInfo
#'
#' @import tidyverse
#'
#' @description Grabs information from the ledger.
#'
#' @inheritParams ViewLedger
#'
#' @details \code{viewAssets} simply prints the current values for each account
#'   by summing the values of each account.\cr \code{viewBudgets} returns the
#'   budget categories and their allowed amounts per month.\cr
#'   \code{viewBudgetCategories} grabs the levels of the factor column
#'   \code{budget.category}, while \code{viewBudgetCategoriesReal} ignores the
#'   categories without explicit budget levels, like discretionary spending, and
#'   \code{viewBudgetCategoriesSpending} is the same, but also discards expected
#'   income, and \code{viewBudgetCategoriesSpendingSmall} elminiates categories
#'   with spending outside one standard deviation of all the budgeted
#'   amounts.\cr \code{viewAccountCategories} provides the factor names from the
#'   ledger that correspond to the account categories.
#'
#' @return Character vector.
#'

#' @rdname ViewInfo
#' @export
viewAssets <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), from = as.Date("1900-01-01"), to = componentsToDate(), suppress = TRUE) {
    ledger.sums <- ledger %>%
        viewLedger(ledger = ledger, from = from, to = to) %>%
        group_by(account) %>%
        do({
            data.frame(amount = sum(.$amount))
        }) %>%
        as.data.frame()
    if (!suppress) print(ledger.sums)
    return(ledger.sums)
}

#' @rdname ViewInfo
#' @export
viewBudgets <- function(suppress = FALSE) {
    load(.dataLocation())
    if (!suppress) print(budgets)
    return(budgets)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesAll <- function(suppress = TRUE) {
    budgets <- names(viewBudgets(suppress = TRUE))
    if (!suppress) print(budgets)
    return(budgets)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesSpendingSmall <- function(suppress = TRUE) {
    budgets <- viewBudgets(suppress = TRUE)
    budgets.spending.small <- budgets %>% na.omit %>% {(. - mean(.))} %>% {./sd(.)} %>% .[.<1 & .>-1] %>% names
    if (!suppress) print(budgets.spending.small)
    return(budgets.spending.small)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesSpending <- function(suppress = TRUE) {
    budgets <- viewBudgets(suppress = TRUE)
    budget.spending <- names(budgets[!is.na(budgets) & budgets < 0])
    if (!suppress) print(budget.spending)
    return(budget.spending)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesReal <- function(suppress = TRUE) {
    budgets <- viewBudgets(suppress = TRUE)
    budget.real <- names(budgets[!is.na(budgets)])
    if (!suppress) print(budget.real)
    return(budget.real)
}

#' @rdname ViewInfo
#' @export
viewAccountCategories <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), accounts = NULL, formal = FALSE, suppress = TRUE) {
    if (is.null(accounts)) accounts <- levels(ledger$account)
    if (formal) {
        accounts <- gsub("BoA", "Bank of America", accounts, ignore.case = TRUE)
        accounts <- gsub("CapOne", "Capital One", accounts, ignore.case = TRUE)
        accounts <- gsub("\\.", " ", accounts)
        accounts <- gsub("ira", "IRA", accounts, ignore.case = TRUE)
        accounts <- gsub("btc", "BTC", accounts, ignore.case = TRUE)
        accounts <- gsub("tiaa", "TIAA", accounts, ignore.case = TRUE)
        accounts <- tools::toTitleCase(accounts)
    }
    if (!suppress) print(accounts)
    return(accounts)
}
