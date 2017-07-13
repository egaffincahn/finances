#' @title Print out reference financial info
#'
#' @name ViewInfo
#'
#' @description Grabs information from the ledger.
#'
#' @inheritParams ViewLedger
#'
#' @details \code{viewAssets} simply prints the current values for each account
#'   by summing the columns of each account.\cr \code{viewBudgets} returns the
#'   budget categories and their allowed amounts per month.\cr
#'   \code{viewBudgetCategories} grabs the levels of the factor column
#'   \code{budget.category}, while \code{viewBudgetCategoriesReal} ignores the
#'   categories without explicit budget levels, like discretionary spending, and
#'   \code{viewBudgetCategoriesSpending} is the same, but also discards expected
#'   income, and \code{viewBudgetCategoriesSpendingSmall} elminiates categories
#'   with spending outside one standard deviation of all the budgeted
#'   amounts.\cr \code{viewAccountCategories} provides the column names from the
#'   ledger that correspond to the account categories. They all begin with
#'   <\code{acct.}>.
#'
#' @return Character vector.
#'

#' @rdname ViewInfo
#' @export
viewAssets <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), yr = currentYear(), mo = currentMonth(), dy = currentDay(), suppress = TRUE) {
    ledger.mini <- colSums(ledger[, viewAccountCategories()])
    if (!suppress) print(ledger.mini)
    return(ledger.mini)
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
    budgets <- viewBudgets(suppress = TRUE)
    budget.categories <- names(budgets)
    if (!suppress) print(budget.categories)
    return(budget.categories)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesSpendingSmall <- function(suppress = TRUE) {
    budgets <- viewBudgets(suppress = TRUE)
    budgets.categories.spending.small <- budgets %>% na.omit %>% {(. - mean(.))} %>% {./sd(.)} %>% .[.<1 & .>-1] %>% names
    if (!suppress) print(budgets.categories.spending.small)
    return(budgets.categories.spending.small)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesSpending <- function(suppress = TRUE) {
    budgets <- viewBudgets(suppress = TRUE)
    budget.categories.spending <- names(budgets[!is.na(budgets) & budgets < 0])
    if (!suppress) print(budget.categories.spending)
    return(budget.categories.spending)
}

#' @rdname ViewInfo
#' @export
viewBudgetCategoriesReal <- function(suppress = TRUE) {
    budgets <- viewBudgets(suppress = TRUE)
    budget.categories.real <- names(budgets[!is.na(budgets)])
    if (!suppress) print(budget.categories.real)
    return(budget.categories.real)
}

#' @rdname ViewInfo
#' @export
viewAccountCategories <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), formal = FALSE, suppress = TRUE) {
    if (formal) {
        load(.dataLocation())
        account.categories <- account.categories.formal
    } else {
        account.categories <- colnames(ledger)[sapply(colnames(ledger), function(x) gregexpr("^acct", x) != -1)]
    }
    if (!suppress) print(account.categories)
    return(account.categories)
}

#' @rdname ViewInfo
#' @export
viewNonAccountCategories <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), suppress = TRUE) {
    account.categories <- ledger %>% {colnames(.)[!(colnames(.) %in% viewAccountCategories(.))]}
    if (!suppress) print(account.categories)
    return(account.categories)
}
