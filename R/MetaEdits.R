#' @title High-level edits
#'
#' @name MetaEdits
#'
#'
#' @description These functions have the power to add new accounts, create new
#'   ledgers, etc.
#'
#' @inheritParams viewLedger
#'
#' @details \code{createLedger} builds a new ledger in the standard form. It
#'   queries the user for account names, initial values, budget categories and
#'   amounts, and file location.\cr \code{addAccount} can take a ledger and add
#'   a new account name to it. It queries the user for the account name and the
#'   column position in the ledger.
#'

#' @rdname MetaEdits
#' @export
createLedger <- function() {
    # ...
    throw("createLedger() not yet supported.")
    saveLedger(ledger, file = file)
}

#' @rdname MetaEdits
#' @export
setBudgets <- function(ledger = viewLedger(file = file), file = viewLedgerFile(), data.location = .dataLocation()) {
    load(data.location)
    budget.categories <- viewBudgetCategoriesAll()
    budget.amounts <- numeric(length(budget.categories))
    print("For each budget category, enter the budget value you expect per month. Income should be positive, costs should be negative. If it is a special category, type NA.")
    # suppressWarnings(warning("NAs introduced by coercion"))
    for (i in seq(budget.categories)) {
        budget.amounts[i] <- as.numeric(readline(paste(budget.categories[i], ": ", sep = "")))
    }
    budgets <- budget.amounts
    names(budgets) <- budget.categories
    save("budgets", "file", file = dataLocation)
}

#' @rdname MetaEdits
#' @export
editAutoAddLocation <- function(data.location = .dataLocation()) {
    load(data.location)
    auto.add.transaction <- readline("Enter new path for reading transactions to auto add (do not use quotes): ")
    save(list = ls(), file = data.location)
}
