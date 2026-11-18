# =============================================================================
# run_analysis.R  —  Assignment 2: Technical Analysis using R, Preliminary Stage
# -----------------------------------------------------------------------------
# AI Assistance Declaration: I used Claude (Anthropic, Claude Opus 5.5, Claude
# desktop app, 2026-11-18) for drafting the script structure and comments. I
# verified outputs by running the script in RStudio and checking the printed
# data and statistics. All final calculations are done by R on my computer. I
# am responsible for the accuracy and originality of this work.
# -----------------------------------------------------------------------------
# How to run: open TechnicalAnalysis.Rproj (or set the working directory to
# this folder) and run:   source("run_analysis.R")
# =============================================================================

# ---- 0. Packages (installed once with install.packages) ---------------------
required <- c("quantmod", "TTR", "ggplot2", "knitr", "rmarkdown")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

source("R/ta_functions.R")

# ---- 1. Configuration --------------------------------------------------------
portfolio_file <- "portfolio.txt"
start_date     <- as.Date("2025-11-01")
end_date       <- as.Date("2026-11-01")   # quantmod treats 'to' as exclusive

cat("Portfolio:", paste(read_portfolio(portfolio_file), collapse = ", "), "\n")

# ---- 2. Import data (one data frame per symbol) -----------------------------
stocks <- load_stock_data(portfolio_file, from = start_date, to = end_date,
                          assign_to_env = TRUE)   # also creates AAPL, MSFT, ...
str(stocks, max.level = 1)

# ---- 3. Statistics -----------------------------------------------------------
results <- calculate_all_statistics(stocks, price_col = "Close", ma_windows = c(20, 50))
print(results$summary_table[, c("Symbol", "Mean", "Median", "Mode", "Std_Dev",
                                "Last_Close", "Last_SMA_20", "Last_SMA_50")],
      digits = 6, row.names = FALSE)

# ---- 4. Display every stock --------------------------------------------------
for (sym in names(stocks)) {
  display_stock_data(stocks[[sym]], sym, n = 5)
  display_quote_summary(stocks[[sym]], sym)
  display_statistics(results$by_symbol[[sym]], sym)
}

# ---- 5. Charts ---------------------------------------------------------------
for (sym in names(stocks)) {
  plot_stock_chart(stocks[[sym]], sym)
  print(plot_price_with_ma(results$by_symbol[[sym]], sym))
}
print(plot_portfolio_growth(stocks))
