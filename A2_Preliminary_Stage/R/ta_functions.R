# =============================================================================
# ta_functions.R  —  Technical Analysis utilities (Assignment 2, Preliminary Stage)
# -----------------------------------------------------------------------------
# AI Assistance Declaration: I used Claude (Anthropic, Claude Opus 5.5, Claude
# desktop app, 2026-11-18) for drafting function structure, comments and
# layout. I verified outputs by running every function in RStudio and checking
# the results against the raw downloaded data. All final calculations are done
# by R on my computer. I am responsible for the accuracy and originality of
# this work.
# -----------------------------------------------------------------------------
# Functions
#   read_portfolio()          read stock symbols from portfolio.txt
#   load_stock_data()         download OHLCV data for every symbol (quantmod)
#   get_mode()                statistical mode of a numeric vector
#   calculate_statistics()    statistics + moving averages for ONE stock
#   calculate_all_statistics() statistics for the whole portfolio
#   display_stock_data()      print a data frame preview for one stock
#   display_quote_summary()   Yahoo-Finance-style quote summary for one stock
#   display_statistics()      print the statistics of one stock
#   plot_stock_chart()        candlestick chart with SMA overlays and volume
#   plot_price_with_ma()      line chart: close price + moving averages
#   plot_portfolio_growth()   growth of $100 invested in each stock
# Requires: quantmod (loads xts, zoo, TTR), TTR, ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(ggplot2)
})

# -----------------------------------------------------------------------------
# 1. Read the portfolio file
# -----------------------------------------------------------------------------
#' @param path   path to a text file with one ticker symbol per line
#' @return       character vector of unique, upper-case symbols
read_portfolio <- function(path = "portfolio.txt") {
  if (!file.exists(path)) {
    stop("Portfolio file not found: ", path)
  }
  symbols <- trimws(readLines(path, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]  # skip blanks/comments
  unique(toupper(symbols))
}

# -----------------------------------------------------------------------------
# 2. Import stock data with quantmod
# -----------------------------------------------------------------------------
#' Reads portfolio.txt and downloads daily prices for every symbol.
#' @param portfolio_file path to portfolio.txt
#' @param from, to       date range (Date or "YYYY-MM-DD")
#' @param src            quantmod data source (default "yahoo")
#' @param assign_to_env  if TRUE, also creates one data frame per symbol
#'                       (e.g. AAPL, MSFT) in the given environment
#' @return named list of data frames: Date, Open, High, Low, Close, Volume, Adjusted
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to   = Sys.Date(),
                            src  = "yahoo",
                            assign_to_env = FALSE,
                            envir = parent.frame()) {
  symbols <- read_portfolio(portfolio_file)
  stocks  <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")
    xts_data <- tryCatch(
      getSymbols(sym, src = src, from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(xts_data) || NROW(xts_data) == 0) next

    df <- data.frame(Date = index(xts_data), coredata(xts_data), row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[sym]] <- df

    if (assign_to_env) {
      assign(make.names(sym), df, envir = envir)   # separate data frame per symbol
    }
  }

  message("Loaded ", length(stocks), " of ", length(symbols), " symbols.")
  stocks
}

# -----------------------------------------------------------------------------
# 3. Statistics
# -----------------------------------------------------------------------------
#' Mode of a numeric vector. Prices are continuous, so values are rounded
#' first (default: to whole dollars); ties return the smallest value.
get_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  if (length(x) == 0) return(NA_real_)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

#' Statistics and moving averages for ONE stock.
#' @param df         data frame returned by load_stock_data() for one symbol
#' @param price_col  column used for the statistics (default "Close")
#' @param ma_windows moving-average window lengths in trading days
#' @return list(summary = 1-row data frame, moving_averages = data frame)
calculate_statistics <- function(df, price_col = "Close", ma_windows = c(20, 50)) {
  price <- df[[price_col]]

  # Simple moving averages (TTR::SMA); first (n - 1) values are NA by design
  ma <- data.frame(Date = df$Date, Price = price)
  for (n in ma_windows) {
    ma[[paste0("SMA_", n)]] <- if (length(price) >= n) SMA(price, n = n) else NA_real_
  }

  daily_return <- c(NA, diff(price) / head(price, -1))

  summary <- data.frame(
    Observations   = length(price),
    Start          = min(df$Date),
    End            = max(df$Date),
    Mean           = mean(price, na.rm = TRUE),
    Median         = median(price, na.rm = TRUE),
    Mode           = get_mode(price),
    Std_Dev        = sd(price, na.rm = TRUE),
    Min            = min(price, na.rm = TRUE),
    Max            = max(price, na.rm = TRUE),
    Last_Close     = tail(price, 1),
    Mean_Daily_Return_pct = 100 * mean(daily_return, na.rm = TRUE),
    Volatility_pct = 100 * sd(daily_return, na.rm = TRUE)
  )
  for (n in ma_windows) {
    summary[[paste0("Last_SMA_", n)]] <- tail(ma[[paste0("SMA_", n)]], 1)
  }

  list(summary = summary, moving_averages = ma)
}

#' Statistics for every stock in the portfolio.
#' @return list(by_symbol = list of calculate_statistics() results,
#'              summary_table = one row per symbol)
calculate_all_statistics <- function(stocks, ...) {
  by_symbol <- lapply(stocks, calculate_statistics, ...)
  summary_table <- do.call(rbind, lapply(names(by_symbol), function(sym) {
    cbind(Symbol = sym, by_symbol[[sym]]$summary)
  }))
  rownames(summary_table) <- NULL
  list(by_symbol = by_symbol, summary_table = summary_table)
}

# -----------------------------------------------------------------------------
# 4. Display utilities
# -----------------------------------------------------------------------------
#' Header + first and last rows of one stock's data frame.
display_stock_data <- function(df, symbol, n = 5) {
  cat(strrep("=", 70), "\n", sep = "")
  cat(sprintf("%s  |  %d trading days  |  %s to %s\n",
              symbol, nrow(df), format(min(df$Date)), format(max(df$Date))))
  cat(strrep("=", 70), "\n", sep = "")
  cat("First", n, "rows:\n")
  print(head(df, n), row.names = FALSE)
  cat("\nLast", n, "rows:\n")
  print(tail(df, n), row.names = FALSE)
  invisible(df)
}

#' Quote summary table in the style of the Yahoo Finance "Summary" tab.
build_quote_summary <- function(df) {
  last <- df[nrow(df), ]
  prev <- df[nrow(df) - 1, ]
  change     <- last$Close - prev$Close
  change_pct <- 100 * change / prev$Close
  data.frame(
    Field = c("Last close", "Change", "Previous close", "Open",
              "Day's range", "Period range", "Volume", "Avg. volume"),
    Value = c(
      sprintf("%.2f", last$Close),
      sprintf("%+.2f (%+.2f%%)", change, change_pct),
      sprintf("%.2f", prev$Close),
      sprintf("%.2f", last$Open),
      sprintf("%.2f - %.2f", last$Low, last$High),
      sprintf("%.2f - %.2f", min(df$Low), max(df$High)),
      format(last$Volume, big.mark = ","),
      format(round(mean(df$Volume)), big.mark = ",")
    )
  )
}

#' Print the quote summary for one stock.
display_quote_summary <- function(df, symbol) {
  quote <- build_quote_summary(df)
  cat(sprintf("\n%s quote summary (as of %s)\n", symbol, format(max(df$Date))))
  print(quote, row.names = FALSE, right = FALSE)
  invisible(quote)
}

#' Statistics of one stock as a two-column (Statistic, Value) table.
statistics_table <- function(stats) {
  s <- stats$summary
  vals <- vapply(s, function(v) {
    if (inherits(v, "Date")) format(v)
    else if (is.integer(v)) format(v)
    else if (is.numeric(v)) format(round(v, 2), nsmall = 2, big.mark = ",")
    else as.character(v)
  }, character(1))
  data.frame(Statistic = gsub("_", " ", names(vals)), Value = unname(vals))
}

#' Print the statistics of one stock in a readable vertical layout.
display_statistics <- function(stats, symbol) {
  tbl <- statistics_table(stats)
  cat(sprintf("\nStatistics for %s (Close price)\n", symbol))
  print(tbl, row.names = FALSE, right = FALSE)
  invisible(tbl)
}

# -----------------------------------------------------------------------------
# 5. Visualisations
# -----------------------------------------------------------------------------
#' Candlestick chart with 20/50-day SMA and volume (quantmod::chartSeries).
plot_stock_chart <- function(df, symbol) {
  x <- xts(round(df[, c("Open", "High", "Low", "Close", "Volume")], 2), order.by = df$Date)
  chartSeries(x, name = symbol, theme = chartTheme("white"),
              TA = "addVo(); addSMA(n = 20, col = 'blue'); addSMA(n = 50, col = 'red')")
}

#' Close price with moving averages (ggplot2).
plot_price_with_ma <- function(stats, symbol) {
  ma <- stats$moving_averages
  sma_cols <- grep("^SMA_", names(ma), value = TRUE)
  long <- do.call(rbind, lapply(c("Price", sma_cols), function(col) {
    data.frame(Date = ma$Date, Series = col, Value = ma[[col]])
  }))
  long$Series[long$Series == "Price"] <- "Close"
  ggplot(long[!is.na(long$Value), ], aes(Date, Value, colour = Series)) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = c(Close = "grey30", SMA_20 = "#1f77b4", SMA_50 = "#d62728")) +
    labs(title = paste(symbol, "- close price and simple moving averages"),
         x = NULL, y = "Price (USD)", colour = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
}

#' Growth of $100 invested at the start of the period, for every stock.
plot_portfolio_growth <- function(stocks) {
  growth <- do.call(rbind, lapply(names(stocks), function(sym) {
    df <- stocks[[sym]]
    data.frame(Date = df$Date, Symbol = sym,
               Value = 100 * df$Adjusted / df$Adjusted[1])
  }))
  ggplot(growth, aes(Date, Value, colour = Symbol)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
    labs(title = "Growth of $100 invested at the start of the period",
         x = NULL, y = "Value (USD)", colour = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
}
