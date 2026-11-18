# =============================================================================
# 00_install_packages.R  —  install the R packages used for technical analysis
# -----------------------------------------------------------------------------
# AI Assistance Declaration: I used Claude (Anthropic, Claude Opus 5.5, Claude
# desktop app, 2026-11-18) for drafting this script. I verified it by running
# it in RStudio and checking that every package loads. I am responsible for the
# accuracy and originality of this work.
# =============================================================================

packages <- c(
  "quantmod",   # download & chart financial data (installs xts, zoo, TTR)
  "TTR",        # Technical Trading Rules: SMA, EMA, RSI, MACD, ...
  "xts", "zoo", # time-series classes used by quantmod
  "ggplot2",    # charts
  "knitr",      # tables in R Markdown
  "rmarkdown"   # knit the .Rmd report
)

repo <- "https://cloud.r-project.org"
to_install <- packages[!packages %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, repos = repo)
}

# Verify: load every package and print its version
for (p in packages) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}
print(data.frame(Package = packages,
                 Version = vapply(packages, function(p) as.character(packageVersion(p)), character(1)),
                 row.names = NULL))
cat("\nR version:", R.version.string, "\n")
