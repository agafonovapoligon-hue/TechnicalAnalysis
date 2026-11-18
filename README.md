# TechnicalAnalysis

> **AI Assistance Declaration:** I used Claude (Anthropic, Claude Opus 5.5, Claude desktop app, 2026-11-18) for drafting code structure, documentation and the report layout. I verified outputs by running and knitting all code in RStudio and checking the results. All final calculations are done by R. I am responsible for the accuracy and originality of this work.

Course repository for **BDA400 — Data Science Tools and Techniques** (CDI College).
It holds a three-stage stock **technical analysis** project in R; each assignment
lives in its own folder.

| Folder | Assignment | Status |
|--------|------------|--------|
| [`A2_Preliminary_Stage/`](A2_Preliminary_Stage) | Assignment 2 — setup, data import, basic statistics, display | ✅ Complete |
| `A3_…` | Next stage | Upcoming |
| `A4_…` | Final stage | Upcoming |

## Assignment 2 — Preliminary Stage

| File | Purpose |
|------|---------|
| `portfolio.txt` | Stock symbols in the portfolio, one per line |
| `R/ta_functions.R` | Utility functions: `read_portfolio()`, `load_stock_data()`, `calculate_statistics()`, `calculate_all_statistics()`, `get_mode()`, display and plotting helpers |
| `run_analysis.R` | Script that runs the full workflow in the R console |
| `technical_analysis_A2.Rmd` | R Markdown report: data import, statistics tables and charts |
| `technical_analysis_A2.html` | Knitted R Markdown report |
| `ViktoriiaAgafonova_BDA400_A02.docx` | Assignment report with step-by-step screenshots |

### Quick start

```r
# R >= 4.1 and RStudio
install.packages(c("quantmod", "TTR", "ggplot2", "knitr", "rmarkdown"))

setwd("A2_Preliminary_Stage")
source("run_analysis.R")                      # console output + charts
rmarkdown::render("technical_analysis_A2.Rmd") # HTML report
```

### Statistics computed (Close price)

Mean · Median · Mode (prices rounded to whole dollars) · Standard deviation ·
20- and 50-day simple moving averages · Min / Max · Mean daily return · Daily volatility

Data source: Yahoo Finance via `quantmod::getSymbols()`. For educational use only — not investment advice.
