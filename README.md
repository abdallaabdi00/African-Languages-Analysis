# African Languages Analysis

This project analyzes a public dataset of African languages using R. The analysis explores language diversity by country, language-family speaker patterns, and languages spoken across multiple countries.

## View the Report

Open the rendered HTML report here:

- `docs/index.html` for GitHub Pages
- `report/African_language_report.html` as the original rendered report

After uploading to GitHub, enable GitHub Pages from the `docs` folder to get a shareable webpage link.

## Project Files

- `data/africa_languages.csv` contains the original public dataset.
- `data/africa_languages_clean.csv` contains the cleaned analysis dataset.
- `analysis/African_languages_Analysis.R` contains the R analysis script.
- `analysis/African_language_report.Rmd` contains the R Markdown source report.
- `figures/african_languages_overview.png` contains the combined visualization.
- `docs/index.html` contains the published web-report version.

## Tools Used

- R
- tidyverse
- patchwork
- R Markdown

## How to Reproduce

From the project root, run the R script:

```r
source("analysis/African_languages_Analysis.R")
```

To render the report, open `analysis/African_language_report.Rmd` in RStudio and click Knit, or run:

```r
rmarkdown::render("analysis/African_language_report.Rmd")
```

## Key Questions

- Which African countries have the largest number of spoken languages?
- Which language families have the highest concentration of native speakers?
- Which languages are spoken across multiple countries?
