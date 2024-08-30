
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSPglass

<!-- badges: start -->
<!-- badges: end -->

GSPglass is an interactive Shiny app that scrapes, summarizes, and topic
models Glassdoor reviews. Install the package, run the app, and follow
the directions provided.

## Installation

You can install the development version of GSPglass from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("taylorgrant/GSPglass")
```

## Example

To call the Shiny app, do as follows:

``` r
library(GSPglass)
runApp()
```

The modal pop-up explains everything. Additional information about the
topic modeling process can be found in the About section of the app.

A standardized rmarkdown report is included in the app. After topic
modeling, the user may choose to generate the report.

## Using without the Shiny app

``` r
library(GSPglass)

# copy url from glassdoor
url <- https://www.glassdoor.com/Reviews/State-Farm-Reviews-E2990.htm

# get CID
companyID <- get_cid(url)
# get the company name
corp <- corp_name(url)
# how many pages of reviews are there? 
max_pages <- estimate_max(corp$hyphen_name, companyID)

# is max_pages too long? or do you want it all? 
gd_reviews <- scrape_glassdoor(corp$hyphen_name, companyID, max_pages)
```
