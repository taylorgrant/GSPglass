#' Scrape Glassdoor Reviews
#'
#' @description Function to scrape Glassdoor reviews and put into a tidy format
#'
#' @param companyID String with alpha-numeric company ID
#' @param max Maximum number of pages to scrape
#'
#' @return Tibble review date, employee title, employee status, duration, summary, rating, pros, and cons
#' @export
#'
#' @examples
#' \dontrun{
#' reviews <- scrape_glassdoor("E14069", max)
#' }
scrape_glassdoor <- function(companyID, max) {
  Sys.sleep(3)
  # scrape and put into dataframe
  pb <- progress::progress_bar$new(total = max)
  get_reviews <- function(pg){
    pb$tick()
    Sys.sleep(3)
    tryCatch({

      start_url <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
      settings_url <- ".htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
      pg_reviews <- rvest::session(paste0(start_url, companyID, "_P", pg, settings_url))

      data.frame(date = pg_reviews |>
                   rvest::html_elements(".middle.common__EiReviewDetailsStyle__newGrey") |>
                   rvest::html_text2(),

                 summary = pg_reviews |>
                   rvest::html_elements(".reviewLink") |>
                   rvest::html_text(),

                 rating = pg_reviews |>
                   rvest::html_elements("#ReviewsFeed .mr-xsm") |>
                   rvest::html_text(),

                 employee_type = pg_reviews |>
                   rvest::html_elements(".eg4psks0") |>
                   rvest::html_text(),

                 pros = pg_reviews |>
                   rvest::html_elements(".v2__EIReviewDetailsV2__fullWidth:nth-child(1) span") |>
                   rvest::html_text(),

                 cons = pg_reviews |>
                   rvest::html_elements(".v2__EIReviewDetailsV2__fullWidth:nth-child(2) span") |>
                   rvest::html_text()

      )}, error = function(e){
        NULL
      })
  }
  cat("Pulling all reviews now...\n")
  out <- 1:max |>
    purrr::map_dfr(get_reviews)

  # 3. tidy the reviews and get in order
  tidy_reviews <- function(tbl) {
    tbl |>
      dplyr::mutate(review_date = as.Date(trimws(gsub("\\-.*", "", date)), format = "%b %d, %Y"),
                    title = trimws(gsub(".*\\-", "", date)),
                    rating = as.numeric(stringr::str_replace_all(rating, ".0", "")),
                    status = gsub("\\,.*", "", employee_type),
                    duration = trimws(gsub(".*\\,", "", employee_type))) |>
      dplyr::select(-c(date, employee_type)) |>
      dplyr::relocate(review_date, .before = dplyr::everything()) |>
      dplyr::relocate(title, .after = "review_date") |>
      dplyr::relocate(status, .after = "title") |>
      dplyr::relocate(duration, .after = "status") |>
      dplyr::arrange(dplyr::desc(review_date))
  }
  cat("Cleaning up and parsing...\n")
  out_tidy <- tidy_reviews(out)
}
