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
  # add headers
  headers = c(
    `sec-ch-ua` = '"Chromium";v="116", "Not)A;Brand";v="24", "Google Chrome";v="116"',
    `Referer` = "https://www.glassdoor.com/",
    `DNT` = "1",
    `Accept-Language` = "en",
    `sec-ch-ua-mobile` = "?0",
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/116.0.0.0 Safari/537.36",
    `sec-ch-ua-platform` = '"macOS"'
  )
  # scrape and put into dataframe
  pb <- progress::progress_bar$new(total = max)
  get_reviews <- function(pg){
    pb$tick()
    Sys.sleep(3)
    tryCatch({

      start_url <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
      settings_url <- ".htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
      # pg_reviews <- rvest::session(paste0(start_url, companyID, "_P", pg, settings_url))
      pg_reviews <- httr::content(httr::GET(url = paste0(start_url, companyID, "_P", pg, settings_url),
                                            httr::add_headers(.headers = headers)))
      data.frame(date = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__reviewDate") |>
                   rvest::html_text2(),

                 title = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__employee") |>
                   rvest::html_text2(),

                 summary = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__title") |>
                   rvest::html_text(),

                 rating = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__overallRating") |>
                   rvest::html_text(),

                 employee_type = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__employeeDetails:nth-child(1)") |>
                   rvest::html_text(),

                 pros = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__pro span") |>
                   rvest::html_text(),

                 cons = pg_reviews |>
                   rvest::html_elements(".review-details__review-details-module__con span") |>
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
                    title = trimws(title),
                    rating = as.numeric(stringr::str_replace_all(rating, ".0", "")),
                    status = gsub("\\,.*", "", employee_type),
                    duration = trimws(ifelse(stringr::str_detect(employee_type, ","),
                                             gsub(".*\\,", "", employee_type),
                                             NA_character_))) |>
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
