#' Scrape Glassdoor Reviews
#'
#' @description Function to scrape Glassdoor reviews and put into a tidy format
#'
#' @param corp Company name from url - With the url, can also run `corp_name(url)$hyphen_name` to get it
#' @param companyID String with alpha-numeric company ID
#' @param min Page to begin scraping; defaults to 1
#' @param max Maximum number of pages to scrape
#'
#' @return Tibble review date, employee title, employee status, duration, summary, rating, pros, and cons
#' @export
#'
#' @examples
#' \dontrun{
#' reviews <- scrape_glassdoor("State-Farm," E2990", min, max)
#' }
scrape_glassdoor <- function(corp, companyID, min = 1, max) {
  Sys.sleep(3)
  # scrape and put into dataframe
  pgs <- (max - min)+1
  pb <- progress::progress_bar$new(total = pgs)
  get_reviews <- function(pg){

    # extract employer reviews
    expand_employer_review <- function(lst) {
      expanded_lst <- lapply(names(lst), function(name) {
        if (grepl("^EmployerReviewRG", name)) {
          return(lst[[name]])
        } else {
          return(NULL)
        }
      })
      expanded_lst <- Filter(Negate(is.null), expanded_lst) # Remove NULL values
      return(expanded_lst)
    }
    # employer name from id
    get_employer_name <- function(empID, nested_list) {
      if (!is.null(empID)) {
        text <- nested_list$props$pageProps$apolloCache[[empID]]$shortName
      } else {
        text <- NA
      }
      return(text)
    }
    # employee job title from id
    get_jobtitle_text <- function(jobID, nested_list) {
      if (!is.null(jobID)) {
        text <- nested_list$props$pageProps$apolloCache[[jobID]]$text
      } else {
        text <- NA
      }
      return(text)
    }
    # employee location from id
    get_location_text <- function(locID, nested_list) {
      if (!is.null(locID)) {
        text <- nested_list$props$pageProps$apolloCache[[locID]]$name
      } else {
        text <- NA
      }
      return(text)
    }

    pb$tick()
    Sys.sleep(3)
    tryCatch({

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

      settings_url <- "?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
      url <- glue::glue("https://www.glassdoor.com/Reviews/{corp}-Reviews-{companyID}_P{pg}.htm{settings_url}")

      pg_reviews <- httr::content(httr::GET(url,httr::add_headers(.headers = headers)))
      # pulling data from the JS script
      tmp <- pg_reviews |>
        rvest::html_elements("script#__NEXT_DATA__") |>
        rvest::html_text() |>
        jsonlite::fromJSON()

      # expand reviews and put list to dataframe
      out <- expand_employer_review(tmp$props$pageProps$apolloCache) |>
        data.table::rbindlist() |>
        tibble()

      # clean and extract data
      out |>
        dplyr::rowwise() |>
        dplyr::mutate(employer = get_employer_name(employer, tmp),
               jobTitle = get_jobtitle_text(jobTitle, tmp),
               location = get_location_text(location, tmp),
               lengthOfEmployment = dplyr::case_when(lengthOfEmployment == 0 ~ NA,
                                              lengthOfEmployment == 1 ~ "Less than 1 year",
                                              TRUE ~ glue::glue("More than {lengthOfEmployment-1} years")),
               review_date = as.Date(lubridate::as_datetime(reviewDateTime)),
               status = ifelse(isCurrentJob == "TRUE", "current_employee", "former_employee"),
               dplyr::across(c(advice, cons, pros, summary), ~stringr::str_replace_all(., "\n", "; ")),
               dplyr::across(c(advice, cons, pros, summary), ~stringr::str_replace_all(., "&amp;", "&"))) |>
        dplyr::select(employer, review_date, title = jobTitle, status, duration = lengthOfEmployment,
               rating = ratingOverall, summary, pros, cons, advice,
               ratingBusinessOutlook:ratingWorkLifeBalance
        ) |>
        dplyr::mutate(across(summary:cons, trimws))

      }, error = function(e){
        NULL
      })
  }
  cat("Pulling all reviews now...\n")
  out <- min:max |>
    purrr::map_dfr(get_reviews)
}
