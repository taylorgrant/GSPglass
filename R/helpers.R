# extract company id from gd url
get_cid <- function(url) {
  last <- sub(".*[-]", "", url)
  cid <- gsub("\\..*", "", last)
  return(cid)
}

## function to convert from one tab to another ##
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# get company name
corp_name <- function(x) {
  x1 <- stringr::str_replace_all(x, "https://www.glassdoor.com/Reviews/", "")
  stringr::str_replace_all(gsub("\\-R.*", "", x1), "-", " ")
}


#' Estimate max
#'
#' @description Estimating the total number of pages to crawl
#'
#' @param companyID Alpha numeric string from url
#'
#' @return Numeric value of total number of pages
#' @export
#'
#' @examples
#' \dontrun{
#' reviews <- estimate_max("E14069")
#' }
estimate_max <- function(companyID) {
  start_url <- "https://www.glassdoor.com/Reviews/Company-Reviews-"
  settings_url <- ".htm?sort.sortType=RD&sort.ascending=false&filter.iso3Language=eng"
  # 1. get the total pages to scrape
  get_maxResults <- function(companyID) {
    # start session
    pg_reviews <- rvest::session(paste0(start_url, companyID, "_P1", settings_url))
    # get reviews and pages
    review_count <- pg_reviews |>
      rvest::html_elements(".activePage .count") |>
      rvest::html_text() |>
      unique() |>
      as.integer()
    return(ceiling(review_count/10))
  }
  cat("Estimating total number of pages we have to crawl...\n")
  max <- get_maxResults(companyID)
}


plot_hc_topics <- function(dat, company, direction) {
  # set up tooltip
  # ct <- nrow(dat$fulldata)
  x <- paste0("Term ", 1:12, ":")
  y <- sprintf("{point.%s} ", paste0("t_", 1:12))
  tltip <- highcharter::tooltip_table(x, y)

  # topic nouns
  dat$fulldata$topic_nouns <- gsub("((?:[^,]+, ){2}[^,]+),", "\\1<br>", dat$fulldata$topic_nouns)
  hc <- highcharter::hchart(
    dat$fulldata,
    "scatter",
    highcharter::hcaes(x, y, name = Topic, size = frac, group = Topic, label = Topic
    ),
    showInLegend = FALSE,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.newid}"
    ),
    minSize = 20,
    maxSize = 70
  ) |>
    highcharter::hc_xAxis(
      title = list(text = "PCA 1"),
      max = dat$range[2],
      min = dat$range[1],
      gridLineWidth = 0,
      lineWidth = 0,
      tickWidth = 0,
      plotLines = list(list(
        value = 0,
        color = '#A9A9A9',
        dashStyle = 'solid',
        width = 1,
        zIndex = 1))
    ) |>
    highcharter::hc_yAxis(
      title = list(text = "PCA 2"),
      max = dat$range[2],
      min = dat$range[1],
      gridLineWidth = 0,
      plotLines = list(list(
        value = 0,
        color = "#A9A9A9",
        dashStyle = "solid",
        width = 1
      ))
    ) |>
    highcharter::hc_title(
      text = glue::glue("Intertopic Distance Map - {company}"),
      align = "left"
    ) |>
    highcharter::hc_subtitle(
      text = glue::glue("Estimated topics for {direction} Reviews"),
      align = "left"
    ) |>
    highcharter::hc_caption(
      text = glue::glue("<b>Mouse over each point to see the topic size and top termss associated with that topic. The score for each word is based upon an optimal blend of topic-specific probability and relevance.</b><br><em>Points are plotted such that they represent 2-dimensional scalings of our topic model. The location of each topic within the two dimensions is informative - the closer a topic is to another, the more similar the topics (and vice versa). The size of each bubble indicates the size of the topic relative to others. The number of topics has been selected via coherence scoring; in this case {nrow(dat$fulldata)} topics is the ideal.</em>")
    ) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      table = FALSE,
      headerFormat = "<span style=\"color:{series.color}\">\u25CF</span> <b>{point.key}</b><br><span style=\"color:{series.color}\">\u25CF</span> <b>{point.point.topic_nouns}</b> (Size: {point.point.pct}) <br><br> <b> Term #: Word (Score)</b> ",
      pointFormat = tltip
    ) |>
  highcharter::hc_exporting(enabled = TRUE)
return(hc)
}


dt_topic_table <- function(dat, company, direction) {

  nms <- c("Topic", "Topic Size", paste("Term", 1:12))
  dat$fulldata  |>
    dplyr::mutate(topic = paste0(Topic,": ", topic_nouns)) |>
    dplyr::select(topic, `Topic Size` = pct, dplyr::starts_with("t_")) |>
    purrr::set_names(nms) |>
    tidyr::pivot_longer(names_to = "Term",
                 values_to = "Score",
                 -c("Topic", "Topic Size")) |>
    dplyr::select(-c(`Topic Size`)) |>
    tidyr::pivot_wider(names_from = Topic,
                values_from = c(Score)) |>
    DT::datatable(rownames = FALSE,
                  options = list(dom = "Blrtip",
                                 pageLength = 12,
                                 buttons = c('excel'),
                                 scrollX = TRUE),
                  caption = glue::glue("Top terms per {direction} topic: {company}"))

}

# summarise the gd data by time
data_summary <- function(dat, time, url) {
  company <- corp_name(url)
  grp <- dplyr::sym(time)

  tmpdata <- dat |>
    dplyr::mutate(month = lubridate::floor_date(review_date, "month"),
           quarter = lubridate::floor_date(review_date, "quarter"),
           year = lubridate::year(review_date)) |>
    dplyr::group_by(!!grp) |>
    dplyr::summarise(rev_ct = dplyr::n(),
                     avg = mean(rating))

  summary_dt <- DT::datatable(tmpdata,
                              colnames = c(stringr::str_to_title(time), "Review Count", "Average Rating"),
                rownames = FALSE,
                options = list(dom = "Blrtip",
                               order = list(0, "desc"),
                               pageLength = 10,
                               buttons = c('excel'),
                               scrollX = TRUE),
                caption = glue::glue("Summary of Reviews for {company} by {stringr::str_to_title(time)}")) |>
    DT::formatRound(columns = 3, digits = 1)

  summary_hc <- highcharter::highchart() |>
    highcharter::hc_yAxis_multiples(
      list(title = list(text = "Review Count")),
      list(title = list(text = "Average Rating"), opposite = TRUE,
           labels = list(max = 5))
    ) |>
    highcharter::hc_xAxis(title = list(text = glue::glue("{stringr::str_to_title(time)}")),
                          type = "category") |>
    highcharter::hc_add_series(data = tmpdata, "column",
                               highcharter::hcaes(x = factor(!!grp), y = rev_ct),
                  name = "Review Count", yAxis = 0) |>
    highcharter::hc_add_series(data = tmpdata, "line",
                               highcharter::hcaes(x = factor(!!grp), y = round(avg, 1)),
                  name = "Average Rating", yAxis = 1) |>
    highcharter::hc_tooltip(shared = TRUE) |>
    highcharter::hc_title(
      text = glue::glue("Summary of Reviews for {company}"),
      align = "left"
    ) |>
    highcharter::hc_subtitle(
      text = glue::glue("By {stringr::str_to_title(time)}"),
      align = "left"
    ) |>
    highcharter::hc_exporting(enabled = TRUE)

  summary_out <- list(summary_dt = summary_dt,
                      summary_hc = summary_hc)
}

hc_wordcloud <- function(dat, type) {
  tmpdat <- dat |>
    dplyr::pull(!!dplyr::enquo(type)) |>
    purrr::map(stringr::str_to_lower) |>
    stringr::str_split("\\s+") |>
    unlist() |>
    tibble::tibble() |>
    purrr::set_names("word") |>
    dplyr::mutate(word = gsub("(?!\\')[[:punct:]]", " ", word, perl=TRUE)) |>
    dplyr::count(word, sort = TRUE) |>
    dplyr::anti_join(tidytext::stop_words, by = "word") |>
    utils::head(80)

  highcharter::hchart(tmpdat, "wordcloud", highcharter::hcaes(name = word, weight = log(n))) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      table = FALSE,
      headerFormat = "",
      pointFormat = "<span style=\"color:{series.color}\">\u25CF</span> <b>Word Count</b><br>{point.word}: {point.n}<br>"
    ) |>
    highcharter::hc_title(text = glue::glue("Glassdoor {stringr::str_to_title(type)} Wordcloud"),
                          align = "left") |>
    highcharter::hc_caption(
      text = glue::glue("<em>Top 80 terms used with stop words removed. Based on {nrow(dat)} total reviews.</em>")
    ) |>
    highcharter::hc_exporting(enabled = TRUE)

}

hc_sum_rating <- function(dat) {
  tmpdat <- dat |>
    dplyr::count(rating) |>
    dplyr::mutate(total = round((n/sum(n))*100,1))

  highcharter::highchart() |>
    highcharter::hc_xAxis(title = list(text = "Rating"),
                          type = "category") |>
    highcharter::hc_add_series(data = tmpdat, "column",
                               highcharter::hcaes(x = rating, y = n),
                               name = "Reviews", yAxis = 0) |>
    highcharter::hc_tooltip(
      useHTML = TRUE,
      table = FALSE,
      headerFormat = "",
      pointFormat = "<span style=\"color:{series.color}\">\u25CF</span> <b>Rating of: {point.rating}</b><br>Count: {point.n}<br>{point.total}% of all reviews"
    )

}
