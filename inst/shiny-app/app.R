library(shiny)

## function to convert from one tab to another ##
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# header ------------------------------------------------------------------
header <- shinydashboard::dashboardHeader(title = "Glassdoor Scraper",
                          shiny::tags$li(shiny::a(href = 'https://goodbysilverstein.com/',
                                    shiny::img(src = 'https://images.squarespace-cdn.com/content/v1/5ea9e40f1c49ae0355b4d859/1615824766494-NPIZRASFQS47W5GHS0UY/RepData_Goodby_Silverstein.png',
                                        title = "GS&P", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"))

# sidebar -----------------------------------------------------------------
sidebar <- shinydashboard::dashboardSidebar(
  width = 325,
  shinydashboard::sidebarMenu(
    id = "tabs",
    convertMenuItem(
      shinydashboard::menuItem("Scraper", tabName = "scraper",
               icon = shiny::icon("square-poll-vertical"),
               selected=T,
               shinydashboard::menuItem("Company:",
                                        shiny::textInput("url", label = "Enter a Glassdoor url"),
                                        shiny::actionButton('submit', "Submit")),
               shinydashboard::menuItem(shiny::htmlOutput("duration")),
               shiny::uiOutput("proceed", align = "center")
               ), "scraper"
    ),
    convertMenuItem(
      shinydashboard::menuItem("Summary Data", tabName = "summary",
                               icon = shiny::icon("square-poll-vertical"),
                               selected=F,
                               shiny::selectInput("timeframe", label = "Aggregate by:",
                                                  choices = list("Year" = "year",
                                                                 "Quarter" = "quarter",
                                                                 'Month' = "month"),
                                                  selected = 'year')
      ),"summary"),
    convertMenuItem(
      shinydashboard::menuItem("Topic Modeling", tabName = "topics",
               icon = shiny::icon("square-poll-vertical"),
               selected=F,
               shinydashboard::menuItem("Topic Model both Pros and Cons?",
                                        shinydashboard::menuItem(shiny::uiOutput("topicmodel"))),
               shinydashboard::menuItem("Generate an HTML report?",
                                        shiny::downloadButton("report", "Yes", class = "dlbtn"))
      ),"topics"),
    convertMenuItem(
      shinydashboard::menuItem("About", tabName = "notes",
                               icon = shiny::icon("square-poll-vertical"),
                               selected=F,
                               shinydashboard::menuItem("About")
      ),"notes")
  )
)


# body --------------------------------------------------------------------
body <- shinydashboard::dashboardBody(
  ## CSS styling for the validation error message on Monthly Sales ##
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
    .shiny-output-error-validation {
      color: white;
      font-size: 100%;
    }
    .dlbtn {
      width: 200px;
      position:relative;
      left:40px;
    }
  "))
  ),
  # tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  shinydashboard::tabItems(
    # conditionally render the output using inputs (server side)
    shinydashboard::tabItem("scraper", shiny::uiOutput("tab1"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
    shinydashboard::tabItem("summary", shiny::uiOutput("tab2"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
    shinydashboard::tabItem("topics", shiny::uiOutput("tab3"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500)),
    shinydashboard::tabItem("notes", shiny::uiOutput("tab4"),
            shinysky::busyIndicator(text = 'Please wait...', wait = 1500))

  )
)

# build the UI ------------------------------------------------------------
ui = shinydashboard::dashboardPage(header, sidebar, body, skin = "black")


### ----- SERVER ----- ###
server = function(input, output, session) {

  shiny::showModal(shiny::modalDialog(shiny::HTML("<b>What is this?</b> This is an app that scrapes and summarizes Glassdoor Reviews.<br><br>
                                                  <b>How do I use it?</b> Search Glassdoor for the company of interest.
                                                  After you find the company, copy the URL and then paste it into the URL text field at
                                                  left under the <b>Scraper</b> tab. The app will then let you know how many pages
                                                  have to be scraped with an estimate of how long it will take. If you press the <code>Yes</code>
                                                  button, it will scrape the site. If you press <code>No</code>, the app will reset itself.<br><br>
                                                  <b>What happens after it scrapes?</b> When the scrape is complete, summary info boxes will appear along
                                                  with a table of all reviews. These reviews are searchable using the filter tabs above each column, or via
                                                  the search bar in the upper right. If you want, you can download all reviews by pushing the <code>Excel</code>
                                                  button.<br><br>If you want to explore the data further, select the <b>Summary Data</b> tab. Here you
                                                  will find summary data by year, quarter, and month as well as wordclouds for various parts of the
                                                  reviews (Summary, Pros, Cons). All plots are downloadable via the hamburger menu in the upper right of
                                                  each plot.<br><br><b>Topic Modeling:</b> If you want to take the analysis further, you can choose to
                                                  topic model the reviews data. Because reviews come in two flavors - Pros and Cons - each are modeled using
                                                  a model designed for shorter texts (for more info see the <b>About</b> tab).<br><br>The topic modeling process
                                                  involves running the model tens of thousands times, so this process will take a few minutes. <em>Be patient</em>.
                                                  You can see how the modeling process is going by referring back to the output in the R console."),
                                 easyClose = T))


  # Shiny Tab 1 -------------------------------------------------------------
  rv <- shiny::reactiveValues(data = data.frame(), name = "data")

  # on submission of URL #
  shiny::observeEvent(input$submit, {
    # require url
    shiny::req(input$url)
    # validate that url is proper
    output$duration <- shiny::renderUI({
      shiny::validate(
        shiny::need(stringr::str_detect(input$url, "glassdoor.com/Reviews"), "Please submit a properly formatted url from Glassdoor.")
        )
      # company id
      cid <- get_cid(input$url)
      xx <- estimate_max(cid)
      shiny::HTML(glue::glue("<p><br>There are <span style='color:white'><b>{xx} pages</b></span> to scrape,
                        which will take <br>at least <span style='color:white'><b>{ceiling((xx*6)/60)}
                        minutes</b></span> to finish.<br>Would you like to proceed?<br></p>"))
      })

    # if input url is proper, show buttons; else nothing
    if (stringr::str_detect(input$url, "glassdoor.com/Reviews")) {
      output$proceed <- shiny::renderUI({
        shiny::div(
          shiny::div(style="display: inline-block; width: 110px ;", shiny::actionButton("proceedYes", "Yes")),
          shiny::div(style="display: inline-block; width: 110px ;", shiny::actionButton("proceedNo", "No"))
        )
      })
    }

    # if yes to proceed, scrape the reviews
    shiny::observeEvent(input$proceedYes, {

        cid <- get_cid(input$url)
        xx <- estimate_max(cid)
        rv$data <- scrape_glassdoor(cid, xx)

      # render table once completed
      output$table <- DT::renderDT(server = FALSE, {
        DT::datatable(rv$data,
                      colnames = c("Review Date", "Job Title", "Employment Status",
                                   "Employment Duration", "Summary", "Rating",
                                   "Pros", "Cons"),
                      extensions = "Buttons",
                      filter = list(position = "top", clear = FALSE),
                      rownames = FALSE,
                      options = list(
                        # searching = FALSE,
                        dom = "Blftp",
                        buttons = c('excel'),
                        columnDefs = list(list(width = '300px', targets = c(6,7)))),
                      caption = corp_name(input$url))
      })

      output$tab1 <- shiny::renderUI({
        tab1_ui <- shinydashboard::tabItem("scraper",
                                           shiny::h4(paste("Reviews for: ", corp_name(input$url))),
                                           value="test1",
                           shiny::fluidRow(
                             shinydashboard::infoBox("Total Reviews", nrow(rv$data),
                                                     width = 3),
                             shinydashboard::infoBox("Avg. Rating (95% C.I.)", glue::glue("{round(mean(rv$data$rating), 1)}",
                                                                                          " ({round(confint(lm(rating ~ 1, rv$data), level = 0.95),2)[1]}",
                                                                                          " - {round(confint(lm(rating ~ 1, rv$data), level = 0.95),2)[2]})"),
                                                     width = 3),
                             shinydashboard::infoBox("Avg. Rating - Current Employees", glue::glue("{round(mean(rv$data[rv$data$status == 'Current Employee',]$rating), 1)}",
                                                                                                   " ({round(confint(lm(rating ~ 1, subset(rv$data, status == 'Current Employee')), level = 0.95),2)[1]}",
                                                                                                   " - {round(confint(lm(rating ~ 1, subset(rv$data, status == 'Current Employee')), level = 0.95),2)[2]})"),
                                                     width = 3),
                             shinydashboard::infoBox("Avg. Rating - Former Employees", glue::glue("{round(mean(rv$data[rv$data$status == 'Former Employee',]$rating), 1)}",
                                                                                                  " ({round(confint(lm(rating ~ 1, subset(rv$data, status == 'Former Employee')), level = 0.95),2)[1]}",
                                                                                                  " - {round(confint(lm(rating ~ 1, subset(rv$data, status == 'Former Employee')), level = 0.95),2)[2]})"),
                                                     width = 3)
                           ),
                           shiny::fluidRow(
                             shinydashboard::box(
                               width = 12,
                               DT::dataTableOutput("table"))
                             ))
        })

    })
    shiny::observeEvent(input$proceedNo, {
      session$reload()
    })
    })


  # TAB 2 - SUMMARY ---------------------------------------------------------

  output$summary_hc <- highcharter::renderHighchart({
    data_summary(rv$data, input$timeframe, input$url)$summary_hc
  })
  output$summary_dt <- DT::renderDT(
    data_summary(rv$data, input$timeframe, input$url)$summary_dt
  )
  output$summary_rating_hc <- highcharter::renderHighchart({
    hc_sum_rating(rv$data)
  })

  output$hc_wc_sum <- highcharter::renderHighchart({
    hc_wordcloud(rv$data, "summary")
  })
  output$hc_wc_pro <- highcharter::renderHighchart({
    hc_wordcloud(rv$data, "pros")
  })
  output$hc_wc_con <- highcharter::renderHighchart({
    hc_wordcloud(rv$data, "cons")
  })

  output$tab2 <- shiny::renderUI({
    shiny::validate(
      shiny::need(nrow(rv$data) > 0, "Need data")
    )
    tab2_ui <- shinydashboard::tabItem("summary_dat",
                                       shiny::h4(paste("Reviews for: ", corp_name(input$url))),
                                       value="test2",
                                       shiny::fluidRow(
                                         shinydashboard::tabBox(
                                           width = 6,
                                           shiny::tabPanel("Ratings Split",
                                                           highcharter::highchartOutput("summary_rating_hc",
                                                                                        height = "485px"
                                                                                        )),
                                           shiny::tabPanel("Avg. Rating", DT::dataTableOutput("summary_dt"))
                                         ),
                                         shinydashboard::box(
                                           width = 6,
                                           highcharter::highchartOutput("summary_hc",height = "525px")
                                         )
                                       ),
                                       shiny::fluidRow(
                                         shinydashboard::tabBox(
                                           width = 12,
                                           shiny::tabPanel("Wordcloud: Summary", highcharter::highchartOutput("hc_wc_sum", height = "600px")),
                                           shiny::tabPanel("Wordcloud: Pros", highcharter::highchartOutput("hc_wc_pro", height = "600px")),
                                           shiny::tabPanel("Wordcloud: Cons", highcharter::highchartOutput("hc_wc_con", height = "600px"))
                                       )
                                       )
    )
  })


  # TAB 3 - TOPIC MODEL -----------------------------------------------------

  output$topicmodel <- shiny::renderUI({
        shiny::actionButton("tmpro", "Yes! model the reviews", style = "width:200px")
    })

  shiny::observeEvent(input$tmpro, {
    shiny::validate(
      shiny::need(nrow(rv$data) > 0, "Need data")
    )

    data_out <- reactive({
      pros <- rv$data |>
        dplyr::rename(text = pros) |>
        GSPbtm::complete_btm(5, 15, "NAV")

      cons <- rv$data |>
        dplyr::rename(text = cons) |>
        GSPbtm::complete_btm(5, 15, 'NAV')

      out <- list(pros = pros,
                  cons = cons,
                  name = corp_name(input$url))
    })

    # highcharter
    output$hc_tm_pro <- highcharter::renderHighchart({
      company <- corp_name(input$url)
      plot_hc_topics(data_out()$pros, company, "Pro")
    })

    output$hc_tm_con <- highcharter::renderHighchart({
      company <- corp_name(input$url)
      plot_hc_topics(data_out()$cons, company, "Con")
    })

    output$hc_pro_time <- highcharter::renderHighchart({
      hc_topic_time(data_out()$pros)
    })

    output$hc_con_time <- highcharter::renderHighchart({
      hc_topic_time(data_out()$cons)
    })

    # datatables
    output$dt_tm_pro <- DT::renderDT({
      company <- corp_name(input$url)
      dt_topic_table(data_out()$pros, company, "Pros")
    })

    output$dt_tm_con <- DT::renderDT({
      company <- corp_name(input$url)
      dt_topic_table(data_out()$cons, company, "Cons")
    })

    # download handler for rmarkdown
    output$report <-shiny::downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- system.file("rmd", "report.Rmd", package="GSPglass")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(data = data_out())

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

    output$full_table <- DT::renderDT(server = FALSE, {
      dat <- data_out()$pros$df |>
        dplyr::select(review_date:model_topic) |>
        dplyr::rename(pro_topic = model_topic) |>
        dplyr::left_join(dplyr::select(data_out()$cons$df, c(doc_id, con_topic = model_topic)),
                         by = c("doc_id" = "doc_id"))

      DT::datatable(dat,
                    colnames = c("Review Date", "Job Title", "Employment Status",
                                 "Employment Duration", "Summary", "Rating",
                                 "Pros", "Cons", "doc_id", "Pros: Topic", "Cons: Topic"),
                    extensions = "Buttons",
                    filter = list(position = "top", clear = FALSE),
                    rownames = FALSE,
                    options = list(
                      # searching = FALSE,
                      dom = "Blftp",
                      buttons = c('excel'),
                      columnDefs = list(list(width = '300px', targets = c(6,7)),
                                        list(visible = FALSE, targets = c("doc_id")))),
                    caption = corp_name(input$url))

    })

  output$tab3 <- shiny::renderUI({
    tab3_ui <- shinydashboard::tabItem("tm",
                                       shiny::h4(paste("Reviews for: ", corp_name(input$url))),
                                       value="test3",
                                       shiny::fluidRow(
                                           shinydashboard::tabBox(
                                             width = 12,
                                             shiny::tabPanel("Positives", highcharter::highchartOutput("hc_tm_pro", height = "800px")),
                                             shiny::tabPanel("Positives (Table)", DT::dataTableOutput("dt_tm_pro")),
                                             shiny::tabPanel("Negatives", highcharter::highchartOutput("hc_tm_con", height = "800px")),
                                             shiny::tabPanel("Negatives (Table)", DT::dataTableOutput("dt_tm_con")),
                                             shiny::tabPanel("All Data", DT::dataTableOutput("full_table"))
                                         ),
                                         shinydashboard::tabBox(
                                           width = 12,
                                           shiny::tabPanel("Pro Topics over Time", highcharter::highchartOutput("hc_pro_time", height = "500px")),
                                           shiny::tabPanel("Con Topics over Time", highcharter::highchartOutput("hc_con_time", height = "500px"))
                                         )
                                       )
    )
  })
  })




  # TAB 4 - NOTES -----------------------------------------------------------
  output$notes <- shiny::renderUI({
    shiny::HTML("<b>About Topic Modeling:</b> Topic modeling is an unsupervised dimension reduction method; in this case, we are
         leveraging the BTM model, which uses 'word-word co-occurrence patterns' (e.g., biterms) within the
         texts. The BTM model excels at finding topics in shorter texts such as survey responses, tweets, and online reviews.
         For more detail, the reference paper for the model can be found
         <a href='https://github.com/xiaohuiyan/xiaohuiyan.github.io/blob/master/paper/BTM-WWW13.pdf' target='_blank'>here</a>.<br><br>
         To properly model the text, the app has to tag appropriate parts of speech. To do this the app
         <b>will download a pre-trained annotator model to your local directory</b> (you will be given the option to delete it via a
         button push after the model runs), which it will then use to tokenize, tag, and parse
         each word. It will also capture the co-occurrence frequency of all nouns, verbs, and adjectives that are within 3 skips
         of each term within each review text. This data will then be passed into the BTM model. The BTM algorithm will then
         estimate a model for varying number of topics - 5 to 15 topics - and will select the best one based on each model's coherence
         score.<br><br>When the best model is identified, the topics are then plotted into a 2-dimensional space via the methodology outlined
         <a href='https://aclanthology.org/W14-3110.pdf' target='_blank'>here</a>. The distance between topics in
         this visualization is meaningful - the closer the topics, the more similar they are. The bubble size indicates the size of the
         topic relative to others, mousing over each bubble will provide the top 4-5 nouns, which serve as a crude descriptor of the topic.
         Additional data provides includes the proportion of the topic and the top 12 most relevant and salient terms for that topic.
         Because Pros and Cons are separate texts, each is modeled and visualized separately.
         <br><br>Because the modeling process is unsupervised, there is no north star as
         to the correct number of topics. Some may potentially overlap, or seem redundant. Take the time to read through the reviews for
         specific topics to see how consistent the overlap may be. This is meant as a quick way to summarize reviews, so use it as an aid, but
         interpretation of the output, as well as whether its accurate or not, is up to the user.")
  })

  output$tab4 <- shiny::renderUI({
    tab4_ui <- shinydashboard::tabItem("notes",
                                       shiny::h4(paste("Reviews for: ", corp_name(input$url))),
                                       value="test4",
                                       shiny::fluidRow(
                                         shinydashboard::box(
                                           width = 12,
                                           shiny::uiOutput("notes")
                                         )
                                       )
    )
  })



}

shiny::shinyApp(ui, server)



