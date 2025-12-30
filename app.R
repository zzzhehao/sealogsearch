source("functions.R")
library(shiny)
library(bslib)
library(DT)
library(markdown)
library(tidyverse)

## config

status <- readRDS("cache/status_report.RDS")
all_sealog_data <- map_dfr(status$cruise_dive.tbl$cache_path, readRDS) %>%
    as_tibble()
youtube_meta <- youtube_yaml("replay-meta.yml")

## UI
# ui <- fluidPage(
ui <- page_sidebar(
    card(
        # Display
        textOutput("dive_cruise_compCheck"),
        uiOutput("summary"),
        uiOutput("results_area")
    ),
    title = titlePanel("Sealog search"),
    sidebar = sidebar(
        width = 330,
        textInput("searchText", "Search keyword", "Keyword to search"),
        actionButton("search", "Search"),
        hr(),
        checkboxInput("cruise_all", "All cruises", T),
        selectInput("cruises", "Select cruises", character(0), selected = NULL, multiple = T),
        checkboxInput("dive_all", "All dives", T),
        selectInput("dives", "Select dives", character(0), selected = NULL, multiple = T),
        card(
            checkboxInput("exact", "Full-word exact match only", F),
            checkboxInput("expansion", "No potential indel-typo", F),
            checkboxInput("case_sensitive", "Case sensitive", F),
            sliderInput(
                "threshold", 
                "Ambiguous search threshold", 
                min = 0.1, 
                max = 0.3, 
                value = 0.2,
                step = 0.1
            ),
        ),
        downloadButton("download_csv", "Download search result"),
        uiOutput("info")
    )
)

server <- function(input, output, session) {
    error <- NULL

    # disable cruise choose if all cruise is selected
    observeEvent(input$cruise_all, {
        if (input$cruise_all) {
            updateSelectInput(session, "cruises", label = NULL, choices = character(0), selected = NULL)
        } else {
            updateSelectInput(session, "cruises", label = "Select cruises", status$cruises)
        }
    })

    # disable dive choose if all dives is selected
    observeEvent(input$dive_all, {
        if (input$dive_all) {
            updateSelectInput(session, "dives", label = NULL, choices = character(0), selected = NULL)
        } else {
            if (input$cruise_all) {
                dives_4choose <- status$dives
            } else {
                dives_4choose <- status$cruise_dive.tbl %>%
                    filter(cruise %in% input$cruises) %>%
                    pull(dive) %>%
                    unique()
            }
            updateSelectInput(session, "dives", label = "Select dives", dives_4choose)
        }
    })

    # Check if cruise and dive filter match
    dive_cruise_conifg <- reactive({
        if (input$cruise_all) {
            user.cruises <- status$cruises
        } else {
            user.cruises <- input$cruises 
        }

        if (input$dive_all) {
            user.dives <- status$cruise_dive.tbl %>%
                filter(cruise %in% user.cruises) %>%
                pull(dive) %>%
                unique()
        } else {
            user.dives <- input$dives
        }

        list(
            error = error,
            user.cruises = user.cruises,
            user.dives = user.dives
        )
    })

    # Show error if any
    output$dive_cruise_compCheck <- renderText({
        errs <- dive_cruise_conifg()$error
        if (length(errs) == 0) {
            return(NULL)
        } else {
            return(paste("Error:", paste(unlist(errs), collapse = "\n"), sep = "\n"))
        }
    })

    # Set config options
    search_option <- reactive({
        list(
            exact = input$exact,
            threshold = input$threshold,
            expansion = !input$expansion,
            cruise.range = dive_cruise_conifg()$user.cruises,
            dive.range = dive_cruise_conifg()$user.dives,
            case_sensitive = input$case_sensitive
        )
    })

    # Do search
    search_res <- eventReactive(input$search, {
        withProgress(message = 'Searching sealogs...', value = 0, {
            start.time <- Sys.time()
            incProgress(0.3, detail = "Scanning text data")
            
            if (length(dive_cruise_conifg()$error) == 0) {
                if (length(dive_cruise_conifg()$error) == 0) {
                    searchText <- input$searchText
                    user.option <- search_option()
                    search_res.obj <- searchSealog(
                        all_sealog_data, 
                        status,
                        searchText,
                        ambiguity.threshold = user.option$threshold,
                        searchRange.cruise = user.option$cruise.range,
                        searchRange.dive = user.option$dive.range,
                        expansion = user.option$expansion,
                        exact = user.option$exact,
                        case_sensitive = user.option$case_sensitive
                    ) 
                    if (search_res.obj$status == "error") {
                        search_res.report.in <- list(
                            status = "error",
                            message = search_res.obj$content,
                            searchText = searchText,
                            result_raw = NA,
                            result_display = NA,
                            runtime_s = NA
                        )
                    }
                    if (search_res.obj$status == "no_result") {
                        search_res.report.in <- list(
                            status = "no_result",
                            message = search_res.obj$content,
                            searchText = searchText,
                            result_raw = NA,
                            result_display = NA,
                            runtime_s = NA
                        )
                    }
                    if (search_res.obj$status == "exit") {
                        search_res.tbl <- search_res.obj$content
                        search_res.tbl.url <- search_res.tbl %>% 
                            searchres_youtube(youtube_meta)
                        search_res.tbl.display <- searchres_formatter(search_res.tbl.url)
                        end.time <- Sys.time()
                        runtime <- round(as.numeric(end.time-start.time), 2)
                        updateActionButton(session, "search")

                        search_res.report.in <- list(
                            status = search_res.obj$status,
                            searchText = searchText,
                            result_raw = search_res.tbl,
                            result_display = search_res.tbl.display,
                            runtime_s = runtime
                        )
                    }
                        incProgress(0.9, detail = "Formatting results")
                    return(search_res.report.in)
                }
            }
        })
    })

    output$results_area <- renderUI({
        if (input$search == 0) {
            return(tagList(
                h3("Welcome to Sealog Search!", style = "color: #2c3e50; font-weight: bold;"),
                p("Select your cruises and dives on the left, enter a keyword, and hit Search."),
                div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 5px solid #0275d8;",
                    p(strong("Tip:"), "Set ambiguous search threshold to 0.1 or use full-word exact match when searching for short keyword (e.g. 'vent') to reduce unrelevant results.")
                )
            ))
        }
        
        # check if result available
        res <- search_res()
        
        if (res$status == "exit") {
            return(DTOutput("display")) 
        } else if (res$status == "no_result") {
            return(div(
                class = "alert alert-warning", 
                role = "alert",
                h4("No results found"),
                p(paste0("We couldn't find matches for '", input$searchText, "'. Try adjusting the ambiguity threshold."))
            ))
        } else {
            return(div(class = "alert alert-danger", paste("Error:", res$message)))
        }
    })

    # display search result
    output$display <- renderDT({
        req(search_res())
        df <- search_res()$result_display

        if (search_res()$status == "exit") {
            df$`Replay url` <- ifelse(
              grepl("^https?://", df$`Replay url`),
              paste0("<a href='", df$`Replay url`, "' target='_blank'>", df$`Replay url`, "</a>"),
              df$`Replay url`
            )

            datatable(df, escape = FALSE)
        }
    })
    
    # prepare csv for download
    output$download_csv <- downloadHandler(
        filename = function() {
            paste0("sealog_searchresult_", format(now("UTC"), "%Y%m%d%H%M"), ".csv")
        },
        content = function(file) {
            write.table(search_res()$result_raw, file, sep = ";", row.names = F, fileEncoding = "UTF-8")
        }
    )

    # show summary text
    output$summary <- renderUI({
        if (search_res()$status != "exit") {
            summary <- "No result."
        } else {
            summary <- paste0("<br>\nSearch took ", search_res()$runtime_s, " seconds and found ", nrow(search_res()$result_raw), " results.")
        }
        HTML(markdownToHTML(text = summary, fragment.only = TRUE))
    })
  
    # static info display
    output$info <- renderUI({
        info <- "For any question/bugs please contact: [hu_zhehao@hotmail.com](mailto:hu_zhehao@hotmail.com)\n\nSource code available at github: [zzzhehao/sealogsearch](https://github.com/zzzhehao/sealogsearch)"            
        HTML(markdownToHTML(text = info, fragment.only = TRUE))
    })
}

shinyApp(ui = ui, server = server)
