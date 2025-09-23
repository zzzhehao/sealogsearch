source("functions.R")
library(shiny)
library(bslib)
library(DT)
library(markdown)

## config

status <- readRDS("cache/status_report.RDS")

## UI
# ui <- fluidPage(
ui <- page_sidebar(
    card(
        # Display
        textOutput("dive_cruise_compCheck"),
        uiOutput("summary"),
        dataTableOutput("display")
    ),
    title = titlePanel("Sealog search"),
    sidebar = card(
        checkboxInput("cruise_all", "All cruises", T),
        selectInput("cruises", "Select cruises", character(0), selected = NULL, multiple = T),
        checkboxInput("dive_all", "All dives", T),
        selectInput("dives", "Select dives", character(0), selected = NULL, multiple = T),
        textInput("searchText", "Search keyword", ""),
        actionButton("search", "Search"),
        card(
            checkboxInput("exact", "Search only for full-word exact match", F),
            checkboxInput("expansion", "Don't search for potential indel-typo", F),
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
        if (length(dive_cruise_conifg()$error) == 0) {cat("")} else {
            dive_cruise_conifg()$error %>%
                unlist() %>%
                cat("Error: \n", ., sep = "\n")
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
    # search_res <- reactive({
        start.time <- Sys.time()
        if (length(dive_cruise_conifg()$error) == 0) {
            searchText <- input$searchText
            user.option <- search_option()
            search_res.tbl <- searchSealog(
                searchText,
                ambiguity.threshold = user.option$threshold,
                searchRange.cruise = user.option$cruise.range,
                searchRange.dive = user.option$dive.range,
                expansion = user.option$expansion,
                exact = user.option$exact,
                case_sensitive = user.option$case_sensitive
            ) 
            hasHit <- length(search_res.tbl)==0
            search_res.tbl.url <- search_res.tbl %>% 
                searchres_youtube("replay-meta.yml")
            search_res.tbl.display <- searchres_formatter(search_res.tbl.url)
            end.time <- Sys.time()
            runtime <- round(as.numeric(end.time-start.time), 2)
            updateActionButton(session, "search")

            list(
                hasHit = hasHit,
                searchText = searchText,
                result_raw = search_res.tbl,
                result_display = search_res.tbl.display,
                runtime_s = runtime
            )
        }
    })

    searchres.display <- reactive({search_res()$result_display})
    output$search_res_report <- reactive({search_res()})

    output$display <- renderDT({
        df <- searchres.display()

        df$`Replay url` <- ifelse(
          grepl("^https?://", df$`Replay url`),
          paste0("<a href='", df$`Replay url`, "' target='_blank'>", df$`Replay url`, "</a>"),
          df$`Replay url`
        )
        
        datatable(df, escape = FALSE)
    })
    
    output$download_csv <- downloadHandler(
        filename = function() {
            paste0("sealog_searchresult_", format(now("UTC"), "%Y%m%d%H%M"), ".csv")
        },
        content = function(file) {
            write.table(search_res()$result_raw, file, sep = ";", row.names = F, fileEncoding = "UTF-8")
        }
    )

    output$summary <- renderUI({
        # summary <- paste0("<br>\nSearch took ", search_res()$runtime_s, " seconds and found ", nrow(search_res()$result_raw), " results.")
        # if (input$searchText != "") {
            # HTML(markdownToHTML(text = summary, fragment.only = TRUE))
        # }
        HTML(markdownToHTML(as.character(class(search_res()$result_raw))))
    })
  
    output$info <- renderUI({
        info <- "For any question/bugs please contact: [zhehao.hu@hotmail.com](mailto:zhehao.hu@hotmail.com)"            
        HTML(markdownToHTML(text = info, fragment.only = TRUE))
    })
}

# testServer(server, {
#     session$setInputs(searchText = "isopod")
#     session$setInputs(cruise_all = T)
#     session$setInputs(dive_all = T)
#     session$setInputs(exact = T)
#     session$setInputs(expansion = F)
#     session$setInputs(case_sensitive = F)
#     session$setInputs(threshold = 0.2)
#     stopifnot(output$search_res_report$hasHit)
# })

shinyApp(ui = ui, server = server)
