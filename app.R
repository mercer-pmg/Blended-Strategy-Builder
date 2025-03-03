
library(rhandsontable)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(rmarkdown)
library(gt)
library(gtExtras)
library(webshot2)
library(ggplot2)
library(pagedown)
library(aws.s3)

source("./R/selectInput_sleeve.R")
source("./R/numericInputIcon_weight.R")
source("./R/numericInputIcon_value.R")
source("./R/selectInput_existingSleeves.R")
source("./R/numericInput_totalValue.R")
source("./R/numericInput_advisoryFee.R")
source("./R/alert_valueBelowMin.R")
source("./R/alert_weightMismatch.R")
source("./R/alert_valueBelowMinMessage.R")
source("./R/update_value.R")
source("./R/sync_inputs.R")
source("./R/inputFields.R")
source("./R/inputHeadings.R")
source("./R/expected_return.R")
source("./R/expected_risk.R")
source("./R/rank_sleeves.R")
source("./R/donut_asset_classes.R")
source("./R/blended_strategy_count.R")
source("./R/update_blended_strategy_record.R")


# platform   <- readr::read_csv(file = "Orion Platform.csv", show_col_types = FALSE) 

platform <- aws.s3::get_object(
     region = Sys.getenv("AWS_DEFAULT_REGION"),
     key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
     secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
     
     object = "orion-platform.csv", 
     bucket = "aspen-investing-menu") |>
     readBin("character") |>
     readr::read_csv(show_col_types = FALSE)

sleeve_ids <- platform |> 
     dplyr::select(model_agg, model_agg_id) |> 
     dplyr::distinct() |>
     rank_sleeves() |>
     dplyr::arrange(rank)

asset_class <- readr::read_csv(
     file          = "Model Agg - Asset Class Exposures.csv", 
     show_col_type = FALSE) |>
     dplyr::mutate(asset_class = asset_class |> stringr::str_replace_all("\xa0", " ")) |>
     dplyr::left_join(readr::read_csv("cma_data_rank.csv"), by = "asset_class") |>
     dplyr::arrange(rank)

sleeve_metrics <- readr::read_csv(file = "Model Aggregate Metrics.csv", show_col_types = FALSE)

ui <- fluidPage(
     
     shinyjs::useShinyjs(),
     shinyjs::extendShinyjs(script = "myfunctions.js", functions = c("backgroundCol")),
     
     shinyFeedback::useShinyFeedback(),
     
     titlePanel(title = span(h1("Aspen Investing: Blended Strategy Builder"), 
                             style = "color:#c00686; font-family:Merriweather"),
                windowTitle = "Blended Strategy Builder | Aspen Investing"),
     
     # Input column
     column(7,
            fluidRow(
                 column(6,selectInput_existingSleeves(platform)),
                 column(2, numericInput_totalValue()),
                 column(2, numericInput_advisoryFee())
            ),
            
            # Input table headings
            fluidRow(
                 column(6, h4("Sleeve")),
                 column(2, h4("Weight")),
                 column(2, h4("Value")) 
            ),
            
            # Input row 1
            fluidRow(
                 column(6, selectInput_sleeve("sleeve1", df = platform)),
                 column(2, numericInputIcon_weight("weight1")),
                 column(2, numericInputIcon_value("value1")),
                 column(2, span(textOutput("message1"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
            ),
            
            
            # Input row 2
            fluidRow(
                 column(6, selectInput_sleeve("sleeve2", df = platform)),
                 column(2, numericInputIcon_weight("weight2")),
                 column(2, numericInputIcon_value("value2")),
                 column(2, span(textOutput("message2"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 3
            fluidRow(
                 column(6, selectInput_sleeve("sleeve3", df = platform)),
                 column(2, numericInputIcon_weight("weight3")),
                 column(2, numericInputIcon_value("value3")),
                 column(2, span(textOutput("message3"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 4
            fluidRow(
                 column(6, selectInput_sleeve("sleeve4", df = platform)),
                 column(2, numericInputIcon_weight("weight4")),
                 column(2, numericInputIcon_value("value4")),
                 column(2, span(textOutput("message4"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 5
            fluidRow(
                 column(6, selectInput_sleeve("sleeve5", df = platform)),
                 column(2, numericInputIcon_weight("weight5")),
                 column(2, numericInputIcon_value("value5")),
                 column(2, span(textOutput("message5"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 6
            fluidRow(
                 column(6, selectInput_sleeve("sleeve6", df = platform)),
                 column(2, numericInputIcon_weight("weight6")),
                 column(2, numericInputIcon_value("value6")),
                 column(2, span(textOutput("message6"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 7
            fluidRow(
                 column(6, selectInput_sleeve("sleeve7", df = platform)),
                 column(2, numericInputIcon_weight("weight7")),
                 column(2, numericInputIcon_value("value7")),
                 column(2, span(textOutput("message7"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 8
            fluidRow(
                 column(6, selectInput_sleeve("sleeve8", df = platform)),
                 column(2, numericInputIcon_weight("weight8")),
                 column(2, numericInputIcon_value("value8")),
                 column(2, span(textOutput("message8"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 9
            fluidRow(
                 column(6, selectInput_sleeve("sleeve9", df = platform)),
                 column(2, numericInputIcon_weight("weight9")),
                 column(2, numericInputIcon_value("value9")),
                 column(2, span(textOutput("message9"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            # Input row 10
            fluidRow(
                 column(6, selectInput_sleeve("sleeve10", df = platform)),
                 column(2, numericInputIcon_weight("weight10")),
                 column(2, numericInputIcon_value("value10")),
                 column(2, span(textOutput("message10"), style = "color: red; font-size: 20px; font-family: IBM Plex Sans"))
                 
            ),
            
            fluidRow(
                 column(6),
                 column(2,
                        actionButton(
                             "update",
                             label = "Update weights",
                             width = "100%",
                             style = "background-color:#CFCCFF; font-family:IBM Plex Sans; border-color:#7673DC")),
                 column(2,
                        actionButton(
                             "submit",
                             label = "Submit",
                             icon  = icon("paper-plane"),
                             width = "100%",
                             style = "background-color:#CFCCFF; font-family:IBM Plex Sans; border-color:#7673DC")
                 )
            )
            
     ),
     
     # Output column
     # column(3,
     #        textOutput("text"),
     #        
     #        span(textOutput("header1"), style = "font-family: Merriweather; font-size: 20px; color: #7673DC"),
     #        tableOutput("table1"),
     #        
     #        span(textOutput("header2"), style = "font-family: Merriweather; font-size: 20px; color: #7673DC"),
     #        tableOutput("table2")),
     # 
     # column(2,
     #        textOutput("matches"),
     #        br(),
     #        uiOutput("download"))
     
     column(5,
            span(textOutput("text"), style = "font-family: IBM Plex Sans; font-size: 20px"),
            span(textOutput("header2"), style = "font-family: Merriweather; font-size: 20px; color: #7673DC"),
            # plotOutput("donut", width = "90%", height = 400),
            plotOutput("donut", width = "90%"),
            layout_columns(
                 col_widths = c(6,6),
                 
                 # Expected return card
                 card(
                      card_header(
                           span(textOutput("header3"),
                                style= "font-family: Merriweather; font-size: 20px; color: #7673DC; text-align: left"
                           )
                      ),
                      card_body(
                           span(
                                textOutput("expected_return"),
                                style= "font-family: IBM Plex Sans; font-size: 50px; text-align: left")
                      )
                 ),
                 
                 # Expected risk card
                 card(
                      card_header(
                           span(textOutput("header4"),
                                style= "font-family: Merriweather; font-size: 20px; color: #7673DC; text-align: left"
                           )
                      ),
                      card_body(
                           span(
                                textOutput("expected_risk"),
                                style= "font-family: IBM Plex Sans; font-size: 50px; text-align: left")
                      )
                 ),
                 
                 # Weighted expense ratio
                 card(
                      card_header(
                           span(textOutput("header6"),
                                style= "font-family: Merriweather; font-size: 20px; color: #7673DC; text-align: left"
                           )
                      ),
                      card_body(
                           span(
                                textOutput("expense_ratio"),
                                style= "font-family: IBM Plex Sans; font-size: 50px; text-align: left"
                           )
                      )
                 ),
                 
                 # Weighted yield
                 card(
                      card_header(
                           span(textOutput("header7"),
                                style= "font-family: Merriweather; font-size: 20px; color: #7673DC; text-align: left"
                           )
                      ),
                      card_body(
                           span(
                                textOutput("yield"),
                                style= "font-family: IBM Plex Sans; font-size: 50px; text-align: left")
                      )
                 )
            ),
            br(),
            br(),
            
            
            # Holdings
            span(textOutput("header8"), style= "font-family: Merriweather; font-size: 20px; color: #7673DC; text-align: left"),
            gt_output("holdings"),
            
            br(),
            br(),
            
            span(textOutput("header5"), style = "font-family: Merriweather; font-size: 20px; color: #7673DC"),
            span(textOutput("matches"), style = "font-family: IBM Plex Sans; font-size: 20px"),
            
            
            
            uiOutput("download"),
            br(),
            span(textOutput("report_text"), style = "font-family: IBM Plex Sans; font-size: 20px"),
            uiOutput("download2"),
            
            br(),
            br()
            
     )
)





server <- function(input, output) {
     
     strategy_current <- reactive({
          
          tibble::tibble(
               sleeve      = 1:10 |> purrr::map_chr(function(i) input[[paste0("sleeve",i)]]),
               weight      = 1:10 |> purrr::map_dbl(function(i) input[[paste0("weight", i)]]),
               value       = 1:10 |> purrr::map_dbl(function(i) input[[paste0("value", i)]]),
               total_value = input[["total_value"]],
               imp_weight  = value/total_value*100,
               imp_value   = weight*total_value/100,
               advisory_fee         = input[["advisory_fee"]]/100
          )|>
               dplyr::filter(weight != 0 | value != 0) |> 
               dplyr::left_join(sleeve_metrics, dplyr::join_by(sleeve == model_agg)) |>
               print()
     })
     
     output$message1  <- renderText(alert_valueBelowMinMessage(1,  df = strategy_current()))
     output$message2  <- renderText(alert_valueBelowMinMessage(2,  df = strategy_current()))
     output$message3  <- renderText(alert_valueBelowMinMessage(3,  df = strategy_current()))
     output$message4  <- renderText(alert_valueBelowMinMessage(4,  df = strategy_current()))
     output$message5  <- renderText(alert_valueBelowMinMessage(5,  df = strategy_current()))
     output$message6  <- renderText(alert_valueBelowMinMessage(6,  df = strategy_current()))
     output$message7  <- renderText(alert_valueBelowMinMessage(7,  df = strategy_current()))
     output$message8  <- renderText(alert_valueBelowMinMessage(8,  df = strategy_current()))
     output$message9  <- renderText(alert_valueBelowMinMessage(9,  df = strategy_current()))
     output$message10 <- renderText(alert_valueBelowMinMessage(10, df = strategy_current()))
     
     
     # Update weight inputs when existing strategy selected
     existingStrategyAllo <- eventReactive(
          input$existingStrategy,{
               platform |>
                    dplyr::filter(strategy == input$existingStrategy) |>
                    dplyr::select(model_agg, agg_target) |>
                    dplyr::distinct() |>
                    dplyr::left_join(sleeve_ids, by = "model_agg") |>
                    dplyr::arrange(rank) |>
                    dplyr::select(model_agg, agg_target)
          }
     )
     
     observeEvent(input$existingStrategy,{
          
          for(i in 1:10){
               
               updateNumericInput(
                    inputId = paste0("weight", i),
                    value   = 0)
               
               updateNumericInput(
                    inputId = paste0("value", i),
                    value = 0)
               
               updateSelectInput(
                    inputId = paste0("sleeve", i),
                    selected = "")
               
          }
          
          k <- nrow(existingStrategyAllo())
          
          for(i in 1:k){
               
               updateSelectInput(
                    inputId = paste0("sleeve", i),
                    selected = existingStrategyAllo() |>
                         dplyr::pull(model_agg) |>
                         dplyr::nth(i))
               
               foo <- existingStrategyAllo() |>
                    dplyr::pull(agg_target) |>
                    dplyr::nth(i)
               
               updateNumericInput(
                    inputId = paste0("weight", i),
                    value   = foo)
               
               # for(i in (k+1):10){
               #      updateSelectInput(
               #           inputId = paste0("sleeve", i),
               #           selected = "")
               #      
               #      updateNumericInput(
               #           inputId = paste0("weight",i),
               #           value   = 0)
               # }
          }
     })
     
     ## Alerts
     
     # Update value input background color if value < sleeve minimum
     observeEvent(input$value1,   alert_valueBelowMin(1,  df = strategy_current()))
     observeEvent(input$value2,   alert_valueBelowMin(2,  df = strategy_current()))
     observeEvent(input$value3,   alert_valueBelowMin(3,  df = strategy_current()))
     observeEvent(input$value4,   alert_valueBelowMin(4,  df = strategy_current()))
     observeEvent(input$value5,   alert_valueBelowMin(5,  df = strategy_current()))
     observeEvent(input$value6,   alert_valueBelowMin(6,  df = strategy_current()))
     observeEvent(input$value7,   alert_valueBelowMin(7,  df = strategy_current()))
     observeEvent(input$value8,   alert_valueBelowMin(8,  df = strategy_current()))
     observeEvent(input$value9,   alert_valueBelowMin(9,  df = strategy_current()))
     observeEvent(input$value10,  alert_valueBelowMin(10, df = strategy_current()))
     
     # Update weight input background color if differs from implied
     observeEvent(input$value1,   alert_weightMismatch(1,  df = strategy_current()))
     observeEvent(input$value2,   alert_weightMismatch(2,  df = strategy_current()))
     observeEvent(input$value3,   alert_weightMismatch(3,  df = strategy_current()))
     observeEvent(input$value4,   alert_weightMismatch(4,  df = strategy_current()))
     observeEvent(input$value5,   alert_weightMismatch(5,  df = strategy_current()))
     observeEvent(input$value6,   alert_weightMismatch(6,  df = strategy_current()))
     observeEvent(input$value7,   alert_weightMismatch(7,  df = strategy_current()))
     observeEvent(input$value8,   alert_weightMismatch(8,  df = strategy_current()))
     observeEvent(input$value9,   alert_weightMismatch(9,  df = strategy_current()))
     observeEvent(input$value10,  alert_weightMismatch(10, df = strategy_current()))
     
     
     ## Reactions
     
     # Update weights on action button
     observeEvent(input$update, update_weights(df = strategy_current()))
     
     # Update values when total value changes
     observeEvent(input$total_value, {
          for(i in 1:10){
               
               req(strategy_current()$weight[i] > 0)
               
               updateNumericInput(
                    inputId = paste0("value", i),
                    value   = input$total_value*strategy_current()$weight[i]/100
               )
          }
     })
     
     # Update value when weight changes
     observeEvent(input$weight1,  update_value(id = "value1",  weight = input$weight1,  total = input$total_value))
     observeEvent(input$weight2,  update_value(id = "value2",  weight = input$weight2,  total = input$total_value))
     observeEvent(input$weight3,  update_value(id = "value3",  weight = input$weight3,  total = input$total_value))
     observeEvent(input$weight4,  update_value(id = "value4",  weight = input$weight4,  total = input$total_value))
     observeEvent(input$weight5,  update_value(id = "value5",  weight = input$weight5,  total = input$total_value))
     observeEvent(input$weight6,  update_value(id = "value6",  weight = input$weight6,  total = input$total_value))
     observeEvent(input$weight7,  update_value(id = "value7",  weight = input$weight7,  total = input$total_value))
     observeEvent(input$weight8,  update_value(id = "value8",  weight = input$weight8,  total = input$total_value))
     observeEvent(input$weight9,  update_value(id = "value9",  weight = input$weight9,  total = input$total_value))
     observeEvent(input$weight10, update_value(id = "value10", weight = input$weight10, total = input$total_value))
     
     
     
     ## After strategy is submitted
     
     # Time of strategy submission
     submit_timestamp <- reactive({format(Sys.time(), format = "%Y%m%d%H%M%S")}) |>
          bindEvent(input$submit)
     
     # Total allocated of strategy submitted
     weightSum <- eventReactive(input$submit, {
          strategy_current() |> dplyr::pull(weight) |> sum()
     })
     
     # Message if strategy submitted is over/under allocated
     output$text <- renderText({
          
          if (isTRUE(base::all.equal(weightSum(), 100))){
               msg <- ""} else {
                    if(weightSum() > 100){
                         msg <- paste0("Sum of sleeve weights is ",
                                       weightSum(),
                                       "%. Reduce total weight to 100%.")} else {
                                            if(weightSum() < 100){
                                                 msg <- paste0("Sum of sleeve weights is ",
                                                               weightSum(),
                                                               "%. Increase total weight to 100%.")}
                                       }
               }
          
          msg
          
     })
     
     # Sleeve table output
     table_headers <- reactive({
          
          if(weightSum() == 100){
               headers <- list(
                    header1 = "Sleeve Allocations",
                    header2 = "Asset Class Allocations",
                    header3 = "Expected Return (Gross)",
                    header4 = "Expected Risk",
                    header5 = "Downloads",
                    header6 = "Weighted Expense Ratio",
                    header7 = "Weighted Yield",
                    header8 = "Holdings")
          } else{
               headers <- NULL
          }
     }) |> bindEvent(input$submit)
     
     output$header1 <- renderText(table_headers()$header1)
     
     sleeve_table1 <- eventReactive(input$submit, {
          
          x <- strategy_current()
          total_weight <- x$weight |> sum()
          
          if(total_weight == 100){
               tbl <- strategy_current() |>
                    dplyr::group_by(sleeve) |>
                    dplyr::summarise(weight = sum(weight))
          } else {
               tbl <- NULL
          }
          
          tbl
     })
     
     output$table1 <- renderTable(sleeve_table1(), colnames = FALSE)
     
     # Asset class table output
     output$header2 <- renderText(table_headers()$header2)
     
     sleeve_table2 <- eventReactive(input$submit, {
          
          x <- strategy_current() |>
               dplyr::mutate(sleeve_weight = weight/100) |>
               dplyr::select(sleeve, sleeve_weight)
          
          y <- asset_class |> dplyr::filter(model_agg %in% x$sleeve)
          
          x <- dplyr::left_join(y, x, by = dplyr::join_by(model_agg == sleeve)) |>
               dplyr::mutate(weight = weight * sleeve_weight)
          
          # total_weight <- x$weight |> sum()
          # 
          #  if(total_weight == 100){
          #      print("equals 100")
          #      tbl <- x |>
          #           dplyr::group_by(asset_class) |>
          #           dplyr::summarise(weight = sum(weight))
          # } else {
          #      tbl <- NULL
          # }
          
          tbl <- x |>
               dplyr::group_by(asset_class) |>
               dplyr::summarise(weight = sum(weight))
          
          # print(tbl)
          # tbl
     })
     
     output$table2 <- renderTable(sleeve_table2(), colnames = FALSE)
     
     
     # Check for matching strategy message
     existing <- eventReactive(input$submit, {
          
          if(isTRUE(all.equal(weightSum(), 100))){
               
               n <- nrow(strategy_current())
               
               matches <- platform |> dplyr::select(strategy)
               
               for (i in 1:n){
                    
                    foo <- platform |>
                         dplyr::filter(model_agg == strategy_current()$sleeve[i] & agg_target == strategy_current()$weight[i]) |>
                         dplyr::pull(strategy)
                    
                    matches <- matches |> dplyr::filter(strategy %in% foo)}
               
               match <- matches |> dplyr::distinct() |> dplyr::pull(strategy)
               
               exists <- length(match) > 0
               
               if(length(match) == 0){strategy_name <- "Blended Strategy"}
               
               if(length(match) == 1){
                    string_match  <- match
                    strategy_name <- match}
               
               if(length(match) > 1){
                    last <- match[length(match)]
                    rest <- match[1:(length(match)-1)]
                    
                    string_match <- paste(rest, collapse = ", ")
                    string_match <- paste(string_match, last, sep = ", and ")
                    
                    strategy_name <- match[1]
               }
               
               if(length(match) == 0){
                    msg <- "This allocation does not exist. Download to create a blended strategy."}
               
               if(length(match > 0)){
                    msg <- paste0("This allocation already exists as ", string_match, ".")}
               
               
               
               list(result = as.logical(exists), message = msg, match = strategy_name)
          }
     })
     
     report_text <- eventReactive(input$submit, {
          if(isTRUE(all.equal(weightSum(), 100))){
               "Download strategy report."
          }
     })
     
     output$report_text <- renderText({report_text()})
     
     output$matches <- renderText({existing()$message})
     
     # Give option to download model if it does not already exist
     output$download <- renderUI({
          if(weightSum() == 100){
               if(!existing()$result){
                    downloadButton("downloadData", "Download New Strategy")
               }
          }
     })
     
     # Donut plot of asset class allocation
     p <-  reactive({donut_asset_classes(sleeve_table2())})
     
     output$donut <- renderPlot({if(weightSum() == 100){p()} else {}})
     
     
     # Submitted strategy expected return
     output$header3 <- renderText(table_headers()$header3)
     
     exp_return <- eventReactive(input$submit,{
          if(weightSum() == 100){
               
               sleeve_table2() |>
                    dplyr::mutate(
                         Asset = asset_class |> stringr::str_replace_all("\xa0", " "),
                         Weight = as.numeric(weight)) |>
                    dplyr::select(Asset, Weight) |>
                    expected_return()
          } else {
               
          }
     })
     
     output$expected_return <- renderText({
          
          if(weightSum() == 100){
               
               print(sleeve_table2())
               
               sleeve_table2() |>
                    dplyr::mutate(
                         Asset = asset_class |> stringr::str_replace_all("\xa0", " "),
                         Weight = as.numeric(weight)) |>
                    dplyr::select(Asset, Weight) |>
                    expected_return()
          } else {
               
          }
     })
     
     # Submitted strategy expected risk
     output$header4 <- renderText(table_headers()$header4)
     
     exp_risk <- eventReactive(input$submit, {
          if(weightSum() == 100){
               sleeve_table2() |>
                    dplyr::mutate(
                         Asset = asset_class,
                         Weight = as.numeric(weight)) |>
                    expected_risk()
          } else {
               
          }
     })
     
     output$expected_risk <- renderText({
          
          if(weightSum() == 100){
               sleeve_table2() |>
                    dplyr::mutate(
                         Asset = asset_class,
                         Weight = as.numeric(weight)) |>
                    expected_risk()
          } else {
               
          }
     })
     
     # Submitted strategy weighted expense ratio
     expense_ratio <- eventReactive(input$submit, {
          if(weightSum() == 100){
               strategy_current() |>
                    dplyr::select(sleeve, weight, fee) |>
                    dplyr::mutate(
                         weight = weight/100,
                         contribution = fee*weight/100) |>
                    dplyr::pull(contribution) |>
                    sum() |>
                    scales::percent(accuracy = 0.01)
          }
     })
     
     advisory_fee <- reactive({input$advisory_fee}) |> shiny::bindEvent(input$submit)
     
     output$expense_ratio <- renderText({expense_ratio()})
     
     
     # Submitted strategy weighted yield
     weighted_yield <- eventReactive(input$submit, {
          if(weightSum() == 100){
               strategy_current() |>
                    dplyr::select(sleeve, weight, yield) |>
                    dplyr::mutate(
                         yield = yield/100,
                         contribution = yield*weight/100) |>
                    dplyr::pull(contribution) |>
                    sum() |>
                    scales::percent(accuracy = 0.01)
          }
     })
     
     output$yield <- renderText({weighted_yield()})
     
     
     output$header6 <- renderText({table_headers()$header6})
     
     output$header7 <- renderText({table_headers()$header7})
     
     output$header5 <- renderText({table_headers()$header5})
     
     output$header8 <- renderText({table_headers()$header8})
     strategy_holdings <- eventReactive(input$submit, {
          
          if(weightSum() == 100){
               
               holdings <- platform |> 
                    dplyr::select(model_agg, product, ticker, target) |>
                    dplyr::distinct() |>
                    dplyr::filter(model_agg %in% strategy_current()$sleeve)
               
               holdings <- strategy_current() |>
                    dplyr::left_join(holdings, dplyr::join_by(sleeve == model_agg)) |>
                    dplyr::select(sleeve, product, ticker, target, weight) |>
                    dplyr::mutate(
                         model_agg = sleeve,
                         weight = weight*target/100,
                         security = paste0(product," (", ticker,")")) |>
                    rank_sleeves() |>
                    dplyr::group_by(sleeve) |>
                    dplyr::arrange(rank,dplyr::desc(weight)) |>
                    dplyr::select(sleeve, security, weight) |>
                    dplyr::mutate(weight = weight |> scales::percent(scale = 1, accuracy = 0.01)) |>
                    dplyr::rename_all(stringr::str_to_title)|>
                    gt::gt(rowname_col = "Security") |>
                    # gt::summary_rows(side = "top") |>
                    gt::tab_style(
                         style = gt::cell_fill(color = "#454759"),
                         locations = gt::cells_stubhead()
                    ) |>
                    gt::tab_style(
                         style = list(
                              gt::cell_fill(color = "#454759"),
                              gt::cell_text(color = "white", align = "center", weight = "bold")),
                         locations = gt::cells_column_labels()) |>
                    gt::tab_style(
                         style = gt::cell_text(align = "center"),
                         locations = gt::cells_body()
                    ) |>
                    gt::tab_style(
                         style = list(
                              gt::cell_fill(color = "#F3F4F6"),
                              gt::cell_text(weight = "bold")),
                         locations = gt::cells_row_groups()) |>
                    gt::tab_options(
                         table.width = "100%",
                         table.font.size = 14,
                         table.align = "left",
                         row_group.padding = gt::px(7),
                         data_row.padding = gt::px(5)
                    ) 
               
               
          }
          
          
     })
     
     output$holdings <- render_gt(strategy_holdings())
     
     output$downloadData <- downloadHandler(
          filename = function(){paste0(next_blended(),".csv")},
          content  = function(file){
               readr::write_csv(
                    x = strategy_current() |>
                         dplyr::left_join(sleeve_ids, by = dplyr::join_by(sleeve==model_agg)) |>
                         
                         dplyr::bind_rows(tibble::tibble(
                              sleeve = c("Income Sweep", "Unsupervised", "UnAssigned", "Transitional"),
                              weight = rep(0,4),
                              model_agg_id = c(69140, 69269, 69277, 69278))) |>
                         
                         dplyr::mutate(
                              `Sleeve Strategy Id`              = "<new>",
                              Name                              = next_blended(),
                              `Contribution Allocation Method`  = "Most Out Of Balance",
                              `Distribution Allocation Method`  = "Most Out Of Balance",
                              `Auto Rebal Frequency`            = "None",
                              `Auto Rebal Month`                = "",
                              `Auto Rebal Day`                  = "",
                              `Entity Id`                       = "",
                              Entity                            = "",
                              `Tolerance Percent`               = 10,
                              `Strategy Detail Id`              = "<new>",
                              `Sleeve Type`                     = "Normal",
                              `Modelagg`                        = sleeve,
                              `Modelagg Id`                     = model_agg_id,
                              `Excluded Rebal Sleeve`           = dplyr::if_else(
                                   condition = sleeve %in% c("Income Sweep", "Unsupervised", "UnAssigned", "Transitional"),
                                   true      = TRUE,
                                   false     = FALSE),
                              `Management Style`                = "",
                              `Subadvisor Id`                   = "",
                              `Target Allocation`               = weight,
                              `Contribution Allocation`         = "",
                              `Distribution Allocation`         = "",
                              `Tolerance Upper`                 = weight/`Tolerance Percent`,
                              `Tolerance Lower`                 = weight/`Tolerance Percent`,
                              `Auto Trade Types`                = "",
                              `Substitute Detail Id`            = "",
                              `Substitute Id`                   = "",
                              `Asset Level Allowed`             = "",
                              `Allowed Asset Classification ID` = "") |>
                         
                         dplyr::select(
                              `Sleeve Strategy Id`,
                              Name,
                              `Contribution Allocation Method`,
                              `Distribution Allocation Method`,
                              `Auto Rebal Frequency`,
                              `Auto Rebal Month`,
                              `Auto Rebal Day`,
                              `Entity Id`,
                              Entity,
                              `Tolerance Percent`,
                              `Strategy Detail Id`,
                              `Sleeve Type`,
                              `Modelagg`,
                              `Modelagg Id`,
                              `Excluded Rebal Sleeve`,
                              `Management Style`,
                              `Subadvisor Id`,
                              `Target Allocation`,
                              `Contribution Allocation`,
                              `Distribution Allocation`,
                              `Tolerance Upper`,
                              `Tolerance Lower`,
                              `Auto Trade Types`,
                              `Substitute Detail Id`,
                              `Substitute Id`,
                              `Asset Level Allowed`,
                              `Allowed Asset Classification ID`),
                    file)
               
               update_s3()
               
               })
     
     
     
     output$downloadReport <- downloadHandler(
          filename = "Strategy Report.html",
          content = function(file) {
               
               print(exp_return())
               print(strategy_current() |> dplyr::pull(advisory_fee) |> unique())
               print(expense_ratio())
               
               # Copy the report file to a temporary directory before processing it, in
               # case we don't have write permissions to the current working dir (which
               # can happen when deployed).
               
               print(existing()$match)
               
               tempReport <- file.path(tempdir(), "Report.Rmd")
               file.copy("Report2.Rmd", tempReport, overwrite = TRUE)
               
               # Set up parameters to pass to Rmd document
               params <- list(allocation     = strategy_current() |> dplyr::select(sleeve, weight, value),
                              total_value    = strategy_current() |> dplyr::pull(total_value) |> unique() |> scales::dollar(),
                              fee            = strategy_current() |> dplyr::pull(advisory_fee) |> unique() |> scales::percent(accuracy = 0.01),
                              strategy_match = existing()$match,
                              plot           = p(),
                              holdings       = strategy_holdings(),
                              return         = exp_return(),
                              risk           = exp_risk(),
                              yield          = weighted_yield(),
                              expense        = expense_ratio())
               
               # Knit the document, passing in the `params` list, and eval it in a
               # child of the global environment (this isolates the code in the document
               # from the code in this app).
               rmarkdown::render(tempReport, 
                                 params = params,
                                 output_format = "all",
                                 output_file = file,
                                 envir = new.env(parent = globalenv())
               )
          }
     )
     
     output$download2 <- renderUI({
          if(weightSum() == 100){
               downloadButton("downloadReport", "Download Strategy Report")
               
          }
     })
     
     
}




# Run the application 
shinyApp(ui = ui, server = server)
