
library(rhandsontable)
library(shiny)
library(shinyWidgets)
library(shinyjs)

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


platform   <- readr::read_csv(file = "Orion Platform.csv", show_col_types = FALSE)
sleeve_ids <- platform |> dplyr::select(model_agg, model_agg_id) |> dplyr::distinct()
asset_class <- readr::read_csv(file = "Model Agg - Asset Class Exposures.csv", show_col_type = FALSE)

ui <- page_sidebar(
     
     title = "Aspen Investing: Blended Strategy Builder",
     
     sidebar = sidebar(open = FALSE),
     
     card(
          full_screen = TRUE,
          card_header("Define a Strategy"),
          height = "1000px",
          
          layout_sidebar(
               fillable = FALSE,
               sidebar = sidebar(
                    title = "Controls",
                    position = "left",
                    width = 400,
                    
                    # Sidebar components
                    selectInput_existingSleeves(platform),
                    numericInput_totalValue(),
                    numericInput_advisoryFee(),
                    
               ),
               
               layout_columns(
                    fillable = FALSE,
                    fill = FALSE,
                    col_widths = c(4,4,4),
                    
                    h6("Sleeves"),
                    h6("Weights"),
                    h6("Value"),
                    
                    selectInput("sleeve1", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight1", value = 0, label = NULL),
                    numericInput("value1", value = 0, label = NULL),
                    
                    selectInput("sleeve2", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight2", value = 0, label = NULL),
                    numericInput("value2", value = 0, label = NULL),
                    
                    selectInput("sleeve3", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight3", value = 0, label = NULL),
                    numericInput("value3", value = 0, label = NULL),
                    
                    selectInput("sleeve4", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight4", value = 0, label = NULL),
                    numericInput("value4", value = 0, label = NULL),
                    
                    selectInput("sleeve5", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight5", value = 0, label = NULL),
                    numericInput("value5", value = 0, label = NULL),
                    
                    selectInput("sleeve6", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight6", value = 0, label = NULL),
                    numericInput("value6", value = 0, label = NULL),
                    
                    selectInput("sleeve7", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight7", value = 0, label = NULL),
                    numericInput("value7", value = 0, label = NULL),
                    
                    selectInput("sleeve8", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight8", value = 0, label = NULL),
                    numericInput("value8", value = 0, label = NULL),
                    
                    selectInput("sleeve9", choices = c("a", "b", "c"), label = NULL),
                    numericInput("weight9", value = 0, label = NULL),
                    numericInput("value9", value = 0, label = NULL),
               )
          )
     ),

card(),

card(),


br(),

# titlePanel(title = span(
#      h1("Aspen Investing: Blended Strategy Builder"), 
#           style = "color:#c00686; font-family:Merriweather")),
# 
# br(),
# 
# layout_columns(
#      card(
#           selectInput_existingSleeves(platform),
#           numericInput_totalValue(),
#           numericInput_advisoryFee()
#      ),
#      
#      card(
#           inputHeadings()
#      ),
# col_widths = c(3,9)),



shinyjs::useShinyjs(),
shinyjs::extendShinyjs(script = "myfunctions.js", functions = c("backgroundCol")),

shinyFeedback::useShinyFeedback(),

# titlePanel(title = span(h1("Aspen Investing: Blended Strategy Builder"), 
#                         style = "color:#c00686; font-family:Merriweather")),
# 
# fluidRow(
#      column(3,selectInput_existingSleeves(platform)),
#      column(5, numericInput_totalValue()),
#      column(1, numericInput_advisoryFee()),
#      # column(5, 
#      #        radioButtons(
#      #             inputId = "allo_type", 
#      #             label   = "Allocation method", 
#      #             choices = c("Weight", "Value")))
# ),
# 
# # Input fields
# fluidRow(inputHeadings()),
# 
# column(3,
#        span(textOutput("text"),
#             style = "font-family:IBM Plex Sans"),
#        span(tableOutput("table")),
#        style = "font-family:IBM Plex Sans"),
# 
# column(5,
#        span(tableOutput("matches"),
#             style="font-family:IBM Plex Sans"),
#        br(),
#        uiOutput("download")),
# 
# fluidRow(1:10 |> purrr::map(inputFields, df = platform)),
# 
# 
# br(),
# 
# fluidRow(
#      column(9,
#             column(6),
#             column(5,
#                    actionButton(
#                         "update",
#                         label = "Update weights",
#                         # icon  = icon("paper-plane"),
#                         width = "100%",
#                         style = "background-color:#CFCCFF; font-family:IBM Plex Sans; border-color:#7673DC")),
#             column(5,
#                    actionButton(
#                         "submit",
#                         label = "Submit",
#                         icon  = icon("paper-plane"),
#                         width = "100%",
#                         style = "background-color:#CFCCFF; font-family:IBM Plex Sans; border-color:#7673DC")
#             )
#      )
# ),

)





server <- function(input, output) {
     
     
     # strategy_current <- reactive({
     #      
     #      tibble::tibble(
     #           sleeve      = 1:10 |> purrr::map_chr(function(i) input[[paste0("sleeve",i)]]),
     #           weight      = 1:10 |> purrr::map_dbl(function(i) input[[paste0("weight", i)]]),
     #           value       = 1:10 |> purrr::map_dbl(function(i) input[[paste0("value", i)]]),
     #           total_value = input[["total_value"]],
     #           imp_weight  = value/total_value*100,
     #           imp_value   = weight*total_value/100, 
     #           minimum     = 10^5
     #           
     #      )|> dplyr::filter(weight != 0 | value != 0) |> print()
     #      
     #      
     # })
     # 
     # output$message1  <- renderText(alert_valueBelowMinMessage(1,  df = strategy_current()))
     # output$message5  <- renderText(alert_valueBelowMinMessage(5,  df = strategy_current()))
     # output$message3  <- renderText(alert_valueBelowMinMessage(3,  df = strategy_current()))
     # output$message4  <- renderText(alert_valueBelowMinMessage(4,  df = strategy_current()))
     # output$message5  <- renderText(alert_valueBelowMinMessage(5,  df = strategy_current()))
     # output$message6  <- renderText(alert_valueBelowMinMessage(6,  df = strategy_current()))
     # output$message7  <- renderText(alert_valueBelowMinMessage(7,  df = strategy_current()))
     # output$message8  <- renderText(alert_valueBelowMinMessage(8,  df = strategy_current()))
     # output$message9  <- renderText(alert_valueBelowMinMessage(9,  df = strategy_current()))
     # output$message10 <- renderText(alert_valueBelowMinMessage(10, df = strategy_current()))
     # 
     # # Update weight inputs when existing strategy selected
     # 
     # existingStrategyAllo <- eventReactive(
     #      input$existingStrategy,{
     #           platform |>
     #                dplyr::filter(strategy == input$existingStrategy) |>
     #                dplyr::select(model_agg, agg_target) |>
     #                dplyr::distinct()
     #      }
     # )
     # 
     # observeEvent(input$existingStrategy,{
     #      
     #      k <- nrow(existingStrategyAllo())
     #      
     #      for(i in 1:k){
     #           
     #           updateSelectInput(
     #                inputId = paste0("sleeve", i),
     #                selected = existingStrategyAllo() |>
     #                     dplyr::pull(model_agg) |>
     #                     dplyr::nth(i))
     #           
     #           foo <- existingStrategyAllo() |>
     #                dplyr::pull(agg_target) |>
     #                dplyr::nth(i)
     #           
     #           updateNumericInput(
     #                inputId = paste0("weight", i),
     #                value   = foo)
     #           
     #           for(i in (k+1):10){
     #                updateSelectInput(
     #                     inputId = paste0("sleeve", i),
     #                     selected = "")
     #                
     #                updateNumericInput(
     #                     inputId = paste0("weight",i),
     #                     value   = 0)
     #           }
     #      }
     # })
     # 
     # 
     # 
     # ## Alerts
     # 
     # # Update value input background color if value < sleeve minimum
     # observeEvent(input$value1,   alert_valueBelowMin(1,  df = strategy_current()))
     # observeEvent(input$value5,   alert_valueBelowMin(5,  df = strategy_current()))
     # observeEvent(input$value3,   alert_valueBelowMin(3,  df = strategy_current()))
     # observeEvent(input$value4,   alert_valueBelowMin(4,  df = strategy_current()))
     # observeEvent(input$value5,   alert_valueBelowMin(5,  df = strategy_current()))
     # observeEvent(input$value6,   alert_valueBelowMin(6,  df = strategy_current()))
     # observeEvent(input$value7,   alert_valueBelowMin(7,  df = strategy_current()))
     # observeEvent(input$value8,   alert_valueBelowMin(8,  df = strategy_current()))
     # observeEvent(input$value9,   alert_valueBelowMin(9,  df = strategy_current()))
     # observeEvent(input$value10,  alert_valueBelowMin(10, df = strategy_current()))
     # 
     # # Update weight input background color if differs from implied
     # observeEvent(input$value1,   alert_weightMismatch(1,  df = strategy_current()))
     # observeEvent(input$value5,   alert_weightMismatch(5,  df = strategy_current()))
     # observeEvent(input$value3,   alert_weightMismatch(3,  df = strategy_current()))
     # observeEvent(input$value4,   alert_weightMismatch(4,  df = strategy_current()))
     # observeEvent(input$value5,   alert_weightMismatch(5,  df = strategy_current()))
     # observeEvent(input$value6,   alert_weightMismatch(6,  df = strategy_current()))
     # observeEvent(input$value7,   alert_weightMismatch(7,  df = strategy_current()))
     # observeEvent(input$value8,   alert_weightMismatch(8,  df = strategy_current()))
     # observeEvent(input$value9,   alert_weightMismatch(9,  df = strategy_current()))
     # observeEvent(input$value10,  alert_weightMismatch(10, df = strategy_current()))
     # 
     # 
     # ## Reactions
     # 
     # # Update weights on action button
     # observeEvent(input$update, update_weights(df = strategy_current()))
     # 
     # # Update values when total value changes
     # observeEvent(input$total_value, {
     #      for(i in 1:10){
     #           
     #           req(strategy_current()$weight[i] > 0)
     #           
     #           updateNumericInput(
     #                inputId = paste0("value", i),
     #                value   = input$total_value*strategy_current()$weight[i]/100
     #           )
     #      }
     # })
     # 
     # # Update value when weight changes
     # observeEvent(input$weight1,  update_value(id = "value1",  weight = input$weight1,  total = input$total_value))
     # observeEvent(input$weight5,  update_value(id = "value5",  weight = input$weight5,  total = input$total_value))
     # observeEvent(input$weight3,  update_value(id = "value3",  weight = input$weight3,  total = input$total_value))
     # observeEvent(input$weight4,  update_value(id = "value4",  weight = input$weight4,  total = input$total_value))
     # observeEvent(input$weight5,  update_value(id = "value5",  weight = input$weight5,  total = input$total_value))
     # observeEvent(input$weight6,  update_value(id = "value6",  weight = input$weight6,  total = input$total_value))
     # observeEvent(input$weight7,  update_value(id = "value7",  weight = input$weight7,  total = input$total_value))
     # observeEvent(input$weight8,  update_value(id = "value8",  weight = input$weight8,  total = input$total_value))
     # observeEvent(input$weight9,  update_value(id = "value9",  weight = input$weight9,  total = input$total_value))
     # observeEvent(input$weight10, update_value(id = "value10", weight = input$weight10, total = input$total_value))
     # 
     # 
     # 
     # 
     # 
     # 
     # 
     # 
     # 
     # 
     # 
     # weightSum <- eventReactive(input$submit, {
     #      sum(input$weight1, 
     #          input$weight5, 
     #          input$weight3, 
     #          input$weight4,
     #          input$weight5, 
     #          input$weight6, 
     #          input$weight7, 
     #          input$weight8,
     #          input$weight9,
     #          input$weight10)
     # })
     # 
     # allocation <- eventReactive(input$submit, {
     #      
     #      if(isTRUE(base::all.equal(weightSum(), 100))){
     #           tibble::tibble(
     #                Sleeves = c(
     #                     input$sleeve1,
     #                     input$sleeve5,
     #                     input$sleeve3,
     #                     input$sleeve4,
     #                     input$sleeve5,
     #                     input$sleeve6,
     #                     input$sleeve7,
     #                     input$sleeve8,
     #                     input$sleeve9,
     #                     input$sleeve10),
     #                
     #                weight = c(
     #                     input$weight1,
     #                     input$weight5,
     #                     input$weight3,
     #                     input$weight4,
     #                     input$weight5,
     #                     input$weight6,
     #                     input$weight7,
     #                     input$weight8,
     #                     input$weight9,
     #                     input$weight10)) |>
     #                
     #                dplyr::filter(weight != 0) |>
     #                dplyr::group_by(Sleeves) |>
     #                dplyr::summarise(Weight = sum(weight))
     #      } 
     # })
     # 
     # 
     # submit_timestamp <- reactive({format(Sys.time(), format = "%Y%m%d%H%M%S")}) |>
     #      bindEvent(input$submit)
     # 
     # existing <- eventReactive(input$submit, {
     #      
     #      if(isTRUE(all.equal(weightSum(), 100))){
     #           
     #           n <- nrow(allocation())
     #           
     #           matches <- platform |> dplyr::select(strategy)
     #           
     #           print(allocation())
     #           
     #           for (i in 1:n){
     #                
     #                foo <- platform |>
     #                     # dplyr::mutate(agg_target = agg_target/100) |>
     #                     dplyr::filter(model_agg == allocation()$Sleeves[i] & agg_target == allocation()$Weight[i]) |>
     #                     dplyr::pull(strategy)
     #                
     #                matches <- matches |> dplyr::filter(strategy %in% foo)}
     #           
     #           match <- matches |> dplyr::distinct() |> dplyr::pull(strategy)
     #           exists <- length(match > 0)
     #           
     #           if(length(match) == 0){
     #                msg <- "This allocation does not exist. Download to create blended strategy."}
     #           
     #           if(length(match > 0)){
     #                msg <- paste0("This allocation already exists as ", match, ".")}
     #           
     #           list(result = as.logical(exists), message = msg)
     #      }
     # })
     # 
     # 
     # 
     # 
     # 
     # output$text <- renderText({
     #      
     #      print(isTRUE(base::all.equal(weightSum(), 100)))
     #      
     #      if (isTRUE(base::all.equal(weightSum(), 100))){
     #           msg <- ""} else {
     #                if(weightSum() > 100){
     #                     msg <- paste0("Sum of sleeve weights is ", 
     #                                   weightSum(), 
     #                                   "%. Reduce total weight to 100%.")} else {
     #                                        if(weightSum() < 100){
     #                                             msg <- paste0("Sum of sleeve weights is ", 
     #                                                           weightSum(), 
     #                                                           "%. Increase total weight to 100%.")}
     #                                   }
     #           }
     #      
     #      msg
     #      
     # })
     # 
     # output$table   <- renderTable({allocation()})
     # output$matches <- renderText({existing()$message})
     # 
     # output$downloadData <- downloadHandler(
     #      filename = function(){paste0("Blended - ", submit_timestamp(),".csv")},
     #      content  = function(file){
     #           readr::write_csv(
     #                x = allocation() |> 
     #                     dplyr::left_join(sleeve_ids, by = dplyr::join_by(Sleeves==model_agg)) |>
     #                     
     #                     dplyr::bind_rows(tibble::tibble(
     #                          Sleeve = c("Income Sweep", "Unsupervised", "UnAssigned", "Transitional"),
     #                          Weight = rep(0,4),
     #                          model_agg_id = c(69140, 69569, 69577, 69578))) |>
     #                     
     #                     dplyr::mutate(
     #                          `Sleeve Strategy Id`              = "<new>",
     #                          Name                              = paste0("Blended - ", submit_timestamp()),
     #                          `Contribution Allocation Method`  = "Most Out Of Balance",
     #                          `Distribution Allocation Method`  = "Most Out Of Balance",
     #                          `Auto Rebal Frequency`            = "None",
     #                          `Auto Rebal Month`                = "",
     #                          `Auto Rebal Day`                  = "",
     #                          `Entity Id`                       = "",
     #                          Entity                            = "",
     #                          `Tolerance Percent`               = 10,
     #                          `Strategy Detail Id`              = "<new>",
     #                          `Sleeve Type`                     = "Normal",
     #                          `Modelagg Id`                     = model_agg_id,
     #                          `Excluded Rebal Sleeve`            = dplyr::if_else(
     #                               condition = Sleeve %in% c("Income Sweep", "Unsupervised", "UnAssigned", "Transitional"),
     #                               true      = TRUE,
     #                               false     = FALSE),
     #                          `Management Style`                = "",
     #                          `Subadvisor Id`                   = "",
     #                          `Target Allocation`               = Weight,
     #                          `Contribution Allocation`         = "",
     #                          `Distribution Allocation`         = "",
     #                          `Tolerance Upper`                 = Weight/`Tolerance Percent`,
     #                          `Tolerance Lower`                 = Weight/`Tolerance Percent`,
     #                          `Auto Trade Types`                = "",
     #                          `Substitute Detail Id`            = "",
     #                          `Substitute Id`                   = "",
     #                          `Asset Level Allowed`             = "",
     #                          `Allowed Asset Classification ID` = "") |>
     #                     
     #                     dplyr::select(
     #                          `Sleeve Strategy Id`,
     #                          Name,
     #                          `Contribution Allocation Method`,
     #                          `Distribution Allocation Method`,
     #                          `Auto Rebal Frequency`,
     #                          `Auto Rebal Month`,
     #                          `Auto Rebal Day`,
     #                          `Entity Id`,
     #                          Entity,
     #                          `Tolerance Percent`,
     #                          `Strategy Detail Id`,
     #                          `Sleeve Type`,
     #                          `Modelagg Id`,
     #                          `Excluded Rebal Sleeve`,
     #                          `Management Style`,
     #                          `Subadvisor Id`,
     #                          `Target Allocation`,
     #                          `Contribution Allocation`,
     #                          `Distribution Allocation`,
     #                          `Tolerance Upper`,
     #                          `Tolerance Lower`,
     #                          `Auto Trade Types`,
     #                          `Substitute Detail Id`,
     #                          `Substitute Id`,
     #                          `Asset Level Allowed`,
     #                          `Allowed Asset Classification ID`), 
     #                file)})
     # 
     # output$download <- renderUI({
     #      if(weightSum() == 100){
     #           if(!existing()$result){
     #                downloadButton("downloadData", "Download")
     #           }
     #      }
     # })
}




# Run the application 
shinyApp(ui = ui, server = server)
