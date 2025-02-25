

library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
     
     # Application title
     titlePanel("Save ggplot plot/table without regenration"),
     
     # Sidebar with a slider input for number of bins
     sidebarLayout(
          sidebarPanel(
               downloadButton('export')
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
               plotOutput("p1"),
               plotOutput("p2"),
               tableOutput("t1")
          )
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
     ## vals will contain all plot and table grobs
     vals <- reactiveValues(p1=NULL,p2=NULL,t1=NULL)
     
     ## Note that we store the plot grob before returning it 
     output$p1 <- renderPlot({
          vals$p1 <- qplot(speed, dist, data = cars)
          vals$p1
     })
     
     output$p2 <- renderPlot({
          vals$p2 <- qplot(mpg, wt, data = mtcars, colour = cyl)
          vals$p2
     })
     ## same thing for th etable grob
     output$t1 <- renderTable({
          dx <- head(mtcars)
          vals$t1 <- tableGrob(dx)
          dx
     })
     ## clicking on the export button will generate a pdf file 
     ## containing all grobs
     output$export = downloadHandler(
          filename = function() {"plots.pdf"},
          content = function(file) {
               pdf(file, onefile = TRUE)
               grid.arrange(vals$p1,vals$p2,vals$t1) 
               dev.off()
          }
     )
}

# Run the application 
shinyApp(ui = ui, server = server)
