#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Hit-or-Miss Monte-Carlo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("nsim",
                     "Number of simulations:",
                     min = 1,
                     max = 1000,
                     value = 100)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("hitmissPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$hitmissPlot <- renderPlot({
      # Hit-or-Miss function
      hitmiss <- function(limits, ymax, ifun, N=1000){
        area <- ymax*diff(limits)
        smpls <- data.frame(x=runif(N,limits[1],limits[2]), y=runif(N, 0,ymax))
        smpls$test <- ifelse(smpls$y<ifun(smpls$x), TRUE, FALSE)
        int <- area*sum(smpls$test)/nrow(smpls)
        list(integral=int, efficiency=sum(smpls$test)/N, data=smpls)
      }     
     
      # which function is to integrate?
      ifun <- function(x) exp(x^2)
      limits <- c(-1,1)
      ymax <- 3     
      
      # integrate based on input$nsim from ui.R
      fint <- hitmiss(limits, ymax, ifun, N=input$nsim)
      
      # plot: set margins, draw function and points
      par(mar=c(5.1,4.1,2.1,2.1))
      plot(ifun, xlim=limits, sub=paste("integral area=",fint$integral))
      points(fint$data[,1:2], col=2+fint$data$test)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

