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
      # selectInput("inputId", #h3("Select box"), 
      #             label = "Function to integrate",
      #             choices = list("exp(x^2)", "log(1+tan(x)^2)",
      #                            "sin(2*x)*cos(x)", "custom"), selected = "sin(2*x)*cos(x)"),
      selectizeInput("int_fun", 
                     label="Function to integrate", 
                     choices=list("exp(x^2)", "log(1+tan(x)^2)",
                                  "sin(2*x)*cos(x)"), multiple = FALSE,
                     options = list(create=TRUE, maxOptions = 5)),
      # conditionalPanel(
      #   condition = "input.inputId == 'custom'",
      #   selectInput("smoothMethod", "Method",
      #               list("lm", "glm", "gam", "loess", "rlm"))
      # ),
      #hidden(textInput("text", 
      #          value = "Enter text...")),
      sliderInput("nsim",
                  "Number of simulations:",
                  min = 1,
                  max = 5000,
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
    hitmiss <- function(limits, ylimits, ifun, N=1000){
      area <- diff(ylimits)*diff(limits)
      smpls <- data.frame(x=runif(N,limits[1],limits[2]), y=runif(N,ylimits[1],ylimits[2]))
      smpls$test <- ifelse( ((smpls$y>=0) & (ifun(smpls$x)>=0) & smpls$y<=ifun(smpls$x)) | ((smpls$y<0) & (ifun(smpls$x)<0) & smpls$y>=ifun(smpls$x)), TRUE, FALSE)
      
      area_pos <- max(0,ylimits)*diff(limits)
      int_pos <- ifelse(sum(smpls$y>=0)>0 , area_pos*sum(smpls[smpls$y>=0,]$test)/nrow(smpls[smpls$y>=0,]), 0)
      area_neg <- min(0,ylimits)*diff(limits)
      int_neg <- ifelse(sum(smpls$y<0)>0, abs(area_neg*sum(smpls[smpls$y<0,]$test)/nrow(smpls[smpls$y<0,])), 0)
      int <- int_pos-int_neg #area*sum(smpls$test)/nrow(smpls)
      list(positive=int_pos, negative=int_neg, integral=int, efficiency=sum(smpls$test)/N, data=smpls)
    }     
    
    # which function is to integrate?
    ifun <- function(x) rep_len(eval(parse(text=input$int_fun)), length(x))#exp(x^2)
    limits <- c(-1,1)
    ylimits <- 1.1*c(min(0, optimize(ifun,lower=limits[1],upper=limits[2],maximum=F)$objective),
                     max(0, optimize(ifun,lower=limits[1],upper=limits[2],maximum=T)$objective) ) #c(-3,3)
    
    # integrate based on input$nsim from ui.R
    fint <- hitmiss(limits, ylimits, ifun, N=input$nsim)
    
    # plot: set margins, draw function and points
    par(mar=c(5.1,5.1,2.1,2.1))
    plot(ifun, xlim=limits, ylim=ylimits, ylab=parse(text=input$int_fun), sub=paste("positive=",fint$positive,", negative=",fint$negative,", area=",fint$integral))
    points(fint$data[,1:2], col=2+fint$data$test)
    abline(0,0,lty=2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

