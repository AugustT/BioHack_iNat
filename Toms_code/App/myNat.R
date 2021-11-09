#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(reticulate)

# 10MB max size
options(shiny.maxRequestSize = 10 * 1024^2)

# load the token
load('token.rdata')

# Import pyinaturalist
pynat <- import('pyinaturalist')

# Define UI 
ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  # Application title
  titlePanel("myNat"),
  
  fluidRow(
    column(12,
           # fileInput(inputId = 'files', 
           #           multiple = TRUE, 
           #           label = 'Choose files (max 10MB each)',
           #           accept = 'audio/*'),
           # div(id = 'console')
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  vals <- reactiveValues(upload_token = NULL)
  
  loginModal <- function(failed = FALSE) {
    modalDialog(
      size = 's',
      textInput("username", "Username",
                placeholder = 'Enter your iNaturalist username'
      ),
      passwordInput("password", label = "Password", 
                    placeholder = 'Enter your iNaturalist password'),
      if (failed)
        div(tags$b("Username or password incorrect", style = "color: red;")),
      
      footer = tagList(
        actionButton("login", "Login")
      )
    )
  }
  
  showModal(loginModal())
  
  observeEvent(input$login, {
    
    # this can be used to test login
    upload_token <- try({
      pynat$get_access_token(input$username,
                             input$password,
                             token[[3]],
                             token[[4]])
    }, silent = TRUE)
    
    vals$upload_token <- upload_token
    
    if(length(upload_token) == 1 &
       class(upload_token) == 'character'){
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
