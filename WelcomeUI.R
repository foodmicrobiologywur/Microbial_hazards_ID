### UI code for tab 1

## Welcome tab

WelcomeUI <- function(id) {
  
tagList(
tabItem(tabName = "welcome", 
        h2("Welcome!"),
        wurlogo, logo, foodmicro,
        
        ## the rows with text and image
        fluidRow(
          theme  = bs_theme(version = 5),
          div(class="row",
              div(class="col-lg-6",
                  # column(width=6,
                  
                  div(tags$style(
                    "img {
                           max-width: 100%;
                              }"), 
                    uiOutput("dss_scheme_image1"))),
              
              div(class="col-lg-6",
                  withMathJax(includeMarkdown("welcome_page.md")),
                  actionButton(inputId="start", label="Start")))
          
          
        ))
)
  
}


WelcomeServer <- function(id) {
  moduleServer(id, function(input, output, session) {

## define a reactive for which url to use (depending on which tab you are)
urlDSS <- reactive ({
  if (input$sidebar == "welcome") {
    urlDSS <- "https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures.jpg?raw=true"
  } else if (input$sidebar == "Food_selec") {
    urlDSS <- "https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%201.jpg?raw=true"
  } else if (input$sidebar == "Processing_variables") {
    urlDSS <- "https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%202.jpg?raw=true"
  } else if (input$sidebar == "Recontamination") {
    urlDSS <- "https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%203.jpg?raw=true"
  } else if (input$sidebar == "Product_char") {
    urlDSS <-  "https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%204.jpg?raw=true"
  } else if (input$sidebar == "Product_ass") {
    urlDSS <-  "https://github.com/alexdank/pics/blob/main/Microbial%20Hazards%20Identification%20DSS%20procedures%20step%205.jpg?raw=true"
  }
})

## define renderImage for DSS
output$dss_scheme_image1 <-renderUI ({
  tags$img(src=urlDSS(), 
           alt="Decision support system graphical overview", deleteFile=FALSE)
})
  })
}


## test the app
welcomeApp <- function() {
ui <- fluidPage(
  WelcomeUI("welcome")
)
server <- function(input, output, session) {
  WelcomeServer("welcome")
}
shinyApp(ui, server)  
}