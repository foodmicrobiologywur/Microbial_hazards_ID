library(rlang)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(readxl)
library(dplyr)
library(tidyverse)
library(shinyWidgets)
library(writexl)
library(DT)
library(data.table)
library(markdown)
library(shinyjs)
library(bslib)
library(shinyFeedback)
library(glue)
library(shinybrowser)
library(bslib)
library(spsComps)

source("./Functions/rowcallback45.R")
source("./Functions/rowcallback.R")
source("./Functions/renderfunc.R")
source("./Functions/infobutton.R")
source("./Text_and_graphics/Informationtext.R")
source("./Text_and_graphics/Barcols.R")
source("./Text_and_graphics/defaultpictograms.R")
source('./datas_and_inputoptions.R')


# Define UI for application that draws a histogram
ui <- dashboardPage(title="Mi ID App",
                    ## header
                    dashboardHeader(title = "Microbial Hazards Identification Decision Support System (Mi ID)",
                                    titleWidth = 900),
                    
                    ## Sidebar
                    dashboardSidebar(
                      tags$hr(
                        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                      ),
                      sidebarMenu(id = "sidebar", 
                                  ## define welcome page
                                  menuItem("Welcome page", tabName = "welcome", icon=icon("home")),
                                  
                                  ## set horizontal line
                                  hr(),
                                  
                                  ## define food selection tab
                                  menuItem("Step 1: Food selection", tabName = "Food_selec", icon = icon("burger")),
                                  
                                  ## define food processing tab
                                  menuItem("Step 2: Processing variables", tabName = "Processing_variables", icon = icon("industry")),
                                  
                                  ## define Recontamination tab
                                  menuItem("Step 3: Recontamination", tabName ="Recontamination", icon=icon("bacterium")),
                                  
                                  menuItem("Step 4: Product characteristics", tabName ="Product_char", icon=icon("burger")),
                                  
                                  
                                  menuItem("Step 5: Assocciation selection", tabName ="Product_ass", icon=icon("magnifying-glass")),
                                  
                                  
                                 
                                  ## set horizontal line
                                  hr(),
                                  
                                  ## define disclaimer tab
                                  menuItem("Disclaimer Text",icon=icon("fa-solid fa-eye"),
                                           href = "https://docs.google.com/document/d/1VyCjpTFAaAvwJZVL7kKKzskiJ9pVwJ9jEpBK4IiGY9A/edit"),

                                  
                                  ## set horizontal line
                                  hr(),
                                
                                  ## define the download tab
                                  menuItem("Download", tabName="Download", icon=icon("download")),
                                  
                                  ## Continue to Mira-DSS
                                  
                                  menuItem("Continue to Risk Ranking",icon=icon("ranking-star"),
                                           href = "https://foodlab-upct.shinyapps.io/Risk-ranking/#welcome"),
                                  
                                  ## set a horizontal line
                                  hr(),
                                  
                                  menuItem("User manual", icon = icon("audible"),
                                           href = "https://docs.google.com/document/d/1T1gOCQ0vZeU3C6PaE2yJJIu1hv-8k152qjBhyeThzRY/edit?usp=sharing"),
                                  
                                  menuItem("Github page", icon = icon("github"),
                                           href = "https://github.com/foodmicrobiologywur/Microbial_hazards_ID")
                                  
                      )),
 
                      
                    ## Body
                    dashboardBody(
                      ## add an amazing icon in your browser :D
                      titlePanel(
                        title = tags$head(tags$link(rel="shortcut icon", 
                                                    href="https://raw.githubusercontent.com/alexanderdank/MIDI-app/main/favicon.ico", 
                                                    type="image/vnd.microsoft.icon"))),
                      
                      shinyDashboardThemes(
                        theme = "poor_mans_flatly"
                      ),
                      tabItems(
                        
                      ## Welcome tab
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
                                  
                                  
                                )),
                        
                        ### Food selection tab
                        
                        tabItem(tabName = "Food_selec",
                                h2("The active list of identified microbial hazards after step 1: Food products selection & Hazards pairing"),
                                
                                ## Set a row with the MIDI logo
                                fluidRow(
                                  wurlogo, logo, foodmicro),
                                fluidRow(
                                  
                                  theme  = bs_theme(version = 5),
                                  div(class="row",
                                      div(class="col-lg-6",
                                          div(tags$style(
                                            "img {
                           max-width: 100%;
                              }"), 
                                            uiOutput("dss_scheme_image2"))),
                                      
                                      div(class="col-lg-6", 
                                          box(width=12, h2("Food Selection"),
                                              
                                              radioButtons("radio", label = "Do you want to use FoodEx2 selection or SAFFI food categories", 
                                                           choices = list("FoodEx2" = 1, "SAFFI food categories" = 2),
                                                           selected = 1),  
                                              
                                              
                                              ### Code for when radiobutton == "SAFFI"
                                              conditionalPanel(
                                                condition = "input.radio == 1",       
                                                helpText("Select the food product(s) for which you would like to identify microbial hazards. The hazards will be displayed in the Hazard overview table."),
                                                
                                                
                                                pickerInput(
                                                  inputId = "food_selection",
                                                  label = "Select food item", 
                                                  choices = inputoptions,
                                                  multiple = FALSE)),
                                              
                                              conditionalPanel(
                                                condition = "input.radio == 2",       
                                                helpText("Select the food categories for which you would like to identify microbial hazards. The hazards will be displayed in the Hazard overview table."),
                                                
                                                
                                                pickerInput(
                                                  inputId = "category_selection",
                                                  label = "Select food category", 
                                                  choices = inputoptionssaffi,
                                                  multiple = FALSE)
                                              ),
                                              ## define the color and outline of the knowledge rules box
                                              tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                background:#18BC9C
                }
                .box.box-solid.box-primary{
                background:#18BC9C
                border-bottom-color:#18BC9C;
                border-left-color:#18BC9C;
                border-right-color:#18BC9C;
                border-top-color:#18BC9C;
                }

                ")),
                                              #add a box
                                              box(width = 12, title = "Knowledge rules", status = "primary", solidHeader 
                                                  = TRUE,
                                                  knowledgerules
                                              )
                                          ))),
                                  ## display the saffi table when people select saffi
                                  fluidRow(box(width=12,
                                               uiOutput('display_SAFFI_message'),
                                               uiOutput('display_SAFFI')))),
                                
                                
                                ### Set  row with the output
                                fluidRow(
                                  actionButton(inputId="next1", label="NEXT"),
                                  useShinyjs(),
                                  actionButton("refresh", "Refresh session"),
                                  dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                                 h2("Information"),
                                                 h4(information), width = "300px"),
                                  title = "Hazard count overview",
                                  DT::DTOutput("tbl1"), width = 12
                                )
                        ),
                        
                        
                        ### Food Processing tab
                        tabItem(tabName = "Processing_variables",
                                h2("The active list of identified microbial hazards after step 2: Processing variables"),
                                
                                ## Set a row with the MIDI logo
                                fluidRow(wurlogo, logo, foodmicro),
                                
                                fluidRow(
                                  div(class="row",
                                      div(class="col-lg-6",
                                          div(tags$style(
                                            "img {
                           max-width: 100%;
                              }"), 
                                            uiOutput("dss_scheme_image3"))),
                                      
                                      div(class="col-lg-6", 
                                          box(width=12,
                                              hr(),
                                              helpText(h3("Important"), "Select the applied processing technique. Please check whether your processing conditions are in line with the description in the table."),
                                              hr(),
                                              
                                              pickerInput(
                                                inputId = "processingvar",
                                                label = "Select processing variable",
                                                choices = inputprocessing,
                                                selected = "None"
                                              ),
                                              
                                              prettySwitch(
                                                inputId = "wetdry",
                                                label = "Does it concern a dry food/high-fat food"),
                                              
                                              uiOutput('HPPmessage'),
                                              
                                              uiOutput('HPP600'),
                                              
                                              ## display the processing table
                                              uiOutput('display_panel')),
                                          ## Conditional panel based on which tab is chosen
                                          ## empty when no processing
                                          # tags$head(tags$style(HTML("table {table-layout: fixed;}"))),
                                          # DT::DTOutput("PTE_DES"),
                                          
                                          ## define the color and outline of the knowledge rules box
                                          tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                background:#18BC9C
                }
                .box.box-solid.box-primary{
                background:#18BC9C
                border-bottom-color:#18BC9C;
                border-left-color:#18BC9C;
                border-right-color:#18BC9C;
                border-top-color:#18BC9C;
                }

                ")),
                                          #add a box
                                          box(width = 12, title = "Knowledge rules", status = "primary", solidHeader 
                                              = TRUE,
                                              knowledgerules
                                          )
                                      ))
                                ),
                                
                                
                                ### SEt a row with the output
                                fluidRow(
                                  actionButton(inputId="next2", label="NEXT"),
                                  useShinyjs(),
                                  actionButton("refresh2", "Refresh session"),
                                  dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                                 h2("Information"),
                                                 h4(information), width = "300px"),
                                  title = "Hazards after processing", 
                                  DT::DTOutput("tbl2"), width = 12
                                )
                                
                        ),
                        
                        ### Recontamination tab
                        tabItem(tabName = "Recontamination",
                                h2("The active list of identified microbial hazards after step 3: Recontamination"),
                                
                                ## Set a row with the MIDI logo
                                fluidRow(wurlogo, logo, foodmicro),
                                
                                ## Set a row with input and DSS Scheme
                                fluidRow(
                                  div(class="row",
                                      div(class="col-lg-6",
                                          div(tags$style(
                                            "img {
                           max-width: 100%;
                              }"), 
                                            uiOutput("dss_scheme_image4"))),
                                      
                                      div(class="col-lg-6", 
                                          box(width=12,
                                              helpText(strong("Identify possible hazards due to recontamination")),
                                              prettySwitch(
                                                inputId = "outside_bag",
                                                label = "Was there any processing performed outside of a closed container?"),
                                              helpText("If this button is not selected, it is assumed no container opening has been performed since initial processing
                  and potential hazards due to recontamination are excluded."),
                                              
                                              ## Define a conditional panel
                                              conditionalPanel(
                                                condition = "input.outside_bag == true",
                                                
                                                prettySwitch(inputId = "include_drywet",
                                                             label = "Do you want to include hazards from dry and wet environments in table?"),
                                                
                                                helpText(strong("Please indicate whether the processing environment was dry or wet.")),
                                                dropdownButton(status = 'warning', 
                                                               icon = icon("info"), 
                                                               size = 'xs',
                                                               inline=TRUE,
                                                               h4("Information"),
                                                               helpText("wet processing environment indicates during the food processing, the surrounding environment contains water, or water vapor or is humid. If this is not the case, please select dry environment"), 
                                                               width = "300px"),
                                                
                                                ## processingwet
                                                tags$div(
                                                  materialSwitch(inputId = "env_wet", label = "Dry processing environment", inline = TRUE, status = "primary"),
                                                  tags$span("wet processing environment")
                                                ),
                                                #prettySwitch(
                                                #inputId = "env_wet",
                                                #label = "Was the processing environment wet?"),
                                                
                                                ## Was there any human contact?
                                                prettySwitch(
                                                  inputId = "human_contact",
                                                  label = "was there any human contact to foods or human contact while adding ingredients"),
                                                
                                                prettySwitch(
                                                  inputId = "dry_spices",
                                                  label = "Were any unprocessed dry spices or herbs added?"),
                                                
                                                prettySwitch(
                                                  inputId = "dry_vitamins",
                                                  label = "Were any unprocessed dry vitamins and/or other dry ingredients added?”")
                                              )),
                                          ## define the color and outline of the knowledge rules box
                                          tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                background:#18BC9C
                }
                .box.box-solid.box-primary{
                background:#18BC9C
                border-bottom-color:#18BC9C;
                border-left-color:#18BC9C;
                border-right-color:#18BC9C;
                border-top-color:#18BC9C;
                }

                ")),
                                          #add a box
                                          box(width = 12, title = "Knowledge rules", status = "primary", solidHeader 
                                              = TRUE,
                                              knowledgerules
                                          )
                                      ))
                                  
                                ),
                                
                                
                                
                                ### Set a row with the output
                                fluidRow(
                                  actionButton(inputId="next3", label="NEXT"),
                                  useShinyjs(),
                                  actionButton("refresh3", "Refresh session"),
                                  box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                                     h2("Information"),
                                                     h4(information), width = "300px"),
                                      title = "Hazards after recontamination",
                                      helpText("This table displays the hazards after potential recontamination due to additions after processing.
                              Hazards added to this list due to potential recontamination are displayed in red. Hazards originating from
                             due to initial food product characteristics are displayed in green.
                              It is possible some hazards occur multiple times during recontamination. (Example).
                              This is information is displayed by the amount of times Recontamination is displayed in the table"),
                                      DT::DTOutput("tblrecon"), width = 12)
                                )
                                
                                
                                
                        ),
                        
                        ### Set product characteristics tab
                        tabItem(tabName="Product_char",
                                h2("The active list of identified microbial hazards after step 4: Product characteristic"),
                                
                                ## set the logo
                                fluidRow(wurlogo, logo, foodmicro),
                                
                                ## set the input and logo tab
                                fluidRow(
                                  theme  = bs_theme(version = 5),
                                  div(class="row",
                                      div(class="col-lg-6",
                                          div(tags$style(
                                            "img {
                           max-width: 100%;
                              }"), 
                                            
                                            uiOutput("dss_scheme_image5"))),
                                      
                                      #column(width=6,
                                      div(class="col-lg-6",
                                          
                                          useShinyjs(),
                                          box(width = 12,
                                              tags$div(
                                                barcol,
                                              
                                                sliderTextInput(
                                                  inputId = "pH",
                                                  label = "Indicate the pH of your product:", 
                                                  choices = c(seq(from = 1, to = 14, by = 0.1)),
                                                 grid = TRUE,
                                                  selected = 7 
                                               )
                                               
                                                ),
                                              tags$div(
                                                barcol1,
                                                sliderTextInput(
                                                  inputId = "aw",
                                                  label = "Indicate the Aw of your product:", 
                                                  choices = c(seq(from = 0, to = 1, by = 0.01)),
                                                  grid = TRUE,
                                                  selected = 0.99
                                                )),
                                              pickerInput(
                                                inputId = "chaintemp",
                                                label = "What is the used temperature during the production and transportation chain", 
                                                choices = c("Room temperature", "1-4 degrees Celcius", "Below 0 degrees Celcius")
                                              ),
                                              dropdownButton(status = 'warning', 
                                                             icon = icon("info"), 
                                                             size = 'xs',
                                                             inline=TRUE,
                                                             h4("Information"),
                                                             helpText("Temperature abuse = if there is potential temperature abuse, an increase of > 5°C is expected. E.g., this means 1-4°C will become ~/>10°C. Freezing will become 1-4°C."), 
                                                             width = "300px"),
                                              materialSwitch(
                                                inputId = "tempabuse",
                                                label = "Was there any temperature abuse (uncontrolled temperature) during the production and transportation chain?",
                                                value = FALSE, 
                                                status = "danger"
                                              ),
                                              #swtich to remove bacteria present that need growth for pathogenic effect but are not growing
                                             materialSwitch(
                                                inputId = "removenongrowth",
                                                label = "By default, MHs that can't grow but require growth to cause illness are excluded under your selected conditions.
                                            Do you want to include them if they can survive?",
                                                value = FALSE, 
                                                status = "danger"
                                              )
                                          ),
                                          ## define the color and outline of the knowledge rules box
                                          tags$style(HTML("
                .box.box-solid.box-primary>.box-header {
                background:#18BC9C
                }
                .box.box-solid.box-primary{
                background:#18BC9C
                border-bottom-color:#18BC9C;
                border-left-color:#18BC9C;
                border-right-color:#18BC9C;
                border-top-color:#18BC9C;
                }

                ")),
                                          #add a box
                                          box(width = 12, title = "Knowledge rules", status = "primary", solidHeader 
                                              = TRUE,
                                              knowledgerules
                                          )
                                      )
                                  )),
                                
                                
                                
                                ## set the output row
                                spsDepend("pop-tip"),
                                fluidRow(
                                  actionButton(inputId="next4", label="NEXT"),
                                  useShinyjs(),
                                  actionButton("refresh4", "Refresh session"),
                                  box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                                     h2("Information"),
                                                     h4(information), width = "300px"),
                                      title = "Hazards after product characteristics",
                                      DT::DTOutput("tbl_proces"), width = 12), width=12)
                                
                        ),
                        tabItem(tabName = "Product_ass",
                                h2("The active list of identified microbial hazards step 5: Microbial hazard association level selection"),
                                
                                fluidRow(wurlogo, logo, foodmicro),
                                
                                fluidRow(
                                  theme  = bs_theme(version = 5),
                                  div(class="row",
                                      div(class="col-lg-6",
                                          div(tags$style(
                                            "img {
                           max-width: 100%;
                              }"), 
                                            
                                            uiOutput("dss_scheme_image6"))),
                                      
                                      div(class="col-lg-6",
                                          box(width = 12,
                                              materialSwitch(
                                                inputId = "filtcount",
                                                label = "Filter MHs with low assocation?",
                                                value = FALSE, 
                                                status = "danger"
                                              )),
                                          box(width = 12, title = "Information", status = "warning", solidHeader 
                                              = TRUE,
                                              helpText("The identification results shown here are not ranked. This will be addressed in the next Microbial hazards risk ranking web-based app (Mira)."))
                                      ))),
                                
                                #set the output row
                                fluidRow(
                                  actionButton(inputId="next5", label="NEXT"),
                                  useShinyjs(),
                                  actionButton("refresh5", "Refresh session"),
                                  box(dropdownButton(status = 'success', icon = icon('info'), inline=TRUE,
                                                     h2("Information"),
                                                     h4(information), width = "300px"),
                                      title = "Selected relevant hazards", 
                                      DT::DTOutput("tbl_filt"), width = 12))
                                
                        ),
                        
                        tabItem(tabName = "Download",
                                h2("Download results"),
                                
                                
                                ## set the logo
                                fluidRow(wurlogo, logo, foodmicro),
                                
                  
                                
                                ## set the downloadbutton
                                fluidRow(
                                  box(
                                    helpText("Thank you for using the Microbial Hazards Identification Decision Support System (MiID)!
                           Click the downloadbutton below to download the results of all tabs.
                           The results of each tab are separated in unique excel sheets.
                           (DOI: to be updated)"),
                                    box(width = 12, title = "Information", status = "warning", solidHeader 
                                        = TRUE,
                                        HTML("The identification results shown here are not ranked. This will be addressed in the next Microbial hazards risk ranking web-based app (Mira).
             Available at <a href='https://foodlab-upct.shinyapps.io/Risk-ranking/#welcome' target='_blank'> Mira-DSS https://foodlab-upct.shinyapps.io/Risk-ranking/#welcome </a>.")),
                                    downloadButton("dl", "Download"),
                                    useShinyjs(),
                                    actionButton("refresh6", "Refresh session")))

                                 
                        )
                        
                        
                      ))
)

