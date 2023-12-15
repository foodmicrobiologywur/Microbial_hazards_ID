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
# Define server logic required to draw a histogram

## get the system date

sysdate <- Sys.Date()


server <- function(input, output, session) {
  
  
  ## define a reactive for which url to use (depending on which tab you are) #add ?raw=true at the end of the link for correct display
  urlDSS <- reactive ({
    if (input$sidebar == "welcome") {
      urlDSS <- "https://github.com/kahyenclaireyeak/webpics_MiID/blob/6caafaf6e7ed03a1e0a9927aeaeb951db0a06378/Figure%201-%20Microbial%20Hazards%20Identification%20DSS%20procedures.jpg?raw=true"
    } else if (input$sidebar == "Food_selec") {
      urlDSS <- "https://github.com/kahyenclaireyeak/webpics_MiID/blob/6caafaf6e7ed03a1e0a9927aeaeb951db0a06378/step%201.jpg?raw=true"
    } else if (input$sidebar == "Processing_variables") {
      urlDSS <- "https://github.com/kahyenclaireyeak/webpics_MiID/blob/6caafaf6e7ed03a1e0a9927aeaeb951db0a06378/step2.jpg?raw=true"
    } else if (input$sidebar == "Recontamination") {
      urlDSS <- "https://github.com/kahyenclaireyeak/webpics_MiID/blob/6caafaf6e7ed03a1e0a9927aeaeb951db0a06378/step%203.jpg?raw=true"
    } else if (input$sidebar == "Product_char") {
      urlDSS <-  "https://github.com/kahyenclaireyeak/webpics_MiID/blob/6caafaf6e7ed03a1e0a9927aeaeb951db0a06378/step%204.jpg?raw=true"
    } else if (input$sidebar == "Product_ass") {
      urlDSS <-  "https://github.com/kahyenclaireyeak/webpics_MiID/blob/6caafaf6e7ed03a1e0a9927aeaeb951db0a06378/step%205.jpg?raw=true"
    }
    return(urlDSS)
  })
  
  ## define renderImage for DSS
  output$dss_scheme_image1 <-renderUI ({
    tags$img(src=urlDSS(), 
             alt="Decision support system graphical overview", deleteFile=FALSE)
  })
  
  output$dss_scheme_image2 <-renderUI ({
    tags$img(src=urlDSS(), 
             alt="Decision support system graphical overview", deleteFile=FALSE)
  })
  
  output$dss_scheme_image3 <-renderUI ({
    tags$img(src=urlDSS(), 
             alt="Decision support system graphical overview", deleteFile=FALSE)
  })
  
  
  output$dss_scheme_image4 <-renderUI ({
    tags$img(src=urlDSS(), 
             alt="Decision support system graphical overview", deleteFile=FALSE)
  })
  
  
  output$dss_scheme_image5 <-renderUI ({
    tags$img(src=urlDSS(), 
             alt="Decision support system graphical overview", deleteFile=FALSE)
  })
  
  output$dss_scheme_image6 <-renderUI ({
    tags$img(src=urlDSS(), 
             alt="Decision support system graphical overview", deleteFile=FALSE)
  })
  
  
  ## define logic for displaying or not displaying the SAFFI table
  output$display_SAFFI <- renderUI ({
    if (input$radio == "2") {
      tags$head(tags$style(HTML("table {table-layout: fixed;}")))
      DT::DTOutput("SAFFI_table")  
      
    } else {}
  })
  
  ## define logic for displaying or not displaying the SAFFI table message
  output$display_SAFFI_message <- renderUI ({
    if (input$radio == "2") {
      helpText("Use the below table to select the correct SAFFI food category")
      
    } else {}
  })
  
  ## define render table for saffi table
  output$SAFFI_table <- DT::renderDT({
    
    SAFFI_disp <- SAFFI_table
    SAFFI_disp <- as.data.frame(SAFFI_disp)
    datatable(SAFFI_disp,
              rownames=FALSE,
              options = list(
                scrollX=TRUE,
                autoWidth = FALSE,
                columnDefs = list(
                  list(width = '50px', targets = "_all")),
                searching = FALSE,
                paging = FALSE  
              ), escape=FALSE)
    
  })
  ## define logic for displaying the numeric input options on tab 1 
  output$numeric_input <- renderUI({
    selected_user_hazards <- input$Manual_hazards_selection
    
    numeric_input_list <- lapply(selected_user_hazards, function(option){
      if(option %in% manual_user_options){
        numericInput(
          inputId = paste0("numericcounthazard", option),
          label = paste("Enter count of user hazard", option),
          value = 0,
          max = 5
        )
      }
      })
      
      tagList(numeric_input_list)
  })
  
  ### retrieve the datqa frame from dynamically rendered values
  # Reactive data frame based on dynamic inputs
  user_hazards <- reactive({
    selected_user_hazards <- input$Manual_hazards_selection
    
    # Retrieve values from dynamic inputs
    input_values <- sapply(selected_user_hazards, function(i) {
      input_id <- paste0("numericcounthazard", i)
      input_value <- input[[input_id]]
      return(input_value)
    })
    
    # Create a data frame with input$num_inputs and input values
    if(length(input_values) == 0){
      data.frame()
    } else {
    data.frame(Genusspecies = selected_user_hazards, Count = input_values)
    }
  })
  
  
  
  ## make the user defined table for row bind to table 1 based on user input on food category
  tbl_0 <- reactive({
    manual_user_table <- data.frame(Genus = character(), Species = character(), 
                                    Food_main_category = character(), Type = character(), 
                                    Count = numeric(), stringsAsFactors = FALSE)
    
    if (nrow(user_hazards()) > 0) {
      types <- food_categories %>% select(Type, Genus, Species)
      user_table <- user_hazards() %>%
        separate(Genusspecies, into = c("Genus", "Species"), sep = " ", extra = "merge") %>%
        left_join(types, by = c("Species", "Genus")) %>%
        mutate(Food_main_category = "Manual user input") %>%
        select(Genus, Species, Food_main_category, Type, Count) %>%
        distinct(Type, Genus, Species, .keep_all = TRUE)
      
      # Combine the data frames
      manual_user_table <- bind_rows(manual_user_table, user_table)
    }
    
    
    return(manual_user_table)
  })
  
  
  ### display a table for selecting and showing the relevant food categories and risks to the user
  tbl_1 <- reactive({
    manual_user_table <- tbl_0()
    if(input$radio == 1) {
      ## make tbl for showing hazards
      selectedfood <- input$food_selection
      
      selected_category <- food_products %>% filter(str_to_sentence(Food_item) %in% selectedfood)
      food_sub_categoryvec <- selected_category %>% pull(Food_subcategory_1)
      
      ## extract relevant hazards from categorie list
      Hazard_counts <- food_categories %>% filter(Food_subcategory_1 %in% food_sub_categoryvec)

      
      ## print table of releant counts and sort based on highest hazard
      Hazard_counts <- Hazard_counts %>% select(1:14) %>% filter(Count > 0) %>% arrange(desc(Count), Food_subcategory_1) %>% select(-Food_subcategory_1)
      print(Hazard_counts)
      
      ## save as table output 1
      tbl_1 <- bind_rows(Hazard_counts, manual_user_table)
      
    } else {
      food_sub_categoryvec <- input$category_selection
      
      
      ## extract relevant hazards from categorie list
      Hazard_counts <- food_categories %>% filter(str_to_sentence(Food_subcategory_1) %in% food_sub_categoryvec)
      
      ## print table of releant counts and sort based on highest hazard
      Hazard_counts <- Hazard_counts %>% select(1:14) %>% filter(Count > 0) %>% arrange(desc(Count), Food_subcategory_1) %>% select(-Food_subcategory_1)
      print(Hazard_counts)
      
      ## save as table output 1
      tbl_1 <- bind_rows(Hazard_counts,manual_user_table)
    }
    
    tbl_1 <- tbl_1 %>% group_by(Type, Genus, Species) %>% 
      mutate(Count = sum(Count)) %>% #change count based on user input
      mutate(Count = if_else(Count > 5, 5, Count))
      })
  
  ## display warning if count is above 5
  output$Countwarning <- renderText({
    if(max(user_hazards()$Count) > 0){
     text <- "Counts are added to the original count already attributed to a hazard if it was already present in the table.
     Counts can never be higher than 5, as this has been set as the maximum of the scale." 
    }
  })
  
  output$tbl1 <- DT::renderDT({
    to_display <- tbl_1()
    to_display <- to_display %>% select(Food_main_category, Type, Genus, Species, Count)
    datatable(to_display %>% arrange(desc(Count)) %>% mutate(Count = case_when(
      Count == 0 ~ "Low Risk (0)",
      Count == 1 ~ "Low Risk (1)",
      Count == 2 ~ "Low Risk (2)",
      Count == 3 ~ "Medium Risk (3)",
      Count == 4 ~ "High Risk (4)",
      Count == 5 ~ "High Risk (5)"),
      Food_main_category = str_to_sentence(Food_main_category)) %>%
        relocate(Count, .after = Species) %>%
        dplyr::rename("Risk Association" = Count,
                      "Food main category" = Food_main_category),
      options = list(
        scrollX=TRUE,
        language = list(
          zeroRecords = "There are no identified microbial hazards under the selected scenarios"),
        rowCallback = JS(rowCallback))
    )
    #"Food subcategory" = Food_sub_to_print))
  })
  
  
  ### Display a table with selection of thermal processing and wet or dry food
  tbl_2 <- reactive({
    tbl_1 <- tbl_1()
    ##extract data which bacteria to consider from tbl_1
    selechazard <- left_join(tbl_1, inactivationsheet, by =c("Type", "Genus", "Species"))
    
    ##Filter based on which thermal processing user entered
    processhazard <- selechazard %>% filter(Processing_condition %in% input$processingvar)
    
    ## select to show either wet or dry column based on user input
    
    if(input$wetdry == TRUE) {
      processhazdry <- processhazard %>% filter(Hazard_inactivation5D_dryfood == "no")
      tbl_hazarddry <- processhazdry %>% select(Food_main_category, Type, Genus, Species, Hazard_inactivation5D_dryfood)
    } else{
      processhazdry <- processhazard %>% filter(Hazard_inactivation5D == "no")
      tbl_hazarddry <- processhazdry %>% select(Food_main_category, Type, Genus, Species, Hazard_inactivation5D)
    }
    
    ## save as table output 1
    tbl_2 <- tbl_hazarddry
    
  })
  
  output$tbl2 <- DT::renderDT({
    to_display2 <- tbl_2()
    to_display2 <- as.data.frame(to_display2 %>% mutate(Food_main_category = str_to_sentence(Food_main_category)) %>%
                                   dplyr::rename("Food main category" = Food_main_category))
    if(input$wetdry == TRUE) {
      to_display2 <- datatable(to_display2 %>% dplyr::rename("5 log inactivation?" = Hazard_inactivation5D_dryfood),
                               options = list(
                                 language = list(
                                   zeroRecords = "There are no identified microbial hazards under the selected scenarios")))
    } else {
      to_display2 <- datatable(to_display2 %>% dplyr::rename("5 log inactivation?" = Hazard_inactivation5D),
                               options = list(
                                 rowCallback = JS(rowCallback),
                                 language = list(
                                   zeroRecords = "There are no identified microbial hazards under the selected scenarios")))
    }            
    
  })
  
  ### Display a table with selection of recontamination possibilies
  tbl_recon <- reactive({
    tbl_2 <- tbl_2()
    
    ### Only allow output in recontamination if indeed processing outside of a closed container!
    
    if(input$outside_bag == TRUE) {
      if(input$include_drywet == TRUE) {
        if(input$env_wet == TRUE) {
          recontamination_to_add <- recontamination %>% filter(Wet_processing_environments == 'Yes') %>% select(Type, Genus, Species, Hazard_origin, Food_main_category)
          tbl_2$Hazard_origin = tbl_2$Food_main_category
          tbl_3 <- add_row(tbl_2, recontamination_to_add)
        }  else {
          recontamination_to_add <- recontamination %>% filter(Dry_processing_environments == 'Yes') %>% select(Type, Genus, Species, Hazard_origin, Food_main_category)
          tbl_2$Hazard_origin = tbl_2$Food_main_category
          tbl_3 <- add_row(tbl_2, recontamination_to_add)
        }
      } else {
        tbl_2$Hazard_origin = tbl_2$Food_main_category
        tbl_3 <- tbl_2
      }
      
      if(input$human_contact == TRUE) {
        recontamination_to_add <- recontamination %>% filter(Human_contact == 'Yes') %>% select(Type, Genus, Species, Hazard_origin, Food_main_category)
        tbl_3 <- add_row(tbl_3, recontamination_to_add)
      }  else {
        tbl_3 <- tbl_3
      }
      
      if(input$dry_spices == TRUE) {
        recontamination_to_add <- recontamination %>% filter(Dry_ingredients_spices == 'Yes') %>% select(Type, Genus, Species, Hazard_origin, Food_main_category)
        tbl_3 <- add_row(tbl_3, recontamination_to_add)
      }  else {
        tbl_3 <- tbl_3
      }
      
      if(input$dry_vitamins == TRUE) {
        recontamination_to_add <- recontamination %>% filter(Dry_ingredients_vitamins == 'Yes') %>% select(Type, Genus, Species, Hazard_origin, Food_main_category)
        tbl_3 <- add_row(tbl_3, recontamination_to_add)
      }  else {
        tbl_3 <- tbl_3
      }
      
    } else {
      tbl_2$Hazard_origin = tbl_2$Food_main_category
      tbl_3 <- tbl_2
    }
    tbl_3 <- tbl_3 %>% group_by(Type,Genus,Species) %>%
      mutate(Hazard_origin2 = toString(Hazard_origin)) %>% 
      distinct_at(vars(c(Type, Genus, Species, Hazard_origin2))) %>% relocate(Hazard_origin2, .before = Type) %>%
      as.data.frame()
    #tbl_3 <- tbl_3 %>% distinct()
    
  })
  
  ## output code for tabpanel
  output$tblrecon <- DT::renderDT({
    tbl_3 <- tbl_recon()
    
    
    to_display3 <- tbl_3
    # to_display3 <- to_display3 %>% mutate(Food_main_category = str_to_sentence(Food_main_category)) %>% 
    to_display3 <- to_display3 %>%
      dplyr::rename("Hazard origin" = Hazard_origin2) 
    datatable(to_display3 %>% mutate(`Hazard origin` = str_to_sentence(`Hazard origin`)),
              options = list(
                scrollX=TRUE,
                rowCallback = JS(rowCallback),
                language = list(
                  zeroRecords = "There are no identified microbial hazards under the selected scenarios"),
                columnDefs = list(
                  list(targets = "_all", render = JS(render))))
    )
    
  })
  
  ### Adapt the table based on product characteristics
  tbl_proces <- reactive({
    tbl_4 <- tbl_recon()
    
    ## Make a table with all columns
    tbl_4 <- left_join(tbl_4, PPC)
   # tbl_4 <- left_join(tbl_4, PPC_GO)
    
    ## define temperatures for different selection of user
    if(input$chaintemp == "Room temperature") {
      temperature <- 20
    } else if(input$chaintemp == "1-4 degrees Celcius"){
      temperature <- 1
    } else {
      temperature <- -20
    }
    
    if(input$tempabuse == TRUE) {
      if(temperature == -20) {
        temperature <- 1
      } else if(temperature == 1){
        temperature <- temperature + 5
      } else {
        
      }
    } else {
      tbl_4 <- tbl_4
    }
    
    ## Extract all rows with "no growth needed"
    No_growth_tbl_4 <- tbl_4 %>% filter(Growth_needed == "no")  
   
    if(input$removenongrowth == FALSE) {
    ## Filter based on cardinal parameters using the 4_step_cardinal
    growth_tbl_4 <- tbl_4 %>% filter(Growth_needed == "yes") %>% 
                      filter(pHmin <= as.numeric(input$pH),
                          pHmax >= as.numeric(input$pH),
                           awmin <= as.numeric(input$aw),
                           awmax >= as.numeric(input$aw),
                           Tmin <= temperature,
                           Tmax >= temperature)
    } else {
  growth_tbl_4 <- tbl_4 %>% filter(Growth_needed == "yes")
  growth_tbl_4 <- left_join(tbl_4, PPC_GO)

  ## filter based on Aw
  if(input$aw < 0.5) {
    growth_tbl_4 <- growth_tbl_4 %>% filter(Survival_Aw0205 == "yes")
  }  else {
    if(input$aw >= 0.5 & input$aw < 0.9){
      ## Assume pathogens needing growth and aw above 0.9 to die between 0.5 and 0.9
      growth_tbl_4 <- growth_tbl_4 %>% filter(Survival_Aw0509 == "yes")
    }
    else {
     growth_tbl_4 <- growth_tbl_4
    }
    
    ## temperature during chain
    if(input$tempabuse == FALSE) {
      if(input$chaintemp == "Room temperature") {
        growth_tbl_4 <- growth_tbl_4
      } else {
        if(input$chaintemp == "1-4 degrees Celcius") {
          growth_tbl_4 <- growth_tbl_4 %>% filter(survive_1_4_degrees == "yes")
          # 
        } else {
          if(input$chaintemp == "Below 0 degrees Celcius") {
            growth_tbl_4 <- growth_tbl_4 %>% filter(survive_below_0_degrees == "yes")
            
          }
        }
      }
      # define logic when there is temp abuse
    } else {
      if(input$chaintemp == "Below 0 degrees Celcius") {
        growth_tbl_4 <- growth_tbl_4 %>% filter(survive_1_4_degrees == "yes")
      } else {
        growth_tbl_4 <- growth_tbl_4
      }
    }
    
  }
}
     tbl_4 <- full_join(No_growth_tbl_4, growth_tbl_4)
    
    Hazard_count <- tbl_1() %>% select(c("Type", "Genus", "Species", "Count"))
    tbl_4 <- left_join(tbl_4, Hazard_count, by = c("Type", "Genus", "Species"))
    
  })
  
  ## output code for tabpanel
  output$tbl_proces <- DT::renderDT({
 #   targets_to_hide <- targets_to_hide()
    # load the df
    to_display4 <- tbl_proces()
    
    to_display4 <- to_display4 %>% arrange(desc(Count)) %>% mutate(Count = case_when(
      Count == 0 ~ "Low Risk (0)",
      Count == 1 ~ "Low Risk (1)",
      Count == 2 ~ "Low Risk (2)",
      Count == 3 ~ "Medium Risk (3)",
      Count == 4 ~ "High Risk (4)",
      Count == 5 ~ "High Risk (5)")) %>%
      # Food_main_category = str_to_sentence(Food_main_category))%>%
      relocate(Count, .after = Species) %>% relocate(Hazard_origin2, .before = Type) %>%
      dplyr::rename(#"Risk Association" = Count,
        "Hazard origin" = Hazard_origin2,
        "Growth needed" = Growth_needed)
     #   "Survival Aw 0.2 - 0.5" = Survival_Aw0205,
      #  "Survival Aw 0.5 - 0.9" = Survival_Aw0509,
      #  "Growth above Aw 0.9" = Growth_aboveAw0.99,
        # "Food main category"= Food_main_category,
     #   "Survive or grow below pH 4.5" = Growth_below4.5,
      #  "Survive or grow above pH 10" = Growth_above10,
       # "Survive or grow refridgerator" = survive_or_grow_1_4_degrees,
        #"Survive or grow in freezer" = survive_or_grow_below_0_degrees)
    
    
    
    
    
    to_display4 <- to_display4 %>% 
      dplyr::select(c(#`Food main category`,
        `Hazard origin`, 
        "Type", "Genus", "Species", 
        "Count", 
        `Growth needed`, 
        Remarks,
        #`Survive or grow below pH 4.5`, 
        #`Survive or grow above pH 10`, 
        #`Survival Aw 0.2 - 0.5`,
        #`Survival Aw 0.5 - 0.9`,
        #`Growth above Aw 0.9`,
        #`Survive or grow refridgerator`,
        #`Survive or grow in freezer`
      ))
    ## rename the column Risk association to include the button
    Risk_text <- tags$span(
      "Risk Association", 
      infoBtn('Risk Association') %>% 
        bsPopover(title = "Risk Association",
                  content = "Risk Association of identified hazard solely reflects the association with the food product, not as a possible recontaminant.",
                  placement = "top",
                  trigger = "hover")
    ) %>% 
      as.character()
    
    
    ## apply the visible filter in the table and color the table based on recontamination
    DT::datatable(to_display4 %>% mutate(`Hazard origin` = str_to_sentence(`Hazard origin`)) %>%
                    rename(!!Risk_text:=Count),
                  escape=FALSE,
                  options = list(
                    rowCallback = JS(rowCallback45),
                    language = list(
                      zeroRecords =  "There are no identified microbial hazards under the selected scenarios"),
                   scrollX = TRUE)) %>%
                  #  columnDefs = list(
                   #   list(#targets = "_all", 
                    #    render = JS(render))
                  #)
               formatStyle(
                          columns = "Growth needed",
                         backgroundColor = styleEqual(c("no","yes"), c('tomato', 'khaki'))
                        )
  })
  
  
  ## Define code for tab 5
  tbl_filt <- reactive({
    tbl_5 <- tbl_proces()
    
    if(input$filtcount == TRUE) {
      
      tbl_5 <- tbl_5 %>% filter(Count >2)
      
    } else {
      tbl_5 <- tbl_5
    }
    
  })
  
  
  ## output code for tabpanel
  output$tbl_filt <- DT::renderDT({
    to_display5 <- tbl_filt()
  #  targets_to_hide <- targets_to_hide()
    to_display5 <- to_display5 %>% arrange(desc(Count)) %>% mutate(Count = case_when(
      Count == 0 ~ "Low Risk (0)",
      Count == 1 ~ "Low Risk (1)",
      Count == 2 ~ "Low Risk (2)",
      Count == 3 ~ "Medium Risk (3)",
      Count == 4 ~ "High Risk (4)",
      Count == 5 ~ "High Risk (5)")) %>%
      relocate(Count, .after = Species)  %>%
      #dplyr::mutate(Food_main_category = str_to_sentence(Food_main_category)) %>%
      dplyr::rename(#"Risk Association" = Count,
        "Hazard origin" = Hazard_origin2,
        "Growth needed" = Growth_needed)
     #   "Survival Aw 0.2 - 0.5" = Survival_Aw0205,
      #  "Survival Aw 0.5 - 0.9" = Survival_Aw0509,
       # "Growth above Aw 0.9" = Growth_aboveAw0.99,
        # "Food main category"= Food_main_category,
      #  "Survive or grow below pH 4.5" = Growth_below4.5,
      #  "Survive or grow above pH 10" = Growth_above10,
      #  "Survive or grow refridgerator" = survive_or_grow_1_4_degrees,
      #  "Survive or grow in freezer" = survive_or_grow_below_0_degrees)
    
    
    to_display5 <- to_display5 %>% 
      dplyr::select(c(#`Food main category`,
        `Hazard origin`, 
        "Type", "Genus", "Species", 
        "Count",
        `Growth needed`, 
        Remarks,
     #   `Survive or grow below pH 4.5`, 
      #  `Survive or grow above pH 10`, 
      #  `Survival Aw 0.2 - 0.5`,
      #  `Survival Aw 0.5 - 0.9`,
        #`Growth above Aw 0.9`,
      #  `Survive or grow refridgerator`,
       # `Survive or grow in freezer`
      ))
    
    Risk_text <- tags$span(
      "Risk Association", 
      infoBtn('Risk Association') %>% 
        bsPopover(title = "Risk Association",
                  content = "Risk Association of identified hazard solely reflects the association with the food product, not as a possible recontaminant.",
                  placement = "top",
                  trigger = "hover")
    ) %>% 
      as.character()
    
    DT::datatable(to_display5 %>% mutate(`Hazard origin` = str_to_sentence(`Hazard origin`)) %>%
                    rename(!!Risk_text:=Count),
                  escape=FALSE,
                  options = list(
                    rowCallback = JS(rowCallback45),
                    language = list(
                      zeroRecords =  "There are no identified microbial hazards under the selected scenarios"),
                    scrollX = TRUE)) %>%
    #  columnDefs = list(
    #   list(#targets = "_all", 
    #    render = JS(render))
    #)
      formatStyle(
               columns = "Growth needed",
              backgroundColor = styleEqual(c("no","yes"), c('tomato', 'khaki'))
            )
  })
  
  
  
  ##define logic for next page buttons
  observeEvent(input$start, {
    updateTabItems(session, "sidebar", "Food_selec") 
  })
  
  observeEvent(input$next1, {
    updateTabItems(session, "sidebar", "Processing_variables") 
  })
  
  observeEvent(input$next2, {
    updateTabItems(session, "sidebar", "Recontamination") 
  })
  
  observeEvent(input$next3, {
    updateTabItems(session, "sidebar", "Product_char") 
  })
  
  observeEvent(input$next4, {
    updateTabItems(session, "sidebar", "Product_ass") 
  })
  
  observeEvent(input$next5, {
    updateTabItems(session, "sidebar", "Download") 
  })
  
  
  
  library(data.table)
  
  ## download all results
  data_list <- reactive({
    list(
      tab_1 = tbl_1(),
      tab_2 = tbl_2(),
      tab_3 = tbl_recon(),
      tab_4 = tbl_proces(),
      tab_5 = tbl_filt()
    )
  })
  
  output$dl <- downloadHandler(
    filename = function() {paste0(sysdate, " ", "all_results.xlsx")},
    content = function(file) {write_xlsx(data_list(), path = file)}
  )
  `%nin%` = Negate(`%in%`)
  ## make a reactive with which PTE_des table to show
  thermal <- inactivationsheet %>% filter(Processing_technique == "Thermal")
  thermal <- unique(thermal$Processing_condition)
  nonthermal <- inactivationsheet %>% filter(Processing_technique %nin% c("Thermal","None"))
  nonthermal <- unique(nonthermal$Processing_condition)
  ### Enable a Processing table display on processing technique tab
  
  
  #make a reactive to display the panel or not in recontamination tab
  output$display_panel <- renderUI ({
    if (input$processingvar == "None"){
      h4("No processing variable selected")
    } else {
      tags$head(tags$style(HTML("table {table-layout: fixed;}")))
      DT::DTOutput("PTE_DES") 
    }
  })
  
  output$HPPmessage <- renderUI({
    if (input$processingvar %in% nonthermal) {
      ##message if dry food
      if (input$wetdry == TRUE) {  
        h4("HPP is not used in the industry to inactivate microbial hazards in dry food products yet due to variations in inactivation and low confidence in full inactivation")
      } else {}
      
    } else {
      
    }
    
  })
  
  output$HPP600 <- renderUI({
    ## message if >600 mPA is selected
    if (input$processingvar == "> 600 MPa") {
      h4("*Non-thermal indicates that the maximum processing temperature is 40°C or below. The upper limit of commercial HPP equipment is 600 MPa, and a holding time of 6 min. Thus data obtained for pressure > 600 MPa and incorporated in this tool are small scale laboratory test results. As temperature increase with increase in pressure, (~3°C in every 100 MPa), pressure > 600 MPa will likely be a thermal HPP inactivation instead of non-thermal HPP inactivation")
      
    } else {}
    
  })
  
  
  
  output$PTE_DES <- DT::renderDT({
    #PTE_DES <- PTE_DES %>% dplyr::rename("Processing technique" = Processing_technique,
    #                                    "Processing condition" = `Processing condition`)
    to_displaydes <- PTE_DES
    to_displaydes <- as.data.frame(to_displaydes)
    
    if( input$processingvar %in% thermal ){
      to_displaydes <- to_displaydes %>% filter(`Processing technique` == "Thermal")%>%
        select(1:4)
    } else {
      to_displaydes <- to_displaydes %>% filter(`Processing technique` != "Thermal")%>%
        select(1:4,5:6)
    }
    
    datatable(to_displaydes,
              rownames=FALSE,
              options = list(
                scrollX = TRUE,
                autoWidth = FALSE,
                columnDefs = list(list(width = '50px', targets = "_all")),
                searching = FALSE,
                paging = FALSE  
              ))
    
  })#,
  # options = list(
  #  autoWidth = FALSE,
  # columnDefs = list(list(width = '50px', targets = "_all")),
  #searching = FALSE,
  #paging = FALSE
  #))
  
  
  ### Define the refresh button
  observeEvent(input$refresh, {
    refresh()
  })
  observeEvent(input$refresh2, {
    refresh()
  })
  observeEvent(input$refresh3, {
    refresh()
  })
  observeEvent(input$refresh4, {
    refresh()
  })
  observeEvent(input$refresh5, {
    refresh()
  })
  observeEvent(input$refresh6, {
    refresh()
  })
  
  ## Define a warning message when opening tabpanel 4
  observeEvent(input$sidebar, {
    if (input$sidebar == "Product_char")  {
      tabtemp <- tbl_proces()
      
      if("Viruses" %in% tabtemp$Type){
        showModal(modalDialog(
          title = h2("Important message"),
          h3("The survival of viruses and parasites in the selected scenario is unknown, but please note that no growth in foods 
        is needed for parasites or viruses to cause illness in humans."),
          easyClose = TRUE
        )) } else if ("Parasites" %in% tabtemp$Type) {
          showModal(modalDialog(
            title = h2("Important message"),
            h3("The survival of viruses and parasites in the selected scenario is unknown, but please note that no growth in foods 
        is needed for parasites or viruses to cause illness in humans."),
            easyClose = TRUE
          ))
        } else {}
    }else{}
    
  })
  
  ## Define a warning message when cronobacter is in the output list in tab 5
  observeEvent(input$sidebar, {
    if(input$sidebar == "Product_ass") { 
      tabtemp2 <- tbl_filt()
      if("Cronobacter" %in% tabtemp2$Genus){
        showModal(modalDialog(
          title = h2("Important message"),
          h3("Cronobacter spp. has been identified as a potential hazard, 
             but is only critical for infants < 6 months and mainly linked to powder infant formula.
             It has a low association chance with other food products, so you 
             may exclude it from your consideration if deemed irrelevant to your case study"),
          easyClose = TRUE
        ))
      } else {}
    }else{}
  })
  
}