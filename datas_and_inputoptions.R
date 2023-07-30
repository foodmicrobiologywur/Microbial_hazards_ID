

##load background excel sheet
## load the excel sheet for food categories
food_categories <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx",sheet = "Step_1b_HFP_subcategory")


## load food products
food_products <- HI_Microbial_hazards_identification_processed_database <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_1a_FPS")

## load the inactivation sheet (+wet/dry processing)
inactivationsheet <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_2_PE")

## input options food products
food_products <- food_products %>% mutate(optionnames = glue("{Food_item} ({Code}) [{Hierachy}]"))

inputoptions <- str_to_sentence(food_products$Food_item)
names(inputoptions) <- str_to_sentence(food_products$optionnames)
#make list alphabetical
inputoptions <- sort(inputoptions)

## input options SAFFi
food_products <- food_products %>% mutate (SAFFI = ifelse(food_products$Food_main_category == food_products$Food_subcategory_1, food_products$Food_subcategory_1,
                                                          ifelse(food_products$Food_main_category != food_products$Food_subcategory_1, glue("{food_products$Food_main_category}: {food_products$Food_subcategory_1}"), "Error"))) %>% 
  filter(!is.na(Food_subcategory_1))

inputoptionssaffi <- str_to_sentence(na.omit(unique(food_products$Food_subcategory_1)))
names(inputoptionssaffi) <- str_to_sentence(unique(food_products$SAFFI))
#make list alphabetical
inputoptionssaffi <- sort(inputoptionssaffi)

## picker input for processing type (thermal or non thermal)
#inputprocessing <- unique(inactivationsheet$Processing_condition)

##creat the names for inputprocessing
inactivationsheetall <- inactivationsheet %>% filter(Processing_technique != "None") %>% mutate(optionnames = glue("{Processing_technique}: {Processing_condition}"))
inactivationsheetnone <- inactivationsheet %>% filter(Processing_technique == "None") %>% mutate(optionnames = glue("None"))
inactivationsheet <- rbind(inactivationsheetall, inactivationsheetnone)
inputprocessing <- unique(inactivationsheet$Processing_condition)
names(inputprocessing) <- unique(inactivationsheet$optionnames)
#inputprocessing <- unique(inputprocessing)


## recontamination risk
recontamination <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_3_RP")

recontamination$Hazard_origin = "Recontamination"
recontamination$Food_main_category = "Recontamination"


### Product properties characteristics
PPC <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_4_cardinal")
PPC_GO <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_4_GO")

##define table for SAFFI selection tab
SAFFI_table <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_1_FC_description")

## define table for Processing thermal
PTE_DES <- read_excel("HI_Microbial_hazards_identification_processed_database.xlsx", sheet = "Step_2_PEdes", skip=2)
