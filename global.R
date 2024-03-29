library(shiny)
library(shinydashboard)
library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(extrafont)
library(highcharter)
library(corrplot)
library(DT)
library(epitools)
loadfonts()


## maybe not needed 
library(shinyjs)
library(shinycssloaders)
library(shinyjqui)
library(shinyBS)
library(shinyWidgets)
library(rintrojs)

source("wonderapi.R")




# setting for the map 
set_urbn_defaults(style = "map")

# use this for custom breakline 
linebreaks <- function(n){HTML(strrep(br(), n))}

# colors use for the map
colors <- c('#0073C2FF', '#EFC000FF', '#868686FF', '#CD534CFF', '#7AA6DCFF',
                       '#003C67FF', '#8F7700FF', '#3B3b3BFF', '#A73030FF', '#4A6990FF')
                       

# read the data.
# bmi <- read_csv("data/bmi.csv")
# race <- read_csv("data/race.csv")
# weight_gain <- read_csv("data/weight_gain.csv")
# 
# hm <- read_csv("data/heatmap.csv")
# M <- cor(hm)

# Get the states data 
# states_df <- read_csv("data/states_database1.csv")
# states_sf <- get_urbn_map("states", sf = TRUE)
demo_df <- read_csv("data/demograph_database1.csv")
diabetes_long <- read_csv("data/diabetes_long2.csv")
diabetes_long$`% of Total Births` <- as.numeric(sub("%","", diabetes_long$`% of Total Births`))

# eclampsia <- read_csv("data/eclampsia_long.csv")
# eclampsia$`% of Total Births` <- as.numeric(sub("%","", eclampsia$`% of Total Births`))
# 
# 
# chart2 <- readRDS("data/database1/long_tables/pre-pregnancy_diabetes.rds")
# chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)
# 
# bmi <- readRDS("data/database1/bmi_tables/pre-pregnancy_diabetes.rds")
# education <- readRDS("data/database1/education_tables/pre-pregnancy_diabetes.rds")
# race <- readRDS("data/database1/race_tables/pre-pregnancy_diabetes.rds")
# wtgain <- readRDS("data/database1/wtgain_tables/pre-pregnancy_diabetes.rds")
# delivery <- readRDS("data/database1/delivery_tables/pre-pregnancy_diabetes.rds")
# gestation <- readRDS("data/database1/gestation_tables/pre-pregnancy_diabetes.rds")
# care <- readRDS("data/database1/care_tables/pre-pregnancy_diabetes.rds")
# lastpreg <- readRDS("data/database1/lastpreg_tables/pre-pregnancy_diabetes.rds")


# years 
years1 = c(2016, 2021)
years2 = c(2007, 2021)
years3 = c(2003, 2006)
years4 = c(1995, 2002)
years5 = c(1995, 2021)

# choices for risk factor  ----------------------------------------------------


state_df = read_csv("data/state_example.csv")
state_list = state_df$`state_name`


### 2016-2021 ##################################################################
risk_factor1 = list(`Pregnancy risk factor` = 
       list("Pre-pregnancy Diabetes" = "pre-pregnancy_diabetes", 
            "Gestational Diabetes" = "gestational_diabetes", 
            "Pre-pregnancy Hypertension" = "pre-pregnancy_hypertension",
            "Gestational Hypertension" = "gestational_hypertension",
            "Eclampsia" = "eclampsia",
            "Previous Preterm Birth" = "previous_preterm_birth",
            "Infertility Treatment Used" ="infertility_treatment",
            "Fertility Enhancing Drugs" = "fertility_enhancing_drugs",
            "Assistive Reproductive Technology"="assistive_reproductive_technology",
            "Previous Cesarean Delivery" ="previous_cesarean"),
     `Infections` = 
       list("Chlamydia"="chlamydia",
            "Gonorrhea"="gonorrhea",
            "Hepatitis B"="hepatitis_B",
            "Hepatitis C"="hepatitis_C",
            "Syphilis" = "syphilis"),
     `Maternal morbidities`=
       list("Admission to Intensive Care Unit"="admission_to_ICU",
            "Maternal Transfusion"="maternal_transfusion",
            "Perineal Laceration"="perineal_laceration",
            "Ruptured Uterus" = "ruptured_uterus",
            "Unplanned Hysterectomy"="unplanned_hysterectomy"),
     `Pregnancy Outcome` = 
       list("Fullterm birth" = "fullterm_birth",
            "Preterm birth" = "preterm_birth",
            "Extreme preterm birth" = "extreme_birth",
            "Severe preterm birth" = "severe_birth",
            "Moderate and Late preterm birth"="moderate_birth")
     )


reverse_map = list("pre-pregnancy_diabetes"="Pre-pregnancy Diabetes",
                   "gestational_diabetes"="Gestational Diabetes",
                   "pre-pregnancy_hypertension"="Pre-pregnancy Hypertension", 
                   "gestational_hypertension"= "Gestational Hypertension",
                   "eclampsia" = "Eclampsia" ,
                   "previous_preterm_birth" = "Previous Preterm Birth",
                   "infertility_treatment" = "Infertility Treatment Used",
                   "fertility_enhancing_drugs"= "Fertility Enhancing Drugs",
                   "assistive_reproductive_technology"="Assistive Reproductive Technology",
                   "previous_cesarean"= "Previous Cesarean Delivery",
                   "chlamydia"="Chlamydia",
                   "gonorrhea"="Gonorrhea",
                   "hepatitis_B"="Hepatitis B",
                   "hepatitis_C"= "Hepatitis C",
                   "syphilis"="Syphilis", 
                   "admission_to_ICU" = "Admission to Intensive Care Unit",
                   "maternal_transfusion"="Maternal Transfusion",
                   "perineal_laceration"= "Perineal Laceration",
                   "ruptured_uterus"="Ruptured Uterus",
                   "unplanned_hysterectomy"="Unplanned Hysterectomy",
                   "fullterm_birth"="Fullterm birth",
                   "preterm_birth"="Preterm birth",
                   "extreme_birth"="Extreme preterm birth",
                   "severe_birth"="Severe preterm birth",
                   "moderate_birth"="Moderate and Late preterm birth")

code_map = list("pre-pregnancy_diabetes"="D149.V74",
                "gestational_diabetes"="D149.V75",
                "pre-pregnancy_hypertension"="D149.V16",
                "gestational_hypertension" = "D149.V17",
                "eclampsia"="D149.V18",
                "previous_preterm_birth"="D149.V76",
                "infertility_treatment"="D149.V77",
                "fertility_enhancing_drugs"="D149.V78",
                "assistive_reproductive_technology"="D149.V79",
                "previous_cesarean"="D149.V80",
                "chlamydia"="D149.V85",
                "gonorrhea"="D149.V83",
                "hepatitis_B"="D149.V86",
                "hepatitis_C"="D149.V87",
                "admission_to_ICU"="D149.V106",
                "maternal_transfusion"="D149.V102",
                "perineal_laceration"="D149.V103",
                "ruptured_uterus"="D149.V104",
                "unplanned_hysterectomy"="D149.V105",
                "oe_gesation_10"="D149.V32")

demo_map = list("bmi"="D149.V71",
                "race"="D149.V42",
                "wtgain"="D149.V73",
                "delivery"="D149.V99",
                "gestation"="D149.V32",
                "care"="D149.V63",
                "lastpreg"="D149.V62")



### 2007 - 2021 ################################################################
risk_factor2 = list(`Pregnancy risk factor`= 
                      list("Chronic Hypertension"="chronic_htn",
                           "Diabetes"="diabetes",
                           "Pregnancy-associated Hypertension"="preg_htn",
                           "Eclampsia" = "eclampsia"),
                    `Pregnancy Outcome` = 
                      list("Fullterm birth" = "fullterm_birth",
                           "Preterm birth" = "preterm_birth",
                           "Extreme preterm birth" = "extreme_birth",
                           "Severe preterm birth" = "severe_birth",
                           "Moderate and Late preterm birth" = "moderate_birth")                    
)

reverse_map_2 = list("chronic_htn"="Chronic Hypertension",
                     "diabetes"= "Diabetes",
                     "preg_htn"="Pregnancy-associated Hypertension",
                     "eclampsia"="Eclampsia",
                     "fullterm_birth"="Fullterm birth",
                     "preterm_birth"="Preterm birth",
                     "extreme_birth"="Extreme preterm birth",
                     "severe_birth"="Severe preterm birth",
                     "moderate_birth"="Moderate and Late preterm birth")                     

code_map_2 <- list("chronic_htn"="D66.V16",
                   "diabetes"="D66.V14",
                   "preg_htn"="D66.V17",
                   "diabetes" = "D66.V14",
                   "eclampsia" = "D66.V18",
                   "oe_gesation_10"="D66.V32")


demo_map_2 <- list("race"="D66.V2", # Mother's Bridged Race
                   "delivery"="D66.V31",
                   "gestation"="D66.V32", 
                   "care"="D66.V8",
                   "education"="D66.V5",
                   "age"="D66.V1",
                   "tobacco_use"="D66.V10") # Age of Mother 9

### 2003 - 2006 ################################################################
risk_factor3 = list(`Pregnancy risk factor` = 
                      list("Anemia" = "anemia", 
                           "Cardiac Disease" = "cardiac_disease", 
                           "Chronic Hypertension" = "chronic_hypertension",
                           "Diabetes" = "diabetes",
                           "Eclampsia" = "eclampsia",
                           "Hydramnios/Oligohydramnios" = "hydramnios_oligohydramnios",
                           "Incompetent Cervix" = "incompetent_cervix",
                           "Lung Disease" = "lung_disease",
                           "Pregnancy-associated Hypertension"="pregnancy_associated_hypertension",
                           "Tobacco Use" ="tobacco_use"),
                    `Pregnancy Outcome` = 
                      list("Fullterm birth" = "fullterm_birth",
                           "Preterm birth" = "preterm_birth",
                           "Extreme preterm birth" = "extreme_birth",
                           "Severe preterm birth" = "severe_birth",
                           "Moderate and Late preterm birth" = "moderate_birth")
)

reverse_map_3 = list("anemia" = "Anemia",
                     "cardiac_disease" = "Cardiac Disease",
                     "chronic_hypertension" = "Chronic Hypertension", 
                     "diabetes" = "Diabetes",
                     "eclampsia" = "Eclampsia" ,
                     "hydramnios_oligohydramnios" = "Hydramnios/Oligohydramnios",
                     "incompetent_cervix" = "Incompetent Cervix",
                     "lung_disease" = "Lung Disease",
                     "pregnancy-associated_hypertension" = "Pregnancy-associated Hypertension",
                     "fullterm_birth" = "Fullterm birth",
                     "preterm_birth" = "Preterm birth",
                     "extreme_birth" = "Extreme preterm birth",
                     "severe_birth" = "Severe preterm birth",
                     "moderate_birth" = "Moderate and Late preterm birth")

code_map_3 <- list("anemia"="D27.V11",
                   "cardiac_disease"="D27.V12",
                   "chronic_hypertension"="D27.V16",
                   "diabetes" = "D27.V14",
                   "eclampsia" = "D27.V18",
                   "hydramnios_oligohydramnios" = "D27.V15",
                   "incompetent_cervix" = "D27.V19",
                   "lung_disease" = "D27.V13",
                   "pregnancy-associated_hypertension" = "D27.V17",
                   "lmp_gesation_age_10"="D27.V6")

demo_map_3 <- list("race"="D27.V2", # Mother's Bridged Race
                   "delivery"="D27.V31",
                   "gestation"="D27.V6", # LMP Gestational Age 10
                   "care"="D27.V8",
                   "education"="D27.V5",
                   "age"="D27.V1",
                   "tobacco_use"="D27.V10") # Age of Mother 9

### 2002 - 1995 ################################################################
risk_factor4 = list(`Pregnancy risk factor` = 
                      list("Anemia" = "anemia", 
                           "Cardiac Disease" = "cardiac_disease", 
                           "Chronic Hypertension" = "chronic_hypertension",
                           "Diabetes" = "diabetes",
                           "Eclampsia" = "eclampsia",
                           "Hydramnios/Oligohydramnios" = "hydramnios_oligohydramnios",
                           "Incompetent Cervix" = "incompetent_cervix",
                           "Lung Disease" = "lung_disease",
                           "Pregnancy-associated Hypertension"="pregnancy_associated_hypertension",
                           "Tobacco Use" ="tobacco_use"),
                    `Pregnancy Outcome` = 
                      list("Fullterm birth" = "fullterm_birth",
                           "Preterm birth" = "preterm_birth",
                           "Extreme preterm birth" = "extreme_birth",
                           "Severe preterm birth" = "severe_birth",
                           "Moderate and Late preterm birth" = "moderate_birth")
)

risk_factor5 = list("Preterm birth" = "preterm_birth")

reverse_map_4 = list("anemia" = "Anemia",
                     "cardiac_disease" = "Cardiac Disease",
                     "chronic_hypertension" = "Chronic Hypertension", 
                     "diabetes" = "Diabetes",
                     "eclampsia" = "Eclampsia" ,
                     "hydramnios_oligohydramnios" = "Hydramnios/Oligohydramnios",
                     "incompetent_cervix" = "Incompetent Cervix",
                     "lung_disease" = "Lung Disease",
                     "pregnancy-associated_hypertension" = "Pregnancy-associated Hypertension",
                     "tobacco_use" = "Tobacco Use",
                     "fullterm_birth" = "Fullterm birth",
                     "preterm_birth" = "Preterm birth",
                     "extreme_birth" = "Extreme preterm birth",
                     "severe_birth" = "Severe preterm birth",
                     "moderate_birth" = "Moderate and Late preterm birth")

code_map_4 <- list("anemia"="D10.V11",
                   "cardiac_disease"="D10.V12",
                   "chronic_hypertension"="D10.V16",
                   "diabetes" = "D10.V14",
                   "eclampsia" = "D10.V18",
                   "hydramnios_oligohydramnios" = "D10.V15",
                   "incompetent_cervix" = "D10.V19",
                   "lung_disease" = "D10.V13",
                   "pregnancy-associated_hypertension" = "D10.V17",
                   "oe_gesation_10"="D10.V6")

demo_map_4 <- list("race"="D10.V2", # Mother's Race
                   "gestation"="D10.V6",
                   "care"="D10.V8",
                   "education"="D10.V5",
                   "age"="D10.V1",  # Age of Mother
                   "tobacco_use" = "D10.V10")

get_preg_outcome_data <- function(database, gestational_table) {
  sprintf("%s.rds", database)
  if (database == 1) {
    feat = "OE Gestational Age Recode 10"  
  } else {
    feat = "OE Gestational Age 10"  
  }
  

  gestational_table <- gestational_table %>% mutate("Preterm birth"=  case_when(!!as.symbol(feat)== "Under 20 weeks" ~ "Yes",
                                                                                !!as.symbol(feat)== "20 - 27 weeks" ~ "Yes", 
                                                                                !!as.symbol(feat)== "28 - 31 weeks" ~ "Yes", 
                                                                                !!as.symbol(feat)== "32 - 35 weeks" ~ "Yes", 
                                                                                !!as.symbol(feat)== "36 weeks" ~ "Yes", 
                                                                                !!as.symbol(feat)== "37 - 39 weeks" ~ "No", 
                                                                                !!as.symbol(feat)== "40 weeks" ~ "No",
                                                                                !!as.symbol(feat)== "41 weeks" ~ "No", 
                                                                                !!as.symbol(feat)== "42 weeks or more" ~ "No", 
                                                                                !!as.symbol(feat)== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Fullterm birth"=  case_when(!!as.symbol(feat)== "Under 20 weeks" ~ "No",
                                                                                 !!as.symbol(feat)== "20 - 27 weeks" ~ "No", 
                                                                                 !!as.symbol(feat)== "28 - 31 weeks" ~ "No", 
                                                                                 !!as.symbol(feat)== "32 - 35 weeks" ~ "No", 
                                                                                 !!as.symbol(feat)== "36 weeks" ~ "No", 
                                                                                 !!as.symbol(feat)== "37 - 39 weeks" ~ "Yes", 
                                                                                 !!as.symbol(feat)== "40 weeks" ~ "Yes",
                                                                                 !!as.symbol(feat)== "41 weeks" ~ "Yes", 
                                                                                 !!as.symbol(feat)== "42 weeks or more" ~ "Yes", 
                                                                                 !!as.symbol(feat)== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Extreme preterm birth"=  case_when(!!as.symbol(feat)== "Under 20 weeks" ~ "Yes",
                                                                                        !!as.symbol(feat)== "20 - 27 weeks" ~ "Yse", 
                                                                                        !!as.symbol(feat)== "28 - 31 weeks" ~ "No", 
                                                                                        !!as.symbol(feat)== "32 - 35 weeks" ~ "No", 
                                                                                        !!as.symbol(feat)== "36 weeks" ~ "No", 
                                                                                        !!as.symbol(feat)== "37 - 39 weeks" ~ "No", 
                                                                                        !!as.symbol(feat)== "40 weeks" ~ "No",
                                                                                        !!as.symbol(feat)== "41 weeks" ~ "No", 
                                                                                        !!as.symbol(feat)== "42 weeks or more" ~ "No", 
                                                                                        !!as.symbol(feat)== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Severe preterm birth"=  case_when(!!as.symbol(feat)== "Under 20 weeks" ~ "No",
                                                                                       !!as.symbol(feat)== "20 - 27 weeks" ~ "No", 
                                                                                       !!as.symbol(feat)== "28 - 31 weeks" ~ "Yes", 
                                                                                       !!as.symbol(feat)== "32 - 35 weeks" ~ "No", 
                                                                                       !!as.symbol(feat)== "36 weeks" ~ "No", 
                                                                                       !!as.symbol(feat)== "37 - 39 weeks" ~ "No", 
                                                                                       !!as.symbol(feat)== "40 weeks" ~ "No",
                                                                                       !!as.symbol(feat)== "41 weeks" ~ "No", 
                                                                                       !!as.symbol(feat)== "42 weeks or more" ~ "No", 
                                                                                       !!as.symbol(feat)== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Moderate and Late preterm birth" =  case_when(!!as.symbol(feat)== "Under 20 weeks" ~ "No",
                                                                                                   !!as.symbol(feat)== "20 - 27 weeks" ~ "No", 
                                                                                                   !!as.symbol(feat)== "28 - 31 weeks" ~ "No", 
                                                                                                   !!as.symbol(feat)== "32 - 35 weeks" ~ "Yes", 
                                                                                                   !!as.symbol(feat)== "36 weeks" ~ "Yes", 
                                                                                                   !!as.symbol(feat)== "37 - 39 weeks" ~ "No", 
                                                                                                   !!as.symbol(feat)== "40 weeks" ~ "No",
                                                                                                   !!as.symbol(feat)== "41 weeks" ~ "No", 
                                                                                                   !!as.symbol(feat)== "42 weeks or more" ~ "No", 
                                                                                                   !!as.symbol(feat)== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
}

#condition <- get_condition(risk_factor1, "pre-pregnancy_diabetes")
#condition <- get_condition(risk_factor2, "chronic_htn")


