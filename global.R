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
states_df <- read_csv("data/states_database1.csv")
states_sf <- get_urbn_map("states", sf = TRUE)
demo_df <- read_csv("data/demograph_database1.csv")
diabetes_long <- read_csv("data/diabetes_long2.csv")
diabetes_long$`% of Total Births` <- as.numeric(sub("%","", diabetes_long$`% of Total Births`))

eclampsia <- read_csv("data/eclampsia_long.csv")
eclampsia$`% of Total Births` <- as.numeric(sub("%","", eclampsia$`% of Total Births`))


chart2 <- readRDS("data/database1/long_tables/pre-pregnancy_diabetes.rds")
chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)

bmi <- readRDS("data/database1/bmi_tables/pre-pregnancy_diabetes.rds")
education <- readRDS("data/database1/education_tables/pre-pregnancy_diabetes.rds")
race <- readRDS("data/database1/race_tables/pre-pregnancy_diabetes.rds")
wtgain <- readRDS("data/database1/wtgain_tables/pre-pregnancy_diabetes.rds")
delivery <- readRDS("data/database1/delivery_tables/pre-pregnancy_diabetes.rds")
gestation <- readRDS("data/database1/gestation_tables/pre-pregnancy_diabetes.rds")
care <- readRDS("data/database1/care_tables/pre-pregnancy_diabetes.rds")
lastpreg <- readRDS("data/database1/lastpreg_tables/pre-pregnancy_diabetes.rds")



# chart3_2 <- 

# years 
years1 = c(2016, 2021)


# choices for risk factor  ----------------------------------------------------

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
                   "preterm_birth"="Preterm birth")

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


get_preg_outcome_data <- function(gestational_table) {
  gestational_table <- gestational_table %>% mutate("Preterm birth"=  case_when(`OE Gestational Age Recode 10`== "Under 20 weeks" ~ "Yes",
                                                                                `OE Gestational Age Recode 10`== "20 - 27 weeks" ~ "Yes", 
                                                                                `OE Gestational Age Recode 10`== "28 - 31 weeks" ~ "Yes", 
                                                                                `OE Gestational Age Recode 10`== "32 - 35 weeks" ~ "Yes", 
                                                                                `OE Gestational Age Recode 10`== "36 weeks" ~ "Yes", 
                                                                                `OE Gestational Age Recode 10`== "37 - 39 weeks" ~ "No", 
                                                                                `OE Gestational Age Recode 10`== "40 weeks" ~ "No",
                                                                                `OE Gestational Age Recode 10`== "41 weeks" ~ "No", 
                                                                                `OE Gestational Age Recode 10`== "42 weeks or more" ~ "No", 
                                                                                `OE Gestational Age Recode 10`== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Fullterm birth"=  case_when(`OE Gestational Age Recode 10`== "Under 20 weeks" ~ "No",
                                                                                 `OE Gestational Age Recode 10`== "20 - 27 weeks" ~ "No", 
                                                                                 `OE Gestational Age Recode 10`== "28 - 31 weeks" ~ "No", 
                                                                                 `OE Gestational Age Recode 10`== "32 - 35 weeks" ~ "No", 
                                                                                 `OE Gestational Age Recode 10`== "36 weeks" ~ "No", 
                                                                                 `OE Gestational Age Recode 10`== "37 - 39 weeks" ~ "Yes", 
                                                                                 `OE Gestational Age Recode 10`== "40 weeks" ~ "Yes",
                                                                                 `OE Gestational Age Recode 10`== "41 weeks" ~ "Yes", 
                                                                                 `OE Gestational Age Recode 10`== "42 weeks or more" ~ "Yes", 
                                                                                 `OE Gestational Age Recode 10`== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Extreme preterm birth"=  case_when(`OE Gestational Age Recode 10`== "Under 20 weeks" ~ "Yes",
                                                                                        `OE Gestational Age Recode 10`== "20 - 27 weeks" ~ "Yse", 
                                                                                        `OE Gestational Age Recode 10`== "28 - 31 weeks" ~ "No", 
                                                                                        `OE Gestational Age Recode 10`== "32 - 35 weeks" ~ "No", 
                                                                                        `OE Gestational Age Recode 10`== "36 weeks" ~ "No", 
                                                                                        `OE Gestational Age Recode 10`== "37 - 39 weeks" ~ "No", 
                                                                                        `OE Gestational Age Recode 10`== "40 weeks" ~ "No",
                                                                                        `OE Gestational Age Recode 10`== "41 weeks" ~ "No", 
                                                                                        `OE Gestational Age Recode 10`== "42 weeks or more" ~ "No", 
                                                                                        `OE Gestational Age Recode 10`== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Severe preterm birth"=  case_when(`OE Gestational Age Recode 10`== "Under 20 weeks" ~ "No",
                                                                                       `OE Gestational Age Recode 10`== "20 - 27 weeks" ~ "No", 
                                                                                       `OE Gestational Age Recode 10`== "28 - 31 weeks" ~ "Yes", 
                                                                                       `OE Gestational Age Recode 10`== "32 - 35 weeks" ~ "No", 
                                                                                       `OE Gestational Age Recode 10`== "36 weeks" ~ "No", 
                                                                                       `OE Gestational Age Recode 10`== "37 - 39 weeks" ~ "No", 
                                                                                       `OE Gestational Age Recode 10`== "40 weeks" ~ "No",
                                                                                       `OE Gestational Age Recode 10`== "41 weeks" ~ "No", 
                                                                                       `OE Gestational Age Recode 10`== "42 weeks or more" ~ "No", 
                                                                                       `OE Gestational Age Recode 10`== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  gestational_table <- gestational_table %>% mutate("Moderate and Late preterm birth" =  case_when(`OE Gestational Age Recode 10`== "Under 20 weeks" ~ "No",
                                                                                                   `OE Gestational Age Recode 10`== "20 - 27 weeks" ~ "No", 
                                                                                                   `OE Gestational Age Recode 10`== "28 - 31 weeks" ~ "No", 
                                                                                                   `OE Gestational Age Recode 10`== "32 - 35 weeks" ~ "Yes", 
                                                                                                   `OE Gestational Age Recode 10`== "36 weeks" ~ "Yes", 
                                                                                                   `OE Gestational Age Recode 10`== "37 - 39 weeks" ~ "No", 
                                                                                                   `OE Gestational Age Recode 10`== "40 weeks" ~ "No",
                                                                                                   `OE Gestational Age Recode 10`== "41 weeks" ~ "No", 
                                                                                                   `OE Gestational Age Recode 10`== "42 weeks or more" ~ "No", 
                                                                                                   `OE Gestational Age Recode 10`== "Unknown or Not Stated" ~ "Unknown or Not Stated"))
  
  
  
}


