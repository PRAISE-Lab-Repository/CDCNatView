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


# bmi <- readRDS("data/database1/bmi_tables/pre-pregnancy_diabetes.rds")
education <- readRDS("data/database1/education_tables/pre-pregnancy_diabetes.rds")




# chart3_2 <- 

# years 
years1 = c(2016, 2021)


# choices for risk factor  ----------------------------------------------------

risk_factor1 = list(`Pregnancy risk factor` = 
       list("Pre-pregnancy Diabetes" = "pre-pregnancy_diabetes", 
            "Gestational Diabetes" = "gestational_diabetes", 
            "Pre-pregnancy hypertension" = "pre-pregnancy_hypertension",
            "Gestational hypertension" = "gestational_hypertension",
            "Eclampsia" = "eclampsia",
            "Previous preterm birth" = "previous_preterm_birth",
            "Infertility treatment used" ="infertility_treatment",
            "Fertility enhancing drugs" = "fertility_enhancing_drugs",
            "Assistive reproductive technology"="assistive_reproductive_technology",
            "Previous cesarean delivery" ="previous_cesarean"),
     `Infections` = 
       list("Chlamydia"="chlamydia",
            "Gonorrhea"="gonorrhea",
            "Hepatitis B"="hepatitis_B",
            "Hepatitis C"="hepatitis_C",
            "Syphilis" = "syphilis"),
     `Maternal morbidities`=
       list("Admission to ICU"="admission_to_ICU",
            "Maternal Transfusion"="maternal_transfusion",
            "Perineal Laceration"="perineal_laceration",
            "Ruptured uterus" = "ruptured_uterus",
            "Unplanned hysterectomy"="unplanned_hysterectomy"),
     `Pregnancy Outcome` = 
       list("Fullterm birth" = "fullterm_birth",
            "Preterm birth" = "preterm_birth",
            "Extreme preterm birth" = "extreme_birth",
            "Severe preterm birth" = "severe_birth",
            "Moderate preterm birth"="moderate_birth",
            "Late preterm"= "late_preterm")
     )

reverse_map = list("pre-pregnancy_diabetes"="Pre-pregnancy Diabetes",
                   "gestational_diabetes"="Gestational Diabetes")

code_map = list("pre-pregnancy_diabetes"="D149.V74",
                "gestational_diabetes"="D149.V75")


