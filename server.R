# Define server logic required to draw a histogram

# set options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)


server <- function(input, output, session) {


  
  # REACTIVES ------------------------------------------------------------------
  
  # graph 1  states ------------------------------------------------------------
  # counts
  update_state_graph2 <- reactive({
    input$confirm
    
    
    isolate({
      req(input$riskInput, input$yearInput)
      states_df <- read_csv("data/states_database1.csv")
      
      state_grph <- states_df %>% filter(condition==input$riskInput)
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      state_grph <- state_grph %>% group_by(state_name, condition_status) %>% summarise(count=sum(counts))
      
      
      
      state_grph <- state_grph %>% 
        group_by(state_name) %>%
        mutate(per = count/sum(count))
      
      state_grph <- state_grph %>% filter( condition_status=="Yes")
      
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })
  
  update_state_graph2_database2 <- reactive({
    input$confirm
    
    
    isolate({
      req(input$riskInput, input$yearInput)

      states_df <- read_csv("data/states_database2.csv")
      
      state_grph <- states_df %>% filter(condition==condition)
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      state_grph <- state_grph %>% group_by(state_name, condition_status) %>% summarise(count=sum(counts))
      
      state_grph <- state_grph %>% 
        group_by(state_name) %>%
        mutate(per = count/sum(count))
      
      state_grph <- state_grph %>% filter( condition_status=="Yes")
      
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  update_state_graph2_database3 <- reactive({
    input$confirm
    
    
    isolate({
      req(input$riskInput, input$yearInput)
      
      states_df <- read_csv("data/states_database3.csv")
      
      state_grph <- states_df %>% filter(condition==condition)
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      state_grph <- state_grph %>% group_by(state_name, condition_status) %>% summarise(count=sum(counts))
      
      state_grph <- state_grph %>% 
        group_by(state_name) %>%
        mutate(per = count/sum(count))
      
      state_grph <- state_grph %>% filter( condition_status=="Yes")
      
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  update_state_graph2_database4 <- reactive({
    input$confirm
    
    
    isolate({
      req(input$riskInput, input$yearInput)
      
      states_df <- read_csv("data/states_database4.csv")
      
      state_grph <- states_df %>% filter(condition==condition)
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      state_grph <- state_grph %>% group_by(state_name, condition_status) %>% summarise(count=sum(counts))
      
      state_grph <- state_grph %>% 
        group_by(state_name) %>%
        mutate(per = count/sum(count))
      
      state_grph <- state_grph %>% filter( condition_status=="Yes")
      
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  # percentage
  update_state_graph <- reactive({
    input$confirm 
    

    isolate({
      req(input$riskInput, input$yearInput)
      
      states_df <- read_csv("data/states_database1.csv")
      
      state_grph <- states_df %>% filter(condition==input$riskInput, condition_status=="Yes")
      
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      
      state_grph <- state_grph %>% group_by(state_name)
      state_grph <- summarise(state_grph, count = sum(counts)) 
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  update_state_graph_database2 <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$yearInput)
      states_df <- read_csv("data/states_database2.csv")
      
      state_grph <- states_df %>% filter(condition==input$riskInput, condition_status=="Yes")
      
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      
      state_grph <- state_grph %>% group_by(state_name)
      state_grph <- summarise(state_grph, count = sum(counts)) 
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  update_state_graph_database3 <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$yearInput)
      states_df <- read_csv("data/states_database3.csv")
      
      state_grph <- states_df %>% filter(condition==input$riskInput, condition_status=="Yes")
      
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      
      state_grph <- state_grph %>% group_by(state_name)
      state_grph <- summarise(state_grph, count = sum(counts)) 
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  update_state_graph_database4 <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$yearInput)
      states_df <- read_csv("data/states_database4.csv")
      
      state_grph <- states_df %>% filter(condition==input$riskInput, condition_status=="Yes")
      
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      
      state_grph <- state_grph %>% group_by(state_name)
      state_grph <- summarise(state_grph, count = sum(counts)) 
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  # graph 2 long plots ---------------------------------------------------------
  update_long_graph <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$yearInput)
      condition <- input$riskInput
      file_path <- sprintf("data/database1/long_tables/%s.rds", condition)

      if (condition %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/long_tables/%s.rds", "oe_gesation_10")
        
      }
      
      chart2 <<- readRDS(file_path)
      chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)
      chart2 <- chart2  %>% filter(chart2$`Average OE Gestational Age (weeks)` != "Not Applicable") 
      chart2$`Average OE Gestational Age (weeks)` <- as.numeric(chart2$`Average OE Gestational Age (weeks)` ) 
      
      chart2 <- chart2  %>% filter(chart2$`Average LMP Gestational Age (weeks)` != "Not Applicable") 
      chart2$`Average LMP Gestational Age (weeks)` <- as.numeric(chart2$`Average LMP Gestational Age (weeks)` ) 
      
      
      if (condition %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        chart2 <- get_preg_outcome_data(1, chart2)

        cc <- chart2 %>% group_by(!!as.symbol(reverse_map[[condition]]), `Year`)
        
        chart2 <- summarise(cc, Births=sum(Births), 
                  `% of Total Births` = sum(`% of Total Births`),
                  `Average Age of Mother (years)` = mean(`Average Age of Mother (years)`),
                  `Average OE Gestational Age (weeks)` = mean(`Average OE Gestational Age (weeks)`),
                  `Average LMP Gestational Age (weeks)` = mean(`Average LMP Gestational Age (weeks)`),
                  `Average Birth Weight (grams)` = mean(`Average Birth Weight (grams)`),
                  `Average Pre-pregnancy BMI` = mean(`Average Pre-pregnancy BMI`)
                  )
      }      
      

      long_grph <- chart2 %>% filter(!!as.symbol(reverse_map[[condition]])=="Yes")
      long_grph <- long_grph %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))
    })
  })

  update_long_graph_database2 <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$riskInput)
      condition <- input$riskInput
      file_path <- sprintf("data/database2/long_tables/%s.rds", condition)
      
      if (condition %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database2/long_tables/%s.rds", "oe_gesation_10")
        
      }
      
      chart2 <<- readRDS(file_path)
      chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)
      
      chart2 <- chart2  %>% filter(chart2$`Average OE Gestational Age` != "Not Applicable") 
      chart2$`Average OE Gestational Age` <- as.numeric(chart2$`Average OE Gestational Age` ) 
      
      chart2 <- chart2  %>% filter(chart2$`Average LMP Gestational Age` != "Not Applicable") 
      chart2$`Average LMP Gestational Age` <- as.numeric(chart2$`Average LMP Gestational Age` )
      
      print(chart2)
      if (condition %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        chart2 <- get_preg_outcome_data(2, chart2)
        
        cc <- chart2 %>% group_by(!!as.symbol(reverse_map_2[[condition]]), `Year`)
        
        chart2 <- summarise(cc, Births=sum(Births), 
                            `% of Total Births` = sum(`% of Total Births`),
                            `Average Age of Mother` = mean(`Average Age of Mother`),
                            `Average OE Gestational Age` = mean(`Average OE Gestational Age`),
                            `Average LMP Gestational Age` = mean(`Average LMP Gestational Age`),
                            `Average Birth Weight` = mean(`Average Birth Weight`)
        )
      }      
      
      
      long_grph <- chart2 %>% filter(!!as.symbol(reverse_map_2[[condition]])=="Yes")
      long_grph <- long_grph %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))
    })
  })
  
  update_long_graph_database3 <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$yearInput)
      condition <- input$riskInput
      file_path <- sprintf("data/database3/long_tables/%s.rds", condition)
      
      if (condition %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database3/long_tables/%s.rds", "oe_gesation_10")
        
      }
      
      chart2 <<- readRDS(file_path)
      chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)
      
      chart2 <- chart2  %>% filter(chart2$`Average LMP Gestational Age` != "Not Applicable") 
      chart2$`Average LMP Gestational Age` <- as.numeric(chart2$`Average LMP Gestational Age` )
      
      if (condition %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        chart2 <- get_preg_outcome_data(3, chart2)
        
        cc <- chart2 %>% group_by(!!as.symbol(reverse_map_3[[condition]]), `Year`)
        
        chart2 <- summarise(cc, Births=sum(Births), 
                            `% of Total Births` = sum(`% of Total Births`),
                            `Average Age of Mother` = mean(`Average Age of Mother`),
                            `Average LMP Gestational Age` = mean(`Average LMP Gestational Age`),
                            `Average Birth Weight` = mean(`Average Birth Weight`)
        )
      }      
      
      long_grph <- chart2 %>% filter(!!as.symbol(reverse_map_3[[condition]])=="Yes")
      long_grph <- long_grph %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))
    })
  })
  
  update_long_graph_database4 <- reactive({
    input$confirm 
    
    isolate({
      req(input$riskInput, input$yearInput)
      condition <- input$riskInput
      
      file_path <- sprintf("data/database4/long_tables/%s.rds", condition)
      
      if (condition %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database4/long_tables/%s.rds", "oe_gesation_10")
        
      }
      
      chart2 <<- readRDS(file_path)
      
      if (condition %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        chart2 <- get_preg_outcome_data(4, chart2)
        
        cc <- chart2 %>% group_by(!!as.symbol(reverse_map_4[[condition]]), `Year`)
        
        chart2 <- summarise(cc, Births=sum(Births))
      }      
      
      long_grph <- chart2 %>% filter(!!as.symbol(reverse_map_4[[condition]])=="Yes")
      long_grph <- long_grph %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))
    })
  })
  
  # graph 3 plots ------------------------------------------------------------
  ### Graph 3: BMI #############################################################
  
  # Database 1
  update_bmi_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput)
      file_path <- sprintf("data/database1/bmi_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/bmi_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      bmi <<- readRDS(file_path)
      bmi <- bmi %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
     
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        bmi <- get_preg_outcome_data(1, bmi)
        
        cc <- bmi %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Year`,`Mother's Pre-pregnancy BMI`)
        
        bmi <- summarise(cc, Births=sum(Births))
        
      }
      
      bmi_sub <- bmi %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Pre-pregnancy BMI`)
      bmi_sub <- summarise(bmi_sub, count = sum(Births))
      bmi_sub <- filter(bmi_sub, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    })  
  })
  
  ### Graph 3: Race ############################################################
  
  # Database 1
  # graph 3: Race tab 
  update_race_graph <- reactive({
    input$confirm 
    isolate({
      condition <- get_condition(risk_factor1, "pre-pregnancy_diabetes")
      file_path <- sprintf("data/database1/race_tables/%s.rds", condition)
      if (condition %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/race_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      race <<- readRDS(file_path)
      race <- race %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (condition %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        race <- get_preg_outcome_data(1, race)
        
        cc <- race %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Single Race 6`)
        
        race <- summarise(cc, Births=sum(Births))
        
      }      
      
      race_sub <- race %>% group_by(!!as.symbol(reverse_map[[condition]]), `Mother's Single Race 6`)
      race_sub <- summarise(race_sub, count = sum(Births))
      # remove entries with missing or unknown condition
      race_sub <- filter(race_sub, !!as.symbol(reverse_map[[condition]]) != "Unknown or Not Stated")
    })  
  })
  
  # Database 2
  update_race_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(2, 'race', "Mother's Bridged Race", reverse_map_2)
    })  
  })
  
  # Database 3
  update_race_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(3, 'race', "Mother's Bridged Race", reverse_map_3)
    })  
  })
  
  # Database 4
  update_race_graph_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(4, 'race', "Mother's Race", reverse_map_4)
    })  
  })
  
  
  
  ### Graph 3: Weight Gain #####################################################
  update_wtgain_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      file_path <- sprintf("data/database1/wtgain_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/wtgain_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      wtgain <<- readRDS(file_path)
      wtgain <- wtgain %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        wtgain <- get_preg_outcome_data(1, wtgain)
        
        cc <- wtgain %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Weight Gain Recode`)
        
        wtgain <- summarise(cc, Births=sum(Births))
        
      }     
      
      wtgain_sub <- wtgain %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Weight Gain Recode`)
      wtgain_sub <- summarise(wtgain_sub, count = sum(Births))
      # remove entries with missing or unknown condition
      wtgain_sub <- filter(wtgain_sub, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")      
    })  
  })  
  
  # graph 3: Delivery method tab
  update_delivery_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      file_path <- sprintf("data/database1/delivery_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/delivery_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      delivery <<- readRDS(file_path)
      delivery <- delivery %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        delivery <- get_preg_outcome_data(1, delivery)
        
        cc <- delivery %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Final Route and Delivery Method`)
        
        delivery <- summarise(cc, Births=sum(Births))
        
      }     
      delivery_sub <- delivery %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Final Route and Delivery Method`)
      delivery_sub <- summarise(delivery_sub, count = sum(Births))
      delivery_sub <- filter(delivery_sub, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    })  
  })  

  update_delivery_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)

      graph3_update(2, 'delivery', 'Delivery Method', reverse_map_2)
      
    })  
  })  
  
  update_delivery_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      graph3_update(3, 'delivery', 'Delivery Method', reverse_map_3)
      
    })  
  })  
  
  
  # graph 3: gestational weeks 
  update_gestation_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      
      
      file_path <- sprintf("data/database1/gestation_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/gestation_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      gestation <<- readRDS(file_path)
      gestation <- gestation %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        gestation <- get_preg_outcome_data(1, gestation)
        
        cc <- gestation %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`,`OE Gestational Age Recode 10`)
        
        gestation <- summarise(cc, Births=sum(Births))
        
      }     
      
      gestation_sub <- gestation %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `OE Gestational Age Recode 10`)
      gestation_sub <- summarise(gestation_sub, count = sum(Births))
      gestation_sub <- filter(gestation_sub, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    })  
  })      

  update_gestation_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      graph3_update(2, 'gestation', 'OE Gestational Age 10', reverse_map_2)
    })  
  })      
  
  update_gestation_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      graph3_update(3, 'gestation', 'LMP Gestational Age 10', reverse_map_3)
    })  
  })   
  
  update_gestation_graph_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      graph3_update(4, 'gestation', 'Gestational Age at Birth', reverse_map_4)
    })  
  })  
  
  # graph 3: prenental care 
  update_care_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      file_path <- sprintf("data/database1/care_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/care_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      care <<- readRDS(file_path)
      care <- care %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        care <- get_preg_outcome_data(1, care)
        
        cc <- care %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`,`Trimester Prenatal Care Began`)
        
        care <- summarise(cc, Births=sum(Births))
        
      }     
      care_sub <- care %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Trimester Prenatal Care Began`)
      care_sub <- summarise(care_sub, count = sum(Births))
      # remove entries with missing or unknown condition
      care_sub <- filter(care_sub, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    })  
  })  
  
  update_care_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(2, 'care', 'Month Prenatal Care Began', reverse_map_2)
      
    })  
  })      
  
  update_care_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(3, 'care', 'Month Prenatal Care Began', reverse_map_3)
      
    })  
  })      
  
  update_care_graph_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(4, 'care', 'Month Prenatal Care Began', reverse_map_4)
      
    })  
  })      
  
  
  # graph 3: tobacco use 
  
  update_tobacco_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(2, 'tobacco_use', 'Tobacco Use', reverse_map_2)
      
    })  
  })      
  
  update_tobacco_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(3, 'tobacco_use', 'Tobacco Use', reverse_map_3)
      
    })  
  })      
  
  update_tobacco_graph_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(4, 'tobacco_use', 'Tobacco Use', reverse_map_4)
      
    })  
  })      
  
  update_lastpreg_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      lastpreg <<- readRDS(file_path)
      lastpreg <- lastpreg %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        lastpreg <- get_preg_outcome_data(1, lastpreg)
        
        cc <- lastpreg %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Interval of Last Pregnancy`)
        
        lastpreg <- summarise(cc, Births=sum(Births))
        
      }     
      lastpreg_sub <- lastpreg %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Interval of Last Pregnancy`)
      lastpreg_sub <- summarise(lastpreg_sub, count = sum(Births))
      # remove entries with missing or unknown condition
      lastpreg_sub <- filter(lastpreg_sub, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    })  
  })     
  
  # graph 3 education
  update_education_graph <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      
      file_path <- sprintf("data/database1/education_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/education_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      education <<- readRDS(file_path)
      education <- education %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        education <- get_preg_outcome_data(1, education)
        
        cc <- education %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`, `Mother's Education`)
        
        education <- summarise(cc, Births=sum(Births))
        
      }     
      
      edu <- education %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Education`)
      edu <- summarise(edu, count = sum(Births))
      
      # remove entries with missing or unknown condition
      edu <- filter(edu, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    })
  })
  
  update_education_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(2, 'education', "Mother's Education", reverse_map_2)
      
    })
  })
  
  update_education_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(3, 'education', "Mother's Education", reverse_map_3)
      
    })
  })
  
  update_education_graph_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(4, 'education', "Mother's Education", reverse_map_4)
      
    })
  })
  
  
  # graph 3 age 
  update_age_graph_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(2, 'age', "Age of Mother 9", reverse_map_2)
    })
  })
  
  update_age_graph_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(3, 'age', "Age of Mother 9", reverse_map_3)
    })
  })
  
  update_age_graph_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$riskInput, input$yearInput)
      graph3_update(4, 'age', "Age of Mother", reverse_map_4)
    })
  })
  
  # graph 4: odd ratio tables 
  
  update_bmi_oddratio <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(1, 'bmi', "Mother's Pre-pregnancy BMI", reverse_map , "Normal 18.5-24.9")
      
    })
    
  })
  
  
  update_race_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/race_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/race_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      race <<- readRDS(file_path)
      race <- race %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        race <- get_preg_outcome_data(1, race)
        
        cc <- race %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Single Race 6`)
        
        race <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- race %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Single Race 6`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      
      odds_sub <- odds_sub %>% arrange(match(`Mother's Single Race 6`, c("White", setdiff(c("White"), `Mother's Single Race 6`))))
      odds_sub[is.na(odds_sub)] = 0
      
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Mother's Single Race 6`
      
      print(odd_table)
      result <- epitab(odd_table, method="oddsratio")
      print(result)
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_race_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'race', "Mother's Bridged Race", reverse_map_2 , "White")
      
    })
    
  })
  
  update_race_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'race', "Mother's Bridged Race", reverse_map_3 , "White")
      
    })
    
  })
  
  update_race_oddratio_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(4, 'race', "Mother's Race", reverse_map_4 , "White")
      
    })
    
  })
  
  
  update_wtgain_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/wtgain_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/wtgain_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      wtgain <<- readRDS(file_path)
      wtgain <- wtgain %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        wtgain <- get_preg_outcome_data(1, wtgain)
        
        cc <- wtgain %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Weight Gain Recode`)
        
        wtgain <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- wtgain %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Weight Gain Recode`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`Mother's Weight Gain Recode`, c("21 to 30 pounds", setdiff(c("21 to 30 pounds"), `Mother's Weight Gain Recode`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Mother's Weight Gain Recode`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_delivery_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/delivery_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/delivery_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      delivery <<- readRDS(file_path)
      delivery <- delivery %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        delivery <- get_preg_outcome_data(1, delivery)
        
        cc <- delivery %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`,`Final Route and Delivery Method`)
        
        delivery <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- delivery %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Final Route and Delivery Method`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`Final Route and Delivery Method`, c("Spontaneous", setdiff(c("Spontaneous"), `Final Route and Delivery Method`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Final Route and Delivery Method`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })

  update_delivery_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'delivery', "Delivery Method", reverse_map_2 , "Vaginal")
      
    })
    
  })
  
  update_delivery_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'delivery', "Delivery Method", reverse_map_3 , "Vaginal")
      
    })
    
  })
  
  
  update_gestation_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/gestation_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/gestation_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      gestation <<- readRDS(file_path)
      gestation <- gestation %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        gestation <- get_preg_outcome_data(1, gestation)
        
        cc <- gestation %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`OE Gestational Age Recode 10`)
        
        gestation <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- gestation %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `OE Gestational Age Recode 10`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`OE Gestational Age Recode 10`, c("37 - 39 weeks", setdiff(c("37 - 39 weeks"), `OE Gestational Age Recode 10`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`OE Gestational Age Recode 10`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_gestation_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'gestation', 'OE Gestational Age 10', reverse_map_2 , "37 - 39 weeks")
      
    })
    
  })
  
  update_gestation_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'gestation', 'LMP Gestational Age 10', reverse_map_3 , "37 - 39 weeks")
      
    })
    
  })
  
  update_gestation_oddratio_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(4, 'gestation', 'Gestational Age at Birth', reverse_map_4 , "37 - 39 weeks")
      
    })
    
  })
  
  
  update_care_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/care_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/care_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      care <<- readRDS(file_path)
      care <- care %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        care <- get_preg_outcome_data(1, care)
        
        cc <- care %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`,`Trimester Prenatal Care Began`)
        
        care <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- care %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Trimester Prenatal Care Began`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`Trimester Prenatal Care Began`, c("White", setdiff(c("White"), `Trimester Prenatal Care Began`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Trimester Prenatal Care Began`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_care_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'care', 'Month Prenatal Care Began', reverse_map_2 , "1st month")
    })
    
  })
  
  update_care_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'care', 'Month Prenatal Care Began', reverse_map_3 , "1st month")
    })
    
  })
  
  update_care_oddratio_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(4, 'care', 'Month Prenatal Care Began', reverse_map_4 , "1st month")
    })
    
  })
  
  
  
  update_lastpreg_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      lastpreg <<- readRDS(file_path)
      lastpreg <- lastpreg %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        lastpreg <- get_preg_outcome_data(1, lastpreg)
        
        cc <- lastpreg %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Interval of Last Pregnancy`)
        
        lastpreg <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- lastpreg %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Interval of Last Pregnancy`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`Interval of Last Pregnancy`, c("72 months and over", setdiff(c("72 months and over"), `Interval of Last Pregnancy`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Interval of Last Pregnancy`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_education_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/education_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/education_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      education <<- readRDS(file_path)
      education <- education %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        education <- get_preg_outcome_data(1, education)
        
        cc <- education %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Education`)
        
        education <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- education %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Education`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`Mother's Education`, c("Bachelor's degree (BA, AB, BS)", setdiff(c("Bachelor's degree (BA, AB, BS)"), `Mother's Education`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Mother's Education`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_education_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'education', "Mother's Education", reverse_map_2 , "Bachelor's degree (BA, AB, BS)")
    })
    
  })
  
  update_tobacco_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'tobacco_use', 'Tobacco Use', reverse_map_2 , "No")
    })
    
  })
  
  update_age_oddratio_database2 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(2, 'age', "Age of Mother 9", reverse_map_2 , "20-24 years")
    })
    
  })
  
  update_education_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'education', "Mother's Education", reverse_map_3 , "Bachelor's degree (BA, AB, BS)")
    })
    
  })
  
  update_tobacco_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'tobacco_use', 'Tobacco Use', reverse_map_3 , "No")
    })
    
  })
  
  update_age_oddratio_database3 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(3, 'age', "Age of Mother 9", reverse_map_3 , "20-24 years")
    })
    
  })
  
  update_education_oddratio_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(4, 'education', "Mother's Education", reverse_map_4 , "Bachelor's degree (BA, AB, BS)")
    })
    
  })
  
  update_tobacco_oddratio_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(4, 'tobacco_use', 'Tobacco Use', reverse_map_4 , "No")
    })
    
  })
  
  update_age_oddratio_database4 <- reactive({
    input$confirm 
    isolate({
      req(input$yearInput, input$riskInput)
      graph4_update(4, 'age', "Age of Mother", reverse_map_4 , "20-24 years")
    })
    
  })
  # DEFINE OUTPUTS -------------------------------------------------------------
  
  # graph 1 -------------------------------------------------------------
  output$distPlot <- renderPlot({
    state_grph <- update_state_graph()
    plot_state(state_grph)
  })
  
  output$distPlot_database2 <- renderPlot({
    state_grph <- update_state_graph_database2()
    plot_state(state_grph)
  })
  
  output$distPlot_database3 <- renderPlot({
    state_grph <- update_state_graph_database3()
    plot_state(state_grph)
  })
  
  output$distPlot_database4 <- renderPlot({
    state_grph <- update_state_graph_database4()
    plot_state(state_grph)
  })
  
  output$statePerPlot <- renderPlot({
    state_grph <- update_state_graph2()
    plot_per_state(state_grph)
  })
  
  output$statePerPlot_database2 <- renderPlot({
    state_grph <- update_state_graph2_database2()
    plot_per_state(state_grph)
  })
  
  output$statePerPlot_database3 <- renderPlot({
    state_grph <- update_state_graph2_database3()
    plot_per_state(state_grph)
  })
  
  output$statePerPlot_database4 <- renderPlot({
    state_grph <- update_state_graph2_database4()
    plot_per_state(state_grph)
  })
  
  # graph 2 -------------------------------------------------------------
  output$longPlot <- renderHighchart({
    long_grph <- update_long_graph()
    get_long_plots(long_grph, "Births")
  })
  
  output$longPerPlot <- renderHighchart({
    long_grph <- update_long_graph()
    get_long_plots(long_grph, "% of Total Births")
  })
  
  output$longAgePlot <- renderHighchart({
    long_grph <- update_long_graph()
    get_long_plots(long_grph, "Average Age of Mother (years)")
    
  })
  
  output$longLMPPlot <- renderHighchart({
    long_grph <- update_long_graph()
    get_long_plots(long_grph, "Average LMP Gestational Age (weeks)")
    
  })
  
  output$longOEPlot <- renderHighchart({
    long_grph <- update_long_graph()
    get_long_plots(long_grph, "Average OE Gestational Age (weeks)")
    
  })
  
  output$longBirthWeightPlot <- renderHighchart({
    long_grph <- update_long_graph()
    get_long_plots(long_grph, "Average Birth Weight (grams)")
  })  
  
  output$longPlot_database2 <- renderHighchart({
    long_grph <- update_long_graph_database2()
    get_long_plots(long_grph, "Births")
  })
  
  output$longPlot_database3 <- renderHighchart({
    long_grph <- update_long_graph_database3()
    get_long_plots(long_grph, "Births")
  })
  
  output$longPlot_database4 <- renderHighchart({
    long_grph <- update_long_graph_database4()
    get_long_plots(long_grph, "Births")
  })
  
  output$longPerPlot_database2 <- renderHighchart({
    long_grph <- update_long_graph_database2()
    get_long_plots(long_grph, "% of Total Births")
  })
  
  output$longPerPlot_database3 <- renderHighchart({
    long_grph <- update_long_graph_database3()
    get_long_plots(long_grph, "% of Total Births")
  })
  
  output$longAgePlot_database2 <- renderHighchart({
    long_grph <- update_long_graph_database2()
    get_long_plots(long_grph, "Average Age of Mother")
    
  })
  
  output$longAgePlot_database3 <- renderHighchart({
    long_grph <- update_long_graph_database3()
    get_long_plots(long_grph, "Average Age of Mother")
    
  })
  
  output$longLMPPlot_database2 <- renderHighchart({
    long_grph <- update_long_graph_database2()
    get_long_plots(long_grph, "Average LMP Gestational Age")
    
  })
  
  output$longLMPPlot_database3 <- renderHighchart({
    long_grph <- update_long_graph_database3()
    get_long_plots(long_grph, "Average LMP Gestational Age")
    
  })
  
  output$longOEPlot_database2 <- renderHighchart({
    long_grph <- update_long_graph_database2()
    get_long_plots(long_grph, "Average OE Gestational Age")
    
  })
  
  output$longBirthWeightPlot_database2 <- renderHighchart({
    long_grph <- update_long_graph_database2()
    get_long_plots(long_grph, "Average Birth Weight")
  })   
  
  output$longBirthWeightPlot_database3 <- renderHighchart({
    long_grph <- update_long_graph_database3()
    get_long_plots(long_grph, "Average Birth Weight")
  })   
  
  # graph 3 -------------------------------------------------------------
  
  output$bmiPlot <- renderHighchart({
    bmi_sub <- update_bmi_graph()
    demoPlot(reverse_map, bmi_sub, "Mother's Pre-pregnancy BMI")
  })
  
  output$racePlot <- renderHighchart({
    req(input$riskInput)
    race_sub <- update_race_graph()
    demoPlot(reverse_map, race_sub, "Mother's Single Race 6")
  })  
  
  output$racePlot_database2 <- renderHighchart({
    race_sub <- update_race_graph_database2()
    demoPlot(reverse_map_2, race_sub, "Mother's Bridged Race")
  })
  
  output$racePlot_database3 <- renderHighchart({
    race_sub <- update_race_graph_database3()
    demoPlot(reverse_map_3, race_sub, "Mother's Bridged Race")
  })
  
  output$racePlot_database4 <- renderHighchart({
    race_sub <- update_race_graph_database4()
    demoPlot(reverse_map_4, race_sub, "Mother's Race")
  })
  
  output$weightGainPlot <- renderHighchart({
    weight_gain_sub <- update_wtgain_graph()
    demoPlot(reverse_map, weight_gain_sub, "Mother's Weight Gain Recode")
  })  
  
  output$educationPlot <- renderHighchart({
    edu <- update_education_graph()
    demoPlot(reverse_map, edu, "Mother's Education")
  })  
  
  output$educationPlot_database2 <- renderHighchart({
    edu <- update_education_graph_database2()
    demoPlot(reverse_map_2, edu, "Mother's Education")
  })    
  
  output$educationPlot_database3 <- renderHighchart({
    edu <- update_education_graph_database3()
    demoPlot(reverse_map_3, edu, "Mother's Education")
  })    
  
  output$educationPlot_database4 <- renderHighchart({
    edu <- update_education_graph_database4()
    demoPlot(reverse_map_4, edu, "Mother's Education")
  })    
  
  output$deliveryPlot <- renderHighchart({
    delivery_sub <- update_delivery_graph()
    demoPlot(reverse_map, delivery_sub, "Final Route and Delivery Method")
  })  
  
  output$deliveryPlot_database2 <- renderHighchart({
    delivery_sub <- update_delivery_graph_database2()
    demoPlot(reverse_map_2, delivery_sub, "Delivery Method")
  })  
  
  output$deliveryPlot_database3 <- renderHighchart({
    delivery_sub <- update_delivery_graph_database3()
    demoPlot(reverse_map_3, delivery_sub, "Delivery Method")
  })  
  
  output$gestationPlot <- renderHighchart({
    gestation_sub <- update_gestation_graph()
    demoPlot(reverse_map, gestation_sub, "OE Gestational Age Recode 10")
  })    
  
  output$gestationPlot_database2 <- renderHighchart({
    gestation_sub <- update_gestation_graph_database2()
    demoPlot(reverse_map_2, gestation_sub, "OE Gestational Age 10")
  })    
  
  output$gestationPlot_database3 <- renderHighchart({
    gestation_sub <- update_gestation_graph_database3()
    demoPlot(reverse_map_3, gestation_sub, "LMP Gestational Age 10")
  })    
  
  output$gestationPlot_database4 <- renderHighchart({
    gestation_sub <- update_gestation_graph_database4()
    demoPlot(reverse_map_4, gestation_sub, "Gestational Age at Birth")
  })   
  
  output$carePlot <- renderHighchart({
    care_sub <- update_care_graph()
    demoPlot(reverse_map, care_sub, "Trimester Prenatal Care Began")
  })      
  
  output$carePlot_database2 <- renderHighchart({
    care_sub <- update_care_graph_database2()
    demoPlot(reverse_map_2, care_sub, "Month Prenatal Care Began")
  })      
  
  output$agePlot_database2 <- renderHighchart({
    age_sub <- update_age_graph_database2()
    demoPlot(reverse_map_2, age_sub, "Age of Mother 9")
  })      
  
  output$tobaccoPlot_database2 <- renderHighchart({
    age_sub <- update_tobacco_graph_database2()
    demoPlot(reverse_map_2, age_sub, "Tobacco Use")
  })  
  
  output$carePlot_database3 <- renderHighchart({
    care_sub <- update_care_graph_database3()
    demoPlot(reverse_map_3, care_sub, "Month Prenatal Care Began")
  })      
  
  output$agePlot_database3 <- renderHighchart({
    age_sub <- update_age_graph_database3()
    demoPlot(reverse_map_3, age_sub, "Age of Mother 9")
  })      
  
  output$tobaccoPlot_database3 <- renderHighchart({
    age_sub <- update_tobacco_graph_database3()
    demoPlot(reverse_map_3, age_sub, "Tobacco Use")
  })  
  
  output$carePlot_database4 <- renderHighchart({
    care_sub <- update_care_graph_database4()
    demoPlot(reverse_map_4, care_sub, "Month Prenatal Care Began")
  })      
  
  output$agePlot_database4 <- renderHighchart({
    age_sub <- update_age_graph_database4()
    demoPlot(reverse_map_4, age_sub, "Age of Mother")
  })      
  
  output$tobaccoPlot_database4 <- renderHighchart({
    age_sub <- update_tobacco_graph_database4()
    demoPlot(reverse_map_4, age_sub, "Tobacco Use")
  })  
  
  output$lastpregPlot <- renderHighchart({
    lastpreg_sub <- update_lastpreg_graph()
    demoPlot(reverse_map, lastpreg_sub, "Interval of Last Pregnancy")
  })        
  
  
  # graph 4 -------------------------------------------------------------
  output$oddsPlot <- renderDataTable({
    bmi_table <- update_bmi_oddratio()

    datatable(bmi_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$raceOddsPlot <- renderDataTable({
    race_table <- update_race_oddratio()
    datatable(race_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$raceOddsPlot_database2 <- renderDataTable({
    race_table <- update_race_oddratio_database2()
    datatable(race_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$raceOddsPlot_database3 <- renderDataTable({
    race_table <- update_race_oddratio_database3()
    datatable(race_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$raceOddsPlot_database4 <- renderDataTable({
    race_table <- update_race_oddratio_database4()
    datatable(race_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$wtgainOddsPlot <- renderDataTable({
    wtgain_table <- update_wtgain_oddratio()
    datatable(wtgain_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$deliveryOddsPlot <- renderDataTable({
    delivery_table <- update_delivery_oddratio()
    datatable(delivery_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })

  output$deliveryOddsPlot_database2 <- renderDataTable({
    delivery_table <- update_delivery_oddratio_database2()
    datatable(delivery_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$deliveryOddsPlot_database3 <- renderDataTable({
    delivery_table <- update_delivery_oddratio_database3()
    datatable(delivery_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$gestationOddsPlot <- renderDataTable({
    gestation_table <- update_gestation_oddratio()
    datatable(gestation_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$gestationOddsPlot_database2 <- renderDataTable({
    gestation_table <- update_gestation_oddratio_database2()
    datatable(gestation_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })

  output$gestationOddsPlot_database3 <- renderDataTable({
    gestation_table <- update_gestation_oddratio_database3()
    datatable(gestation_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$gestationOddsPlot_database4 <- renderDataTable({
    gestation_table <- update_gestation_oddratio_database4()
    datatable(gestation_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  
  output$careOddsPlot <- renderDataTable({
    care_table <- update_care_oddratio()
    datatable(care_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$careOddsPlot_database2 <- renderDataTable({
    care_table <- update_care_oddratio_database2()
    datatable(care_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$careOddsPlot_database3 <- renderDataTable({
    care_table <- update_care_oddratio_database3()
    datatable(care_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$careOddsPlot_database4 <- renderDataTable({
    care_table <- update_care_oddratio_database4()
    datatable(care_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$lastpregOddsPlot <- renderDataTable({
    lastpreg_table <- update_lastpreg_oddratio()
    datatable(lastpreg_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$tobaccoOddsPlot_database2 <- renderDataTable({
    tobacco_table <- update_tobacco_oddratio_database2()
    datatable(tobacco_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })

  output$ageOddsPlot_database2 <- renderDataTable({
    age_table <- update_age_oddratio_database2()
    datatable(age_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$tobaccoOddsPlot_database3 <- renderDataTable({
    tobacco_table <- update_tobacco_oddratio_database3()
    datatable(tobacco_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$ageOddsPlot_database3 <- renderDataTable({
    age_table <- update_age_oddratio_database3()
    datatable(age_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$tobaccoOddsPlot_database4 <- renderDataTable({
    tobacco_table <- update_tobacco_oddratio_database4()
    datatable(tobacco_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$ageOddsPlot_database4 <- renderDataTable({
    age_table <- update_age_oddratio_database4()
    datatable(age_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$educationOddsPlot <- renderDataTable({
    edu_table <- update_education_oddratio()
    datatable(edu_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$educationOddsPlot_database2 <- renderDataTable({
    edu_table <- update_education_oddratio_database2()
    datatable(edu_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$educationOddsPlot_database3 <- renderDataTable({
    edu_table <- update_education_oddratio_database3()
    datatable(edu_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$educationOddsPlot_database4 <- renderDataTable({
    edu_table <- update_education_oddratio_database4()
    datatable(edu_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  
  # tabpanels for different panel ----------------------------------------------
  
  # panel 1: state graph
  output$box_pat <- renderUI({
    get_graph1('box_pat', "distPlot", "statePerPlot")
  })
  
  output$box_pat5 <- renderUI({
    get_graph1('box_pat', "distPlot_database2", "statePerPlot_database2")
  })
  
  output$box_pat9 <- renderUI({
    get_graph1('box_pat', "distPlot_database3", "statePerPlot_database3")
  })
  
  output$box_pat13 <- renderUI({
    get_graph1('box_pat', "distPlot_database4", "statePerPlot_database4")
  })
  
  # panel 2: longitudinal graphs 
  output$box_pat2 <- renderUI({
    get_graph2("box_pat2" , "longPlot", "longPerPlot", "longAgePlot", "longOEPlot", "longLMPPlot", "longBirthWeightPlot")
  })
  
  output$box_pat6 <- renderUI({
    get_graph2("box_pat6" , "longPlot_database2", "longPerPlot_database2", 
               "longAgePlot_database2", "longOEPlot_database2", "longLMPPlot_database2", 
               "longBirthWeightPlot_database2")
  })
  
  output$box_pat10 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat10",
        width = NULL,
        height = 100,
        tabPanel(
          title = "Cases",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longPlot_database3"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "Percent of Total Births",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longPerPlot_database3"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "Age",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longAgePlot_database3"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "LMP Gestational Age",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longLMPPlot_database3"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "Birth Weight",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longBirthWeightPlot_database3"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
      )
    )
  })
  
  output$box_pat14 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat10",
        width = NULL,
        height = 100,
        tabPanel(
          title = "Cases",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longPlot_database4"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        )
      )
    )
  })
  
  
  # panel 3: category graphs 
  output$box_pat3 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat3",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "BMI",
          fluidRow(
            column(12, highchartOutput("bmiPlot")),
          ),
        ), 
        tabPanel(
          title = "Race",
          withSpinner(
            highchartOutput("racePlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Weight Gain",
          withSpinner(
            highchartOutput("weightGainPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Delivery Method",
          withSpinner(
            highchartOutput("deliveryPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            highchartOutput("gestationPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            highchartOutput("carePlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        tabPanel(
          title = "Interval of Last Pregnancy",
          withSpinner(
            highchartOutput("lastpregPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )    
        ),
        tabPanel(title = "Education",
                 withSpinner(
                   highchartOutput("educationPlot"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        )
        
      )
    )
  })
  
  output$box_pat7 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat7",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "Race",
          withSpinner(
            highchartOutput("racePlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Delivery Method",
          withSpinner(
            highchartOutput("deliveryPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            highchartOutput("gestationPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            highchartOutput("carePlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        tabPanel(title = "Education",
                 withSpinner(
                   highchartOutput("educationPlot_database2"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ), 
        tabPanel(title = "Tobacco Use",
                 withSpinner(
                   highchartOutput("tobaccoPlot_database2"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ),  
        tabPanel(title = "Maternal Age",
                 withSpinner(
                   highchartOutput("agePlot_database2"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ),  
      )
    )
  })
  
  output$box_pat11 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat7",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "Race",
          withSpinner(
            highchartOutput("racePlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Delivery Method",
          withSpinner(
            highchartOutput("deliveryPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            highchartOutput("gestationPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            highchartOutput("carePlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        tabPanel(title = "Education",
                 withSpinner(
                   highchartOutput("educationPlot_database3"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ), 
        tabPanel(title = "Tobacco Use",
                 withSpinner(
                   highchartOutput("tobaccoPlot_database3"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ),  
        tabPanel(title = "Maternal Age",
                 withSpinner(
                   highchartOutput("agePlot_database3"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ),  
      )
    )
  })

  output$box_pat15 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat15",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "Race",
          withSpinner(
            highchartOutput("racePlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            highchartOutput("gestationPlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            highchartOutput("carePlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        tabPanel(title = "Education",
                 withSpinner(
                   highchartOutput("educationPlot_database4"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ), 
        tabPanel(title = "Tobacco Use",
                 withSpinner(
                   highchartOutput("tobaccoPlot_database4"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ),  
        tabPanel(title = "Maternal Age",
                 withSpinner(
                   highchartOutput("agePlot_database4"),
                   type = 4,
                   color = "#d33724",
                   size = 0.7
                 )         
                 
        ),  
      )
    )
  })
  
  
  # panel 4: odd ratio tables
  output$box_pat4 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat4",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "BMI",
          
          fluidRow(
            column(12, dataTableOutput("oddsPlot")),
          ),          
        ),
        tabPanel(
          title = "Race",
          withSpinner(
            dataTableOutput("raceOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Weight Gain",
          withSpinner(
            dataTableOutput("wtgainOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        tabPanel(
          title = "Delivery",
          withSpinner(
            dataTableOutput("deliveryOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            dataTableOutput("gestationOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            dataTableOutput("careOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        tabPanel(
          title = "Interval of Last Pregnancy",
          withSpinner(
            dataTableOutput("lastpregOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )     
        ),
        tabPanel(
          title = "Education",
          withSpinner(
            dataTableOutput("educationOddsPlot"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )     
        )
      )
    )
  })    
  
  output$box_pat8 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat4",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "Race",
          
          fluidRow(
            column(12, dataTableOutput("raceOddsPlot_database2")),
          ),          
        ),
        tabPanel(
          title = "Delivery Method",
          withSpinner(
            dataTableOutput("deliveryOddsPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            dataTableOutput("gestationOddsPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            dataTableOutput("careOddsPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        
        tabPanel(
          title = "Education",
          withSpinner(
            dataTableOutput("educationOddsPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Tobacco Use",
          withSpinner(
            dataTableOutput("tobaccoOddsPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Maternal Age",
          withSpinner(
            dataTableOutput("ageOddsPlot_database2"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        )
      )
    )
  })    
  
  output$box_pat12 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat12",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "Race",
          
          fluidRow(
            column(12, dataTableOutput("raceOddsPlot_database3")),
          ),          
        ),
        tabPanel(
          title = "Delivery Method",
          withSpinner(
            dataTableOutput("deliveryOddsPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            dataTableOutput("gestationOddsPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            dataTableOutput("careOddsPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        
        tabPanel(
          title = "Education",
          withSpinner(
            dataTableOutput("educationOddsPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Tobacco Use",
          withSpinner(
            dataTableOutput("tobaccoOddsPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Maternal Age",
          withSpinner(
            dataTableOutput("ageOddsPlot_database3"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        )
      )
    )
  })    
  
  output$box_pat16 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_pat16",
        width = NULL,
        # height = 400,
        tabPanel(
          title = "Race",
          
          fluidRow(
            column(12, dataTableOutput("raceOddsPlot_database4")),
          ),          
        ),
        tabPanel(
          title = "Gestational Age",
          withSpinner(
            dataTableOutput("gestationOddsPlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        ),
        tabPanel(
          title = "Start of Prenatal Care",
          withSpinner(
            dataTableOutput("careOddsPlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        ),
        
        tabPanel(
          title = "Education",
          withSpinner(
            dataTableOutput("educationOddsPlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Tobacco Use",
          withSpinner(
            dataTableOutput("tobaccoOddsPlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )              
        ),
        tabPanel(
          title = "Maternal Age",
          withSpinner(
            dataTableOutput("ageOddsPlot_database4"),
            type = 4,
            color = "#d33724",
            size = 0.7
          )        
        )
      )
    )
  })    
  
  
  # Observation that react when button is clicked -----------------------------
  
  # database 1 
  observe({
    
    if (input$database1) {
      updateSelectInput(session, "riskInput", label = "",
                        choices = risk_factor1,
                        selected = "pre-pregnancy_diabetes")
      
      updateSliderInput(session, "yearInput", label = "Year",
                        value = years1, 
                        min = min(years1),
                        max = max(years1),
                        step = 1L)
      
      req(input$riskInput, input$yearInput)
      
      updateButton(session, "database1", value = TRUE, disabled=TRUE)
      updateButton(session, "database2", value = FALSE, disabled=FALSE)
      updateButton(session, "database3", value = FALSE, disabled=FALSE)
      updateButton(session, "database4", value = FALSE, disabled=FALSE)
      
      shinyjs::show("database1_panel")
      shinyjs::hide("database2_panel")
      shinyjs::hide("database3_panel")
      shinyjs::hide("database4_panel")
      click("confirm")
    }
    
  })  
  
  # database 2 
  observe({
    
    if (input$database2) {
      updateSelectInput(session, "riskInput", label = "",
                        choices = risk_factor2,
                        selected = "chronic_htn")

      updateSliderInput(session, "yearInput", label = "Year",
                        value = years2,
                        min = min(years2),
                        max = max(years2),
                        step = 1L)

      req(input$riskInput, input$yearInput)
      
      updateButton(session, "database2", value = TRUE, disabled=TRUE)
      updateButton(session, "database1", value = FALSE, disabled=FALSE)
      updateButton(session, "database3", value = FALSE, disabled=FALSE)
      updateButton(session, "database4", value = FALSE, disabled=FALSE)
      
      shinyjs::show("database2_panel")
      shinyjs::hide("database1_panel")
      shinyjs::hide("database3_panel")
      shinyjs::hide("database4_panel")
      click("confirm")
      
    }
    
  })  
  
  # database 3 
  observe({
    
    if (input$database3) {
      updateSelectInput(session, "riskInput", label = "",
                        choices = risk_factor3,
                        selected = "anemia")
      
      updateSliderInput(session, "yearInput", label = "Year",
                        value = years3, 
                        min = min(years3),
                        max = max(years3),
                        step = 1L)
      
      req(input$riskInput, input$yearInput)
      updateButton(session, "database3", value = TRUE, disabled=TRUE)
      updateButton(session, "database1", value = FALSE, disabled=FALSE)
      updateButton(session, "database2", value = FALSE, disabled=FALSE)
      updateButton(session, "database4", value = FALSE, disabled=FALSE)
      
      shinyjs::show("database3_panel")
      shinyjs::hide("database1_panel")
      shinyjs::hide("database2_panel")
      shinyjs::hide("database4_panel")
      click("confirm")
    }
    
  })  
  
  # database 4 
  observe({
    
    if (input$database4) {
      updateSelectInput(session, "riskInput", label = "",
                        choices = risk_factor4,
                        selected = "anemia")
      
      updateSliderInput(session, "yearInput", label = "Year",
                        value = years4, 
                        min = min(years4),
                        max = max(years4),
                        step = 1L)
      req(input$riskInput, input$yearInput)
      updateButton(session, "database4", value = TRUE, disabled=TRUE)
      updateButton(session, "database1", value = FALSE, disabled=FALSE)
      updateButton(session, "database2", value = FALSE, disabled=FALSE)
      updateButton(session, "database3", value = FALSE, disabled=FALSE)
      
      shinyjs::show("database4_panel")
      shinyjs::hide("database1_panel")
      shinyjs::hide("database2_panel")
      shinyjs::hide("database3_panel")
      click("confirm")
    }
    
  })  
  
  
  # helper functions -----------------------------------------------------------
  get_graph1 <- function(plot_id, plot1, plot2){
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = plot_id,
        width = NULL,
        height = 100,
        tabPanel(
          title = sprintf("Cases of Condition in the United States "),
          withSpinner(
            plotOutput(plot1),
            type = 4,
            color = "#d33724",
            size = 0.7
          ),
          
        ), 
        tabPanel(
          title = sprintf("Percentage of Condition in the United States "),
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            plotOutput(plot2),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),         
      )
    )
  }
  
  # get_graph2("box_pat2" , "longPlot", "longPerPlot", "longAgePlot", 
  # "longOEPlot", "longLMPPlot", "longBirthWeightPlot")
  get_graph2 <- function(plot_id, plot_1, plot_2, plot_3, plot_4, plot_5, plot_6) {
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = plot_id,
        width = NULL,
        height = 100,
        tabPanel(
          title = "Cases",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput(plot_1),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "Percent of Total Births",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput(plot_2),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "Age",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput(plot_3),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "OE Gestational Age",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput(plot_4),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "LMP Gestational Age",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput(plot_5),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        tabPanel(
          title = "Birth Weight",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput(plot_6),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
      )
    )
  }

  plot_state <- function(state_grph) {
    ggplot() + 
      geom_sf(state_grph,
              mapping = aes(fill = count),
              color = "#ffffff", size = 0.25) +
      geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE),
                   aes(label = state_abbv),
                   size = 3) +
      scale_fill_gradientn() +
      labs(fill = "Cases") +
      coord_sf(datum = NA) 
  }
  
  update_graphs <- function() {
    
    # update long graph ---------------------------------------------------------
    file_path <- sprintf("data/database1/long_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_long_data(input$riskInput, code_map[[input$riskInput]])
    }
    
    chart2 <<- readRDS(file_path)
    chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)
    
    # update bmi graph ---------------------------------------------------------
    file_path <- sprintf("data/database1/bmi_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'bmi', demo_map[['bmi']])
    }
    
    bmi <<- readRDS(file_path)
    
    # update race graph ---------------------------------------------------------
    
    file_path <- sprintf("data/database1/race_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'race', demo_map[['race']])
    }
    
    race <<- readRDS(file_path)
    
    # update weight gain graph -------------------------------------------------
    file_path <- sprintf("data/database1/wtgain_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'wtgain', demo_map[['wtgain']])
    }
    
    wtgain <<- readRDS(file_path)
    
    # update delivery graph -------------------------------------------------
    file_path <- sprintf("data/database1/delivery_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'delivery', demo_map[['delivery']])
    }
    
    delivery <<- readRDS(file_path)
    
    # update gestation graph -------------------------------------------------
    file_path <- sprintf("data/database1/gestation_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'gestation', demo_map[['gestation']])
    }
    
    gestation <<- readRDS(file_path)
    
    # update care graph -------------------------------------------------
    file_path <- sprintf("data/database1/care_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'care', demo_map[['care']])
    }
    
    care <<- readRDS(file_path)
    
    # update lastpreg graph -------------------------------------------------
    file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_table_data(input$riskInput, code_map[[input$riskInput]], 'lastpreg', demo_map[['lastpreg']])
    }
    
    lastpreg <<- readRDS(file_path)
    
    
    # update education graph -------------------------------------------------
    file_path <- sprintf("data/database1/education_tables/%s.rds", input$riskInput)
    
    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_education_data(input$riskInput, code_map[[input$riskInput]])
    }
    
    education <<- readRDS(file_path)
    
  }
  
  get_condition <- function(risk_factor, default_condition) {
    condition <- input$riskInput
    
    condition_found <- FALSE 
    for (x in names(risk_factor)) {
      if (condition %in% risk_factor[[x]]) {
        condition_found <- TRUE 
      }
    }
    
    if (!condition_found) {
      condition = default_condition
    }
    condition
  }
  
  demoPlot <- function(reverse_map, sub_table, feat){
    req(input$riskInput, input$yearInput)
    feat.name <- chartr(" ", "_", feat)
    file <- sprintf("%s_%s", input$riskInput, tolower(feat.name))
    
    sub_table %>% hchart("column", hcaes(x = factor(!!as.symbol(reverse_map[[input$riskInput]])), y = count, group=!!as.symbol(feat)), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_tooltip(pointFormat = "<b> Count:</b> {point.y:,.0f} <br>
                 <b> Percentage:</b> {point.percentage:,.2f}%") %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(sub_table[[reverse_map[[input$riskInput]]]])) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = file
      )
  }
  
  plot_per_state <- function(state_grph) {
    ggplot() +
      geom_sf(state_grph,
              mapping = aes(fill = per),
              color = "#ffffff", size = 0.25) +
      geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE),
                   aes(label = state_abbv),
                   size = 3) +
      scale_fill_gradientn() +
      labs(fill = "Percentage") +
      coord_sf(datum = NA)
  }
  
  graph4_update <- function(database, tablename, feat, reverse_map, ref_feat) {
    req(input$riskInput, input$yearInput)
    sub_table <- graph3_update(database, tablename, feat, reverse_map)
    
    sub_table <- sub_table %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
    sub_table <- sub_table %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
    
    sub_table <- sub_table %>% arrange(match(!!as.symbol(feat), c(ref_feat, setdiff(c(ref_feat), !!as.symbol(feat)))))
    
    sub_table[is.na(sub_table)] = 0
    
    odd_table <- data.matrix(sub_table[,-c(1) ])
    rownames(odd_table) = sub_table[[feat]]
    
    result <- epitab(odd_table, method="oddsratio")
    as.data.frame(result$tab[,c(1, 3, 5, 8)])    
    
  }
  
  get_long_plots <- function(long_grph, feat) {
    feat.name <- chartr(" ", "_", tolower(feat))
    long_grph %>% hchart("line", hcaes(x = Year, y = !!as.symbol(feat))) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(text="Value")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = feat.name
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
  }
  
  graph3_update <- function(database, tablename, feat, reverse_map) {
    req(input$riskInput, input$yearInput)
    
    file_path <- sprintf("data/database%s/%s_tables/%s.rds", database, tablename, input$riskInput)
    if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
      # database = 2
      # tablename = "age"
      file_path <- sprintf("data/database%s/%s_tables/%s.rds", database, tablename, "oe_gesation_10")
    }
    
    sub_table <- readRDS(file_path)
    sub_table <- sub_table %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
    
    if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
      
      sub_table <- get_preg_outcome_data(database, sub_table)
      
      cc <- sub_table %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Year`, !!as.symbol(feat))
      
      sub_table <- summarise(cc, Births=sum(Births))
      
    }     
    
    sub_table <- sub_table %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), !!as.symbol(feat))
    sub_table <- summarise(sub_table, count = sum(Births))
    
    
    # remove entries with missing or unknown condition
    sub_table <- filter(sub_table, !!as.symbol(reverse_map[[input$riskInput]]) != "Unknown or Not Stated")
    sub_table <- filter(sub_table, !!as.symbol(reverse_map[[input$riskInput]]) != "Not Reported")  
  }
  
  
}

