# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # REACTIVES ------------------------------------------------------------------
  observeEvent(input$database2, {
    updateSelectInput(session, "riskInput", label = "",
                      choices = risk_factor2,
                      selected = "chronic_htn")
    updateButton(session, "database2", style = "warning")
    updateButton(session, "database1", style = "success")
    updateButton(session, "database3", style = "success")
    updateButton(session, "database4", style = "success")
    
  })  
  
  observeEvent(input$database3, {
    showModal(modalDialog(
      title = "Work in Progress",
      "We are still actively working on get this set of CDC dataset into our database.",
      easyClose = TRUE,
      footer = NULL
    ))
  })  
  
  observeEvent(input$database4, {
    showModal(modalDialog(
      title = "Work in Progress",
      "We are still actively working on get this set of CDC dataset into our database.",
      easyClose = TRUE,
      footer = NULL
    ))
  })  
  
 
  update_graphs <- function() {

    # update long graph ---------------------------------------------------------
    file_path <- sprintf("data/database1/long_tables/%s.rds", input$riskInput)

    # if file does not exist get data from wonder api
    if (!file.exists(file_path)) {
      get_long_data(input$riskInput, code_map[[input$riskInput]])
    }

    chart2 <<- readRDS(file_path)
    chart2$`% of Total Births` <<- as.numeric(chart2$`% of Total Births`)

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
  
  update_state_graph2 <- reactive({
    input$confirm 
    isolate({
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
  
  update_state_graph <- reactive({
    input$confirm 
    isolate({
      state_grph <- states_df %>% filter(condition==input$riskInput, condition_status=="Yes")
      
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      
      state_grph <- state_grph %>% group_by(state_name)
      state_grph <- summarise(state_grph, count = sum(counts)) 
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  
  update_long_graph <- reactive({
    input$confirm 
    
    isolate({
      file_path <- sprintf("data/database1/long_tables/%s.rds", input$riskInput)

      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/long_tables/%s.rds", "oe_gesation_10")
        
      }
      
      chart2 <<- readRDS(file_path)
      chart2$`% of Total Births` <<- as.numeric(chart2$`% of Total Births`)

      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        chart2 <- get_preg_outcome_data(chart2)

        cc <- chart2 %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`, `Year`)
        
        chart2 <- summarise(cc, Births=sum(Births), 
                  `% of Total Births` = sum(`% of Total Births`),
                  `Average Age of Mother (years)` = mean(`Average Age of Mother (years)`),
                  `Average OE Gestational Age (weeks)` = mean(`Average OE Gestational Age (weeks)`),
                  `Average LMP Gestational Age (weeks)` = mean(`Average LMP Gestational Age (weeks)`),
                  `Average Birth Weight (grams)` = mean(`Average Birth Weight (grams)`),
                  `Average Pre-pregnancy BMI` = mean(`Average Pre-pregnancy BMI`)
                  )
      }      
      

      long_grph <- chart2 %>% filter(!!as.symbol(reverse_map[[input$riskInput]])=="Yes")
      long_grph <- long_grph %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))
    })
  })


  update_bmi_graph <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/bmi_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/bmi_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      bmi <<- readRDS(file_path)
      bmi <- bmi %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
     
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        bmi <- get_preg_outcome_data(bmi)
        
        cc <- bmi %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`, `Year`,`Mother's Pre-pregnancy BMI`)
        
        bmi <- summarise(cc, Births=sum(Births))
        
      }
      
      bmi_sub <- bmi %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Pre-pregnancy BMI`)
      bmi_sub <- summarise(bmi_sub, count = sum(Births))
    })  
  })
  
    
  update_race_graph <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/race_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/race_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      race <<- readRDS(file_path)
      race <- race %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        race <- get_preg_outcome_data(race)
        
        cc <- race %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Single Race 6`)
        
        race <- summarise(cc, Births=sum(Births))
        
      }      
      
      race_sub <- race %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Single Race 6`)
      race_sub <- summarise(race_sub, count = sum(Births))
    })  
  })
  
  update_wtgain_graph <- reactive({
    input$confirm 
    isolate({

      file_path <- sprintf("data/database1/wtgain_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/wtgain_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      wtgain <<- readRDS(file_path)
      wtgain <- wtgain %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        wtgain <- get_preg_outcome_data(wtgain)
        
        cc <- wtgain %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Weight Gain Recode`)
        
        wtgain <- summarise(cc, Births=sum(Births))
        
      }     
      
      wtgain_sub <- wtgain %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Weight Gain Recode`)
      wtgain_sub <- summarise(wtgain_sub, count = sum(Births))
    })  
  })  
  
  update_delivery_graph <- reactive({
    input$confirm 
    isolate({

      file_path <- sprintf("data/database1/delivery_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/delivery_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      delivery <<- readRDS(file_path)
      delivery <- delivery %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        delivery <- get_preg_outcome_data(delivery)
        
        cc <- delivery %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Final Route and Delivery Method`)
        
        delivery <- summarise(cc, Births=sum(Births))
        
      }     
      delivery_sub <- delivery %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Final Route and Delivery Method`)
      delivery_sub <- summarise(delivery_sub, count = sum(Births))
    })  
  })  

  update_gestation_graph <- reactive({
    input$confirm 
    isolate({
      
      file_path <- sprintf("data/database1/gestation_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/gestation_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      gestation <<- readRDS(file_path)
      gestation <- gestation %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        gestation <- get_preg_outcome_data(gestation)
        
        cc <- gestation %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`,`OE Gestational Age Recode 10`)
        
        gestation <- summarise(cc, Births=sum(Births))
        
      }     
      
      gestation_sub <- gestation %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `OE Gestational Age Recode 10`)
      gestation_sub <- summarise(gestation_sub, count = sum(Births))
    })  
  })      
  
  update_care_graph <- reactive({
    input$confirm 
    isolate({


      
      file_path <- sprintf("data/database1/care_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth", "extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/care_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      care <<- readRDS(file_path)
      care <- care %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        care <- get_preg_outcome_data(care)
        
        cc <- care %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`,`Trimester Prenatal Care Began`)
        
        care <- summarise(cc, Births=sum(Births))
        
      }     
      care_sub <- care %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Trimester Prenatal Care Began`)
      care_sub <- summarise(care_sub, count = sum(Births))
    })  
  })      
  
  update_lastpreg_graph <- reactive({
    input$confirm 
    isolate({
      
      file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/lastpreg_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      lastpreg <<- readRDS(file_path)
      lastpreg <- lastpreg %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        lastpreg <- get_preg_outcome_data(lastpreg)
        
        cc <- lastpreg %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Interval of Last Pregnancy`)
        
        lastpreg <- summarise(cc, Births=sum(Births))
        
      }     
      lastpreg_sub <- lastpreg %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Interval of Last Pregnancy`)
      lastpreg_sub <- summarise(lastpreg_sub, count = sum(Births))
    })  
  })     
  
  update_education_graph <- reactive({
    input$confirm 
    isolate({

      
      file_path <- sprintf("data/database1/education_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/education_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      education <<- readRDS(file_path)
      education <- education %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        education <- get_preg_outcome_data(education)
        
        cc <- education %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`,`Moderate and Late preterm birth`, `Year`, `Mother's Education`)
        
        education <- summarise(cc, Births=sum(Births))
        
      }     
      
      edu <- education %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Education`)
      edu <- summarise(edu, count = sum(Births))
    })
  })
  
  update_bmi_oddratio <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/bmi_tables/%s.rds", input$riskInput)
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        file_path <- sprintf("data/database1/bmi_tables/%s.rds", "oe_gesation_10")
        
      }
      
      
      bmi <<- readRDS(file_path)
      bmi <- bmi %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))  
      
      if (input$riskInput %in% c("fullterm_birth", "preterm_birth","extreme_birth", "severe_birth", "moderate_birth")) {
        
        bmi <- get_preg_outcome_data(bmi)
        
        cc <- bmi %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Pre-pregnancy BMI`)
        
        bmi <- summarise(cc, Births=sum(Births))
        
      }
      
      bmi_sub <- bmi %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Pre-pregnancy BMI`)
      bmi_sub <- summarise(bmi_sub, count = sum(Births))
      
      bmi_sub <- bmi_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      bmi_sub <- bmi_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      bmi_sub <- bmi_sub %>% arrange(match(`Mother's Pre-pregnancy BMI`, c("Normal 18.5-24.9", setdiff(c("Normal 18.5-24.9"), `Mother's Pre-pregnancy BMI`))))
      
      odd_table <- data.matrix(bmi_sub[,-c(1) ])
      rownames(odd_table) = bmi_sub$`Mother's Pre-pregnancy BMI`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
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
        
        race <- get_preg_outcome_data(race)
        
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
        
        wtgain <- get_preg_outcome_data(wtgain)
        
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
        
        delivery <- get_preg_outcome_data(delivery)
        
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
        
        gestation <- get_preg_outcome_data(gestation)
        
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
        
        care <- get_preg_outcome_data(care)
        
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
        
        lastpreg <- get_preg_outcome_data(lastpreg)
        
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
        
        education <- get_preg_outcome_data(education)
        
        cc <- education %>% group_by(`Preterm birth`, `Fullterm birth`, `Extreme preterm birth`, `Severe preterm birth`, `Moderate and Late preterm birth`,`Year`,`Mother's Education`)
        
        education <- summarise(cc, Births=sum(Births))
        
      }     
      odds_sub <- education %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Education`)
      odds_sub <- summarise(odds_sub, count = sum(Births))
      
      odds_sub <- odds_sub %>% filter(!!as.symbol(reverse_map[[input$riskInput]]) %in% c("Yes", "No"))
      odds_sub <- odds_sub %>% pivot_wider(names_from=!!as.symbol(reverse_map[[input$riskInput]]), values_from=count)
      
      odds_sub <- odds_sub %>% arrange(match(`Mother's Education`, c("White", setdiff(c("White"), `Mother's Education`))))
      odds_sub[is.na(odds_sub)] = 0
      
      odd_table <- data.matrix(odds_sub[,-c(1) ])
      rownames(odd_table) = odds_sub$`Mother's Education`
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  
  output$box_pat <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 100,
        tabPanel(
          title = sprintf("Cases of Condition in the United States "),
          # withSpinner(
          #   # plotlyOutput("distPlot", height = 230),
          #   plotOutput("distPlot"),
          #   type = 4,
          #   color = "#d33724", 
          #   size = 0.7 
          # ),
          fluidRow(
             column(12, plotOutput("distPlot")),
             # column(6, plotOutput("distPlot2"))
          ),
          
          # plotOutput("statePerPlot"),
        ), 
        tabPanel(
          title = sprintf("Percentage of Condition in the United States "),
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
            introBox(
              dropdown(
                radioGroupButtons(
                  inputId = "box_pat1",
                  label = NULL, 
                  choices = c("Show all", "Show top 10 only"), 
                  selected = "Show all", 
                  direction = "vertical"
                ),
                size = "xs",
                icon = icon("gear", class = "opt"), 
                up = TRUE
              )
            )
          ),
          withSpinner(
            # plotlyOutput("distPlot", height = 230),
            plotOutput("statePerPlot"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),         
      )
    )
  })
  
  output$box_pat2 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat2",
        width = NULL,
        height = 100,
        tabPanel(
          title = "Cases",
          div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em;",
          ),
          withSpinner(
            highchartOutput("longPlot"),
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
            highchartOutput("longPerPlot"),
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
            highchartOutput("longAgePlot"),
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
            highchartOutput("longOEPlot"),
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
            highchartOutput("longLMPPlot"),
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
            highchartOutput("longBirthWeightPlot"),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        ),
        # tabPanel(title = "Number of Prenatal Visits"),
      )
    )
  })
  
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
            # column(6, dataTableOutput("oddsPlot2"))
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
  
  output$longPlot <- renderHighchart({
    
    long_grph <- update_long_graph()
    
    long_grph %>% hchart("line", hcaes(x = Year, y = Births)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(text="Counts")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    
  })
  
  output$longPerPlot <- renderHighchart({
    long_grph <- update_long_graph()
    
    
    long_grph %>% hchart("line", hcaes(x = Year, y = `% of Total Births`)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(format = "{value}%")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    
  })
  
  output$longAgePlot <- renderHighchart({
    long_grph <- update_long_graph()
    
    long_grph %>% hchart("line", hcaes(x = Year, y = `Average Age of Mother (years)`)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(format = "{value}%")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    
  })
  
  output$longLMPPlot <- renderHighchart({
    long_grph <- update_long_graph()
    
    long_grph %>% hchart("line", hcaes(x = Year, y = `Average LMP Gestational Age (weeks)`)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(format = "{value}%")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    
  })
  
  output$longOEPlot <- renderHighchart({
    long_grph <- update_long_graph()
    
    long_grph %>% hchart("line", hcaes(x = Year, y = `Average OE Gestational Age (weeks)`)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(format = "{value}%")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    
  })
  
  output$longBirthWeightPlot <- renderHighchart({
    long_grph <- update_long_graph()
    
    long_grph %>% hchart("line", hcaes(x = Year, y = `Average Birth Weight (grams)`)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(format = "{value}%")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)))
    
  })  
  
  output$longBMIPlot <- renderHighchart({
    long_grph <- diabetes_long %>% filter(`Pre-pregnancy Diabetes`=="Yes") 
    
    long_grph %>% hchart("line", hcaes(x = Year, y = `Average Pre-pregnancy BMI`)) %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Year")) %>%
      hc_yAxis(title = list(format = "{value}%")) %>%
      hc_legend(align = "center",
                verticalAlign = "top") %>%
      hc_exporting(
        enabled =TRUE,
        filename = 'Risk_Factor'
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) 
  })    
  
  output$bmiPlot <- renderHighchart({
    bmi_sub <- update_bmi_graph()
    demoPlot(bmi_sub, "Mother's Pre-pregnancy BMI")
  })
  
  output$racePlot <- renderHighchart({
    race_sub <- update_race_graph()
   
    race_sub %>% hchart("column", hcaes(x = factor(!!as.symbol(reverse_map[[input$riskInput]])), y = count, group=`Mother's Single Race 6`), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(race_sub[[reverse_map[[input$riskInput]]]])) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top") 
  })  
  
  demoPlot <- function(sub_table, feat){
    sub_table %>% hchart("column", hcaes(x = factor(!!as.symbol(reverse_map[[input$riskInput]])), y = count, group=!!as.symbol(feat)), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(sub_table[[reverse_map[[input$riskInput]]]])) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top") 
  }
  
  output$weightGainPlot <- renderHighchart({
    weight_gain_sub <- update_wtgain_graph()
    demoPlot(weight_gain_sub, "Mother's Weight Gain Recode")
  })  
  
  output$educationPlot <- renderHighchart({
    edu <- update_education_graph()
    
    edu %>% hchart("column", hcaes(x = factor(!!as.symbol(reverse_map[[input$riskInput]])), y = count, group=`Mother's Education`), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(edu[[reverse_map[[input$riskInput]]]])) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top") 
  })  
  
  output$deliveryPlot <- renderHighchart({
    delivery_sub <- update_delivery_graph()
    demoPlot(delivery_sub, "Final Route and Delivery Method")
  })  
  
  output$gestationPlot <- renderHighchart({
    gestation_sub <- update_gestation_graph()
    demoPlot(gestation_sub, "OE Gestational Age Recode 10")
  })    
  
  output$carePlot <- renderHighchart({
    care_sub <- update_care_graph()
    demoPlot(care_sub, "Trimester Prenatal Care Began")
  })      
  
  output$lastpregPlot <- renderHighchart({
    lastpreg_sub <- update_lastpreg_graph()
    demoPlot(lastpreg_sub, "Interval of Last Pregnancy")
  })        
  
  
  output$oddsPlot <- renderDataTable({
    bmi_table <- update_bmi_oddratio()

    datatable(bmi_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  
  output$raceOddsPlot <- renderDataTable({
    race_table <- update_race_oddratio()
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

  output$gestationOddsPlot <- renderDataTable({
    gestation_table <- update_gestation_oddratio()
    datatable(gestation_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
      
  output$careOddsPlot <- renderDataTable({
    care_table <- update_care_oddratio()
    datatable(care_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$lastpregOddsPlot <- renderDataTable({
    lastpreg_table <- update_lastpreg_oddratio()
    datatable(lastpreg_table, options = list(searching = FALSE), caption="Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$distPlot <- renderPlot({
    state_grph <- update_state_graph()
    ggplot() + 
      geom_sf(state_grph,
              mapping = aes(fill = count),
              color = "#ffffff", size = 0.25) +
      geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE),
                   aes(label = state_abbv),
                   size = 3) +
      scale_fill_gradientn() +
      labs(fill = "Cases") +
      # ggtitle("Pre-pregnancy diabetes") +
      coord_sf(datum = NA) 
      # theme(
      #   legend.position = "bottom",
      #   legend.text.align = 0,
      #   plot.margin = unit(c(.5,.5,.2,.5), "cm")) +
      # theme(
      #   axis.line = element_blank(),
      #   axis.text.x = element_blank(),
      #   axis.text.y = element_blank(),
      #   axis.ticks = element_blank(),
      #   axis.title.x = element_blank(),
      #   axis.title.y = element_blank(),
      #   panel.grid.major = element_blank(),
      #   panel.grid.minor = element_blank(),
      # )
  })
  
  output$distPlot2 <- renderPlot({
    state_grph <- update_state_graph3()
    ggplot() +
      geom_sf(state_grph,
              mapping = aes(fill = count),
              color = "#ffffff", size = 0.25) +
      geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE),
                   aes(label = state_abbv),
                   size = 3) +
      scale_fill_gradient(low = "grey", high = "brown") +
      labs(fill = "Cases") +
      ggtitle("Eclampsia") +
      coord_sf(datum = NA) +
      
      theme(
        legend.position = "bottom",
        legend.text.align = 0,
        plot.margin = unit(c(.5,.5,.2,.5), "cm")) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
      ) 
  })
  
  output$statePerPlot <- renderPlot({
    state_grph <- update_state_graph2()
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
  })
}

