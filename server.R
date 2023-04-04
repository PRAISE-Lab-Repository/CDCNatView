# Define server logic required to draw a histogram
server <- function(input, output) {
  # REACTIVES ------------------------------------------------------------------
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
      print("cake")
      print(input$riskInput)
      state_grph <- states_df %>% filter(condition==input$riskInput, condition_status=="Yes")
      
      state_grph <- state_grph %>% filter( between(year, input$yearInput[1], input$yearInput[2]))  
      
      state_grph <- state_grph %>% group_by(state_name)
      state_grph <- summarise(state_grph, count = sum(counts)) 
      state_grph <- left_join(get_urbn_map(map = "states", sf = TRUE),
                              state_grph,
                              by = "state_name")
    })
  })  
  
  update_state_graph3 <- reactive({
    input$confirm 
    isolate({
      state_grph <- states_df %>% filter(condition=="eclampsia", condition_status=="Yes")
      
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
      
      # if file does not exist get data from wonder api
      if (!file.exists(file_path)) {
        get_long_data(input$riskInput, code_map[[input$riskInput]])
      }
      
      chart2 <- readRDS(file_path)
      chart2$`% of Total Births` <- as.numeric(chart2$`% of Total Births`)
      
      long_grph <- chart2 %>% filter(!!as.symbol(reverse_map[[input$riskInput]])=="Yes")
      long_grph <- long_grph %>% filter( between(Year, input$yearInput[1], input$yearInput[2]))
    })
  })
  
  update_bmi_graph <- reactive({
    input$confirm 
    isolate({
      bmi <-  demo_df %>% filter(condition==input$riskInput)
      bmi <- bmi %>% group_by(bmi, condition_status)
      bmi <- summarise(bmi, count = sum(counts))
    })
  })
  
  update_bmi_graph2 <- reactive({
    input$confirm 
    isolate({
      bmi <-  demo_df %>% filter(condition=="gestational_diabetes")
      bmi <- bmi %>% group_by(bmi, condition_status)
      bmi <- summarise(bmi, count = sum(counts))
    })
  })
  
  update_race_graph <- reactive({
    input$confirm 
    isolate({
      race <-  demo_df %>% filter(condition==input$riskInput)
      race <- race %>% group_by(race, condition_status)
      race <- summarise(race, count = sum(counts))
    })
  })
  
  update_wtgain_graph <- reactive({
    input$confirm 
    isolate({
      wtgain <-  demo_df %>% filter(condition==input$riskInput)
      wtgain <- wtgain %>% group_by(wtgain, condition_status)
      wtgain <- summarise(wtgain, count = sum(counts))
    })
  })
  
  update_education_graph <- reactive({
    input$confirm 
    isolate({
      file_path <- sprintf("data/database1/education_tables/%s.rds", input$riskInput)
      
      # if file does not exist get data from wonder api
      if (!file.exists(file_path)) {
        get_education_data(input$riskInput, code_map[[input$riskInput]])
      }
      
      education <- readRDS(file_path)
      
      edu <- education %>% group_by(!!as.symbol(reverse_map[[input$riskInput]]), `Mother's Education`)
      edu <- summarise(edu, count = sum(Births))
    })
  })
  
  update_bmi_oddratio <- reactive({
    input$confirm 
    isolate({
      bmi <-  demo_df %>% filter(condition==input$riskInput)
      bmi <- bmi %>% group_by(bmi, condition_status)
      bmi <- summarise(bmi, count = sum(counts))
      
      bmi <- bmi %>% filter(condition_status %in% c("Yes", "No"))
      bmi <- bmi %>% pivot_wider(names_from=condition_status, values_from=count)
      
      bmi <- bmi %>% arrange(match(bmi, c("Normal 18.5-24.9", setdiff(c("Normal 18.5-24.9"), bmi))))
      
      odd_table <- data.matrix(bmi[,-c(1) ])
      rownames(odd_table) = bmi$bmi
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
    

  })
  
  update_bmi_oddratio2 <- reactive({
    input$confirm 
    isolate({
      bmi <-  demo_df %>% filter(condition=="eclampsia")
      bmi <- bmi %>% group_by(bmi, condition_status)
      bmi <- summarise(bmi, count = sum(counts))
      
      bmi <- bmi %>% filter(condition_status %in% c("Yes", "No"))
      bmi <- bmi %>% pivot_wider(names_from=condition_status, values_from=count)
      
      bmi <- bmi %>% arrange(match(bmi, c("Normal 18.5-24.9", setdiff(c("Normal 18.5-24.9"), bmi))))
      
      odd_table <- data.matrix(bmi[,-c(1) ])
      rownames(odd_table) = bmi$bmi
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })   
  })
  
  update_race_oddratio <- reactive({
    input$confirm 
    isolate({
      race <-  demo_df %>% filter(condition==input$riskInput)
      race <- race %>% group_by(race, condition_status)
      race <- summarise(race, count = sum(counts))
      
      race <- race %>% filter(condition_status %in% c("Yes", "No"))
      race <- race %>% pivot_wider(names_from=condition_status, values_from=count)
      
      race <- race %>% arrange(match(race, c("White", setdiff(c("White"), race))))
      
      odd_table <- data.matrix(race[,-c(1) ])
      rownames(odd_table) = race$race
      
      result <- epitab(odd_table, method="oddsratio")
      as.data.frame(result$tab[,c(1, 3, 5, 8)])
      
    })
  })
  
  update_wtgain_oddratio <- reactive({
    input$confirm 
    isolate({
      wtgain <-  demo_df %>% filter(condition==input$riskInput)
      wtgain <- wtgain %>% group_by(wtgain, condition_status)
      wtgain <- summarise(wtgain, count = sum(counts))
      
      wtgain <- wtgain %>% filter(condition_status %in% c("Yes", "No"))
      wtgain <- wtgain %>% pivot_wider(names_from=condition_status, values_from=count)
      
      wtgain <- wtgain %>% arrange(match(wtgain, c("21 to 30 pounds", setdiff(c("21 to 30 pounds"), wtgain))))
      
      odd_table <- data.matrix(wtgain[,-c(1) ])
      rownames(odd_table) = wtgain$wtgain
      
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
            # column(6, highchartOutput("bmiPlot2"))
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
        tabPanel(title = "Pregnancy Outcome"),
        tabPanel(title = "Gestational Age"),
        tabPanel(title = "Height"),
        tabPanel(title = "Start of Prenatal Care"),
        tabPanel(title = "Interval of Last Pregnancy"),
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
        tabPanel(title = "Pregnancy Outcome"),
        tabPanel(title = "Gestational Age"),
        tabPanel(title = "Height"),
        tabPanel(title = "Start of Prenatal Care"),
        tabPanel(title = "Interval of Last Pregnancy"),
        tabPanel(title = "Education")
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
  
  output$bmiPlot2 <- renderHighchart({
    bmi <- update_bmi_graph2()
    
    bmi %>% hchart("column", hcaes(x = factor(condition_status), y = count, group=bmi), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(bmi$condition_status)) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top")  %>%
      hc_title(
        text = "<b>Eclampsia</b>",
        margin = 20,
        align = "left"
      )
  })
  
  output$bmiPlot <- renderHighchart({
    bmi <- update_bmi_graph()

    bmi %>% hchart("column", hcaes(x = factor(condition_status), y = count, group=bmi), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(bmi$condition_status)) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top")
  })
  
  output$racePlot <- renderHighchart({
    race <- update_race_graph()
    race %>% hchart("column", hcaes(x = factor(condition_status), y = count, group=race), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(race$condition_status)) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top") 
  })  
  
  output$weightGainPlot <- renderHighchart({
    weight_gain <- update_wtgain_graph()
    weight_gain %>% hchart("column", hcaes(x = factor(condition_status), y = count, group=wtgain), stacking="percent") %>%
      hc_add_theme(hc_theme_flat()) %>%
      hc_colors(colors) %>%
      hc_xAxis(title = list(text="Condition Present"), categories = levels(weight_gain$condition_status)) %>%
      hc_xAxis(title = list(text="Condition Present")) %>%
      hc_yAxis(labels = list(format = "{value}%")) %>%
      hc_yAxis(title = list(text="Percent")) %>%
      hc_legend(align = "center", verticalAlign = "top") 
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
  
  output$heatmapPlot <- renderPlot({
    corrplot.mixed(M, order = 'AOE', tl.cex=0.6)
  })
  
  output$oddsPlot <- renderDataTable({
    bmi_table <- update_bmi_oddratio()

    datatable(bmi_table, options = list(searching = FALSE), caption="BMI vs. Preterm Birth Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  output$oddsPlot2 <- renderDataTable({
    bmi_table <- update_bmi_oddratio2()
    
    datatable(bmi_table, options = list(searching = FALSE), caption="BMI vs. Eclampsia Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  
  output$raceOddsPlot <- renderDataTable({
    race_table <- update_race_oddratio()
    datatable(race_table, options = list(searching = FALSE), caption="Race vs. Pre-pregnancy Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
  })
  
  
  output$wtgainOddsPlot <- renderDataTable({
    wtgain_table <- update_wtgain_oddratio()
    datatable(wtgain_table, options = list(searching = FALSE), caption="Weight Gain vs. Pre-pregnancy Odds Ratio Analysis")  %>%  formatRound(columns=c('oddsratio', 'p.value'), digits=3)
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
      labs(fill = "Diabetes Percentag") +
      coord_sf(datum = NA)
  })
}

