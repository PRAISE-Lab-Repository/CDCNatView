ui <- dashboardPage(
  dashboardHeader(
    title="CDC NatView",
    
    tags$li(
      a(
        strong("ABOUT US"),
        height = 40,
        href = "http://www.cs.columbia.edu/~ansaf/praise/index.html",
        title = "ABOUT US",
        target = "_blank"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        strong("CONTACT"),
        height = 40,
        href = "http://www.cs.columbia.edu/~ansaf/praise/contact-us.html",
        title = "CONTACT",
        target = "_blank"
      ),
      class = "dropdown"
    )
    
    
    ),
  
  
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    width = 300,
    
    introBox(
      div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
      
      sidebarMenu(
        introBox(
          div(id = "sidebar_button",
              bsButton(inputId = "confirm", 
                       label = "UPDATE DASHBOARD", 
                       icon = icon("play-circle"), 
                       style = "danger")
          )
        ),
        div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
        
        # Controls the year slidebar -----------------------------------------------------------------
        br(),
        menuItem(
          "YEAR",
          tabName = "year",
          icon = icon("calendar"),
          sliderInput(
            inputId = "yearInput",
            label = "Year",
            value = years1,
            min = min(years1),
            max = max(years1),
            step = 1L,
            sep = ""
          )
        ),
        br(),
        menuItem(
          "PATIENT CONDITION",
          tabName = "risk_factor",
          icon = icon("flask"),
          selectInput(
            inputId = "riskInput",
            label = "",
            choices = risk_factor1,
            selected = "pre-pregnancy_diabetes"
            # ,multiple = TRUE
          )
        ),
        br(),
        menuItem(
          "STATES",
          tabName = "state_filter",
          icon = icon("map"),
          selectInput(
            inputId = "stateLong",
            label = "",
            choices = state_list
          )
        )
        # br(),
        # menuItem(
        #   "DOWNLOAD SELECTION",
        #   tabName = "download",
        #   icon = icon("download"),
        #   textInput(
        #     inputId = "filename",
        #     placeholder = "Name download file",
        #     label = ""
        #   ),
        #   div(
        #     downloadButton(
        #       outputId = "downloadData",
        #       label = "Save Selected Data",
        #       icon = icon("download"),
        #       style = "color: black; margin-left: 15px; margin-bottom: 5px;"
        #     )
        #   )
        # )
        
      )
    )
  ),
  
  
  
  
  # Body -----------------------------------------------------------------
  
  dashboardBody(
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("database1", 
                   label = "Database 1: 2016 - 2021 (Expanded)", 
                   style = "success", type='toggle',value = TRUE, disabled=TRUE),
          bsButton("database2",
                   label = "Database 2: 2007 - 2021",
                   style = "success", type='toggle'),
          bsButton("database3",
                   label = "Database 3: 2003 - 2006",
                   style = "success", type='toggle'),
          bsButton("database4",
                   label = "Database 4: 1995 - 2002",
                   style = "success", type='toggle'),
          bsButton("PTB_visualization", 
                   label = "PTB Incidence 1995-2021", 
                   style = "success", type='toggle')
          )
      )
    ),    
    
    fluidRow(
      div(
        id = "database1_panel", 
        column(
          width = 6,
          introBox(
            uiOutput("box_pat"),
            linebreaks(20),
            uiOutput("box_pat3")
          )
        ),
        column(
          width = 6,
          introBox(
            uiOutput("box_pat2"),
            linebreaks(18),
            uiOutput("box_pat4")
          )
        )
      )
    ),
    
    fluidRow(
      div(
        id = "database2_panel", 
        column(
          width = 6,
          introBox(
            uiOutput("box_pat5"),
            linebreaks(20),
            uiOutput("box_pat7")            
          )
        ),
        column(
          width = 6,
          introBox(
            uiOutput("box_pat6"),
            linebreaks(18),
            uiOutput("box_pat8")  
          )
        )
      )
    ),
    
    fluidRow(
      div(
        id = "database3_panel", 
        column(
          width = 6,
          introBox(
            uiOutput("box_pat9"),
            linebreaks(20),
            uiOutput("box_pat11")  
          )
        ),
        column(
          width = 6,
          introBox(
            uiOutput("box_pat10"),
            linebreaks(18),
            uiOutput("box_pat12")  
          )
        )
      )
    ),
    

    fluidRow(
      div(
        id = "database4_panel", 
        column(
          width = 6,
          introBox(
            uiOutput("box_pat13"),
            linebreaks(20),
            uiOutput("box_pat15")  
          )
        ),
        column(
          width = 6,
          introBox(
            uiOutput("box_pat14"),
            linebreaks(18),
            uiOutput("box_pat16")  
            
          )
        )
      )
    ),
    
    fluidRow(
      div(
        id = "PTB_visualization_panel", 
        column(
          width = 6,
          introBox(
            uiOutput("box_pat17")
          )
        )
      )
    ),
    
    # tags$script(HTML('
    #           $(document).ready(function() {
    #           $("header").find("nav").append(\'<div class="navCenter"> CDC Wonder Explorer </div>\');
    #           })
    #           ')),
    tags$head(
      # Include our custom CSS
      includeCSS("styles.css"),
    ),
    useShinyjs()
    # box(plotOutput("distPlot"))
  )
)
