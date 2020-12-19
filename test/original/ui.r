library(shiny); source("./global.R")

## change to 30mb
options(shiny.maxRequestSize = 30*1024^2)


navbarPage(
  title = "Embedded Standard Setting",
  theme = shinytheme("flatly"),
  # Data Import 
  #--------------------------------------------------------------
  tabPanel("Data Import",
           fluidRow(
             column(4, offset = 4,
                    fileInput("setups", "Choose xlsx(or xls) file for Setup", 
                              multiple = FALSE,
                              accept = c(".xlsx", "xls")
                              ),
                    br(),
                    actionButton("import", "IMPORT")
                    )
             ),
            br(),br(),br(),br(),
            
            column(10, offset = 3,
                    tableOutput("setting")
               ),
            br(),
            column(10, offset = 3,
                    tableOutput("itemtable1")
               ),
            br(),
            column(10, offset = 3,
                    tableOutput("itemtable2")
               )
            #uiOutput("descriptor")
           ),
  # ESS Individual Results
  #--------------------------------------------------------------
  tabPanel("ESS Individual Results",
    fluidRow(
      column(3,
        checkboxGroupInput("tests",
          "GCA",
          choices = NULL
          )
        ),
      column(3,
        radioButtons("loc",
          "Location",
          choices = 0
          )
        )
      ),
    fluidRow(
      column(1,
        materialSwitch(
          inputId = "WESS",
          label = "Count",
          value = FALSE, 
          status = "info"
          )
        ),
      column(1,
        "WESS")
      ),
    actionButton("run_tab1", "RUN",
      icon("paper-plane"), style = tabStyle),
      br(),
      br(),
    tabsetPanel(
      tabPanel(
        "Individual",
        
        use_waitress(),
        
        DT::DTOutput("indi")
        ),
      tabPanel(
        "Modal",
        withSpinner(
          DT::DTOutput("group_mode")
          )
        ),
      tabPanel(
        "Median",
        withSpinner(
          DT::DTOutput("group_median")
          )
        )
      )
    ),
  # Detailed ESS Group Results
  #--------------------------------------------------------------
  tabPanel("Detailed ESS Group Results",
           actionButton("update", "Update"),
    uiOutput("tabs")
    ),
  # Cut Score Summary
  #--------------------------------------------------------------
  tabPanel("Cut Score Summary",
           
           tabsetPanel(
              tabPanel(align="center",
               "Cut Page Table",
               htmlOutput("pagetb")
             ),
             tabPanel(align="center",
               "Cut Page Plot",
                plotOutput("pagePlot1"),
                plotOutput("pagePlot2"),
                plotOutput("pagePlot3")
               )
             )
    ),
  # Item Review
  #--------------------------------------------------------------
  tabPanel("Item Review",
    br(),
    br(),
           DT::dataTableOutput("review")
  ),
  # Report
  #--------------------------------------------------------------
  tabPanel("Report",
    actionButton("run_tab4", "RUN",
      icon("paper-plane"),
      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    downloadButton("report", "Download"),
    
    DT::DTOutput("ireview"),
  ),
  # Cut Score Validity
  #--------------------------------------------------------------
  tabPanel("Cut Score Validity",
    sidebarPanel(
      title = "",
      fileInput(
        "val_data",
        "Choose xlsx(or xls) file for Setup",
        multiple = FALSE,
        accept = c(".xlsx", "xls")
        ),
      actionButton("val_import", "IMPORT"),
      width = 2
    ),
    mainPanel(
      DT::DTOutput("valid"),
      width = 10)
  )
) # Shiny Ui
#############################################################################