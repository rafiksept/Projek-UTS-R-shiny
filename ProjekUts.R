library(shiny)
library(shinydashboard)

#fungsi css untuk melakukan customisasi page
css <- function(){
  tags$head(
    tags$style(HTML("
                      .main-header .logo{
                      font-size:18px;
                      }
                      
                      li {
                        list-style-type: none;
                        margin: 15px 20px;
                        font-size:18px;
                      }
                      
                      .fa{
                      margin-right:3px;
                      width: 22px;
                      }
                      
                      .main-header .navbar{
                      background-color:black;
                      }
                      
                      .upload-file{
                      padding-left:20px;
                      padding-right:20px;
                      }
                      
                      .content-wrapper{
                      background-color:white;
                      }
                      
                      
                      "))
  )
}


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Kelompok 17"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName="Data", icon=icon("folder-open"), selected = TRUE),
      menuItem("Visualisasi", tabName="Visualisasi", icon=icon("chart-simple")),
      menuItem("Interpretasi", tabName="Interpretasi", icon=icon("panorama"))
    )

  ),
  dashboardBody(
      css(),
      tabItems(
        tabItem(
          tabName = "Data",
          fluidRow(
            tags$div(
              h1("Data"),
              fileInput("Upload", "choose your CSV file:", accept = ".csv"),
              uiOutput("files"),
              ),
              
              class = "upload-file",
              
            )

            
          ),

        tabItem(
          tabName = "Visualisasi",
          h1("Visualisasi")
        ),
        tabItem(
          tabName = "Interpretasi",
          h1("interpretasi")
        )
      ),
  )
)

server <- function(input, output, session){
  data <- reactive({
    req(input$Upload)
    read.csv(file = input$Upload$datapath)
    
    })
  
  
  output$files <- renderUI({
    if (is.null(input$Upload)){
      tags$style(HTML('
        #files{
        overflow-x:visible;
    }'
      ))
    } else {
      
      div(
        h3("RINGKASAN DATA"),
        div(
          renderTable(head(data(), n = 10)),
          p(paste("Ukuran Data:", nrow(data()), "x" , ncol(data())), style="font-weight:bold"),
          style = "overflow-x:auto;"
        )
        
      )
      
    }
    
  })
    
}

shinyApp(ui =ui, server = server)