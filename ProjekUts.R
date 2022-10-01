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
          h1("Data")
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

server <- function(input, output){
  
}


shinyApp(ui =ui, server = server)