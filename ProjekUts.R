library(shiny)
library(shinydashboard)
library(dplyr)
library(RColorBrewer)
library(modeest)


#fungsi css untuk melakukan customisasi page
css <- function(){
  tags$head(
    tags$style(HTML("
                      .main-header .logo{
                      font-size:18px;
                      }
                      
                      .sidebar-menu li {
                        list-style-type: none;
                        margin: 15px 20px;
                        font-size:18px;
                      }
                      
                      .fa{
                      margin-right:3px;
                      width: 22px;
                      }
                      
                      .content-wrapper{
                      background-color:white;
                      }

                      .upload-file{
                      padding-left:20px;
                      padding-right:20px;
                      }
                      

                    
                      
                      
                      "))
  )
}

decsBox1 <- function(rata){
  renderInfoBox({
    infoBox(
      "Rata Rata", rata, icon = icon("list"),
      color = "purple", fill=TRUE
    )
  })
  
  
}
decsBox2 <- function(median){
  renderInfoBox({
    infoBox(
      "Median", median, icon = icon("list"),
      color = "green", fill=TRUE
    )
  })
  
  
}
decsBox3 <- function(modus){
  renderInfoBox({
    infoBox(
      "Modus", modus, icon = icon("list"),
      color = "red", fill=TRUE
    )
  })
  
  
}
decsBox4 <- function(min){
  renderInfoBox({
    infoBox(
      "Minimal", min, icon = icon("list"),
      color = "blue", fill=TRUE
    )
  })
  
  
}
decsBox5 <- function(max){
  renderInfoBox({
    infoBox(
      "Maksimal", max, icon = icon("list"),
      color = "orange", fill=TRUE
    )
  })
  
  
}
decsBox6 <- function(quant1){
  renderInfoBox({
    infoBox(
      "Quartile 1", quant1, icon = icon("list"),
      color = "olive", fill=TRUE
    )
  })
  
  
}
decsBox7 <- function(quant3){
  renderInfoBox({
    infoBox(
      "Quartile 3", quant3, icon = icon("list"),
      color = "fuchsia", fill=TRUE
    )
  })
  
  
}

selectInputCountry <- function(id, judul, pilih){
  selectInput(id, h5(judul),
              choices = list("Indonesia"="Indonesien", "Brunei" = "Brunei Darussalam", "Filipina"= "Philippinen", "Malaysia" = "Malaysia",
                             "Thailand"= "Thailand","Vietnam"="Vietnam", "Singapura" = "Singapur",
                             "Timor Leste" = "Timor-Leste", "Myanmar"="Myanmar"), selected=pilih)
}

# fungsi summaryBox
decsBox <- function(rata){
  renderInfoBox({
    infoBox(
      "Rata Rata", rata, icon = icon("list"),
      color = "purple", fill=TRUE
    )
  })
}

fileNotUpload <- function(){
  h1("Harap Upload File Terlebih dahulu atau masukkan File yang benar")
}


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Kelompok 16"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName="Data", icon=icon("folder-open"), selected = TRUE),
      menuItem("Statistika Deskriptif", tabName="Statistika_Deskriptif", icon=icon("list")),
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
            h1("Kelompok 16"),
            h3("Nama Kelompok"),
            tags$ul(
              tags$li("Rafik Septiana(162112133035)"),
              tags$li("Christeigen Theodore Suhalim(162112133055)"),
              tags$li("Muhammad Aqeela Addimaysqi(162112133050)")
            ), 
            h1("Data"),
            p("unduh di data di link berikut:", a("https://drive.google.com/drive/folders/19FK2vQ42rNeNqniaGLg64W7cpas3puKt?usp=sharing")),
            fileInput("Upload", "choose your CSV file:", accept = ".csv"),
            uiOutput("files"),
          ),
          class = "upload-file",
        )
      ),
      tabItem(
        tabName = "Statistika_Deskriptif",
        h1("Statistika Deskriptif"),
        selectInput(
          "intColumn",
          "Pilih Column:",
          c("WRI"= "WRI",
            "Exposure" = "Exposure",
            "Vulnerability" = "Vulnerability",
            "Susceptibility" = "Susceptibility",
            "Lack of Coping Capabilities" = "Lack.of.Coping.Capabilities",
            "Lack of Adaptive Capacities" = "Lack.of.Adaptive.Capacities"
          )
        ),
        infoBoxOutput("descrip"),
        infoBoxOutput("descrip1"),
        infoBoxOutput("descrip2"),
        infoBoxOutput("descrip3"),
        infoBoxOutput("descrip4"),
        infoBoxOutput("descrip5"),
        infoBoxOutput("descrip6")
      ),
      
      tabItem(
        tabName = "Visualisasi",
        uiOutput("visualisasi1"),
        uiOutput("visualisasi2"),
        uiOutput("visualisasi3")
      ),
      tabItem(
        tabName = "Interpretasi",
        uiOutput("interpretasi1")
      )
    ),
  ),
)



server <- function(input, output, session){
  data <- reactive({
    req(input$Upload)
    read.csv(file = input$Upload$datapath, sep =";")
  })
  
  validasi <- reactive({
    column_valid = c("Region","WRI","Exposure","Vulnerability","Susceptibility" ,"Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities","Year","Exposure.Category","WRI.Category","Vulnerability.Category","Susceptibility.Category")
    data_valid = FALSE
    req(input$Upload)
    col_upload = colnames(data())
    if (length(col_upload) == length(column_valid)){
      for( i in 1:length(col_upload)){
        if (!(col_upload[i] %in% column_valid)){
          data_valid = TRUE
        }
      }
    } else {
      data_valid = TRUE
    }
    return(data_valid)
  })
  
  
  plotHeatMap <- renderPlot(
    {
      plotdata1 <- data()%>%
        filter(Region %in% c(input$negara1)) %>%
        arrange(Region,Year)%>%
        select(Region, Year, WRI) %>%
        filter(Year >= 2017) 
      
      plotdata2 <- data()%>%
        filter(Region %in% c(input$negara2)) %>%
        arrange(Region,Year)%>%
        select(Region, Year, WRI) %>%
        filter(Year >= 2017) 
      
      plotdata3 <- data()%>%
        filter(Region %in% c(input$negara3)) %>%
        arrange(Region,Year)%>%
        select(Region, Year, WRI) %>%
        filter(Year >= 2017) 
      
      plotdata4 <- data()%>%
        filter(Region %in% c(input$negara4)) %>%
        arrange(Region,Year)%>%
        select(Region, Year, WRI) %>%
        filter(Year >= 2017) 
      
      plotdata5 <- data()%>%
        filter(Region %in% c(input$negara5)) %>%
        arrange(Region,Year)%>%
        select(Region, Year, WRI) %>%
        filter(Year >= 2017) 
      
      
      merge_data <- rbind(plotdata1, plotdata2, plotdata3, plotdata4, plotdata5) 
      
      plotdataWRI = matrix(merge_data$WRI, ncol = 5, nrow =5, byrow = TRUE)
      
      print(plotdataWRI)
      
      colnames(plotdataWRI) <- c(2017, 2018, 2019, 2020, 2021)
      negara <- c(input$negara1, input$negara2, input$negara3, input$negara4, input$negara5)
      for(i in 1:length(negara)){
        if(negara[i] == "Indonesien"){
          negara[i] = "Indonesia"
        } else if(negara[i] == "Philippinen"){
          negara[i] = "Filipina"
        }
      }
      rownames(plotdataWRI) <- negara
      
      
      heatmap(plotdataWRI, Rowv = NA, Colv = NA, margins = c(7, 7),scale ="row" , col = brewer.pal(n = 8, name =
                                                                                                     input$color))
    }
  )
  
  plotBarChart <- renderPlot(
    {
      
      
      plotdata <- data() %>% 
        group_by_(input$variabelX) %>%
        count_(input$variabelY) %>%
        na.omit()
      
      
      plotdataCount = matrix(plotdata$n, ncol = 5, nrow =5, byrow = TRUE)
      
      colnames(plotdataCount) <- c("High", "Low", "Medium", "Very High", "Very Low")
      rownames(plotdataCount) <- c("High", "Low", "Medium", "Very High", "Very Low")
      
      barplot(as.matrix(plotdataCount), xlab = input$variabelX, ylab = input$variabelY, col = brewer.pal(n = 5, name =input$color1))
    }
  )
  
  output$visualisasi1 <- renderUI({
    if(is.null(input$Upload) || (validasi()) ){
      fileNotUpload()
    } else {
      fluidRow(class = "heatmap", style = "padding-left:20px;padding-right:20px;",
               h3("Nilai WRI Negara Asia Tenggara (2017-2021)", style = "margin-left:10px;font-weight:bold;"),
               div(class = "heatmap-content",
                   fluidRow(
                     column(2,
                            selectInput("color", h5("Pilih Warna"),
                                        choices = list("Biru" = "Blues", "Merah" = "YlOrRd",
                                                       "Hijau" = "Greens"), selected = "Blues"),
                     ),
                     column(2,
                            selectInputCountry("negara1", "Negara 1", "Thailand")
                     ),
                     column(2,
                            selectInputCountry("negara2","Negara 2","Brunei Darussalam")
                     ),
                     column(2,
                            selectInputCountry("negara3","Negara 3","Philippinen")
                     ),
                     column(2,
                            selectInputCountry("negara4","Negara 4","Malaysia")
                     ),
                     column(2,
                            selectInputCountry("negara5","Negara 5","Indonesien")
                     ),
                     
                   ),
               ),
               plotHeatMap
      )
      
    }
  })
  
  output$descrip <- renderUI({
    rata2 <- mean(data()[, c(input$intColumn)], na.rm=TRUE)
    rataRound <- round(rata2, 2)
    decsBox1(rataRound)
  })
  
  output$descrip1 <- renderUI({
    median1 <- median(data()[, c(input$intColumn)], na.rm=TRUE)
    medianRound <- round(median1, 2)
    decsBox2(medianRound)
  })
  
  output$descrip2 <- renderUI({
    modus1 <- mfv1(data()[, c(input$intColumn)])
    modusRound <- round(modus1, 2)
    decsBox3(modusRound)
  })
  
  output$descrip3 <- renderUI({
    min1 <- min(data()[, c(input$intColumn)], na.rm=TRUE)
    minRound <- round(min1, 2)
    decsBox4(minRound)
  })
  
  output$descrip4 <- renderUI({
    max1 <- max(data()[, c(input$intColumn)], na.rm=TRUE)
    maxRound <- round(max1, 2)
    decsBox5(maxRound)
  })
  
  output$descrip5 <- renderUI({
    quantil1 <- quantile(data()[, c(input$intColumn)], prob=c(.25), na.rm=TRUE)
    quantil1Round <- round(quantil1, 2)
    decsBox6(quantil1Round)
  })
  
  output$descrip6 <- renderUI({
    quantil3 <- quantile(data()[, c(input$intColumn)], prob=c(.75), na.rm=TRUE)
    quantil3Round <- round(quantil3, 2)
    decsBox7(quantil3Round)
  })
  
  output$visualisasi2 <- renderUI({
    if(is.null(input$Upload) || (validasi())){
      return("")
    } else {
      fluidRow(
        h3("Stacked Bar Chart Setiap Variabel", style = "margin-left:10px;font-weight:bold;"),
        column(6,
               plotBarChart),
        column(6,
               box(
                 style="background-color:whitesmoke;",
                 h4("Customisasi Bar Chart", style="font-weight:bold;"),
                 selectInput("variabelX", h5("X"),
                             choices = list("Exposure" = "Exposure.Category", "WRI" = "WRI.Category",
                                            "Vulnerability" = "Vulnerability.Category", "Susceptibility"  = "Susceptibility.Category"), selected = "Vulnerability.Category"),
                 selectInput("variabelY", h5("Y"),
                             choices = list("Exposure" = "Exposure.Category", "WRI" = "WRI.Category",
                                            "Vulnerability" = "Vulnerability.Category", "Susceptibility"  = "Susceptibility.Category"), selected = "Exposure.Category"),
                 selectInput("color1", h5("Pilih Warna"),
                             choices = list("Orange"="Oranges","Biru" = "Blues", "Merah" = "YlOrRd"
                             ), selected = "Oranges")
                 
               )
        )
      )
    }
  })
  
  
  
  
  output$files <- renderUI({
    if (is.null(input$Upload) || (validasi())){
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
  
  output$visualisasi3 <- renderUI({
    if(is.null(input$Upload) || (validasi())){
      return("")
    } else {
      fluidRow(
        h3(textOutput("caption"), style = "margin-left:10px;font-weight:bold;"),
        sidebarLayout(
          sidebarPanel(
            selectInput("variable", "Variable:",
                        c("Exposure" = "Exposure",
                          "Vulnerability" = "Vulnerability",
                          "Susceptibility" = "Susceptibility",
                          "Lack.of.Coping.Capabilities" = "Lack.of.Coping.Capabilities",
                          "Lack.of.Adaptive.Capacities" = "Lack.of.Adaptive.Capacities")),
            
            checkboxInput("outliers", "Show outliers", TRUE)
            
          ),
          mainPanel(
            plotOutput("WRIPlot")
            
          )
        )
        
      )
    }
  })
  
  formulaText <- reactive({
    paste("WRI ~", input$variable)
  })
  
  output$caption <- renderText({
    paste("Scatter Plot" , formulaText())
  })
  
  output$WRIPlot <- renderPlot({
    print(!validasi())
    plot(as.formula(formulaText()),
         data = data(),
         outline = input$outliers,
         col = "#75AADB", pch = 19)
  })
  
  output$descrip <- renderUI({
    rata2 <- mean(data()[, c(input$intColumn)], na.rm=TRUE)
    rataRound <- round(rata2, 2)
    decsBox(rataRound)
  })
  
  formulaText1 <- reactive({
    paste("Heatmap :  Terlihat bahwa di dalam data : Pada grafik diatas, kita dapat melihat nilai WRI (world risk index) di 5 negara asia tenggara yaitu Indonesia, Brunei Darussalam, Thailand, Malaysia, Filipina. Di dalam heatmap tersebut, warna yang lebih gelap mempresentasikan nilai WRI yang semakin tinggi, warna yang lebih terang mempresentasikan nilai WRI yang semakin rendah.
              
              Brunei Darussalam : Terlihat pada periode 2017 sampai 2021, memiliki kenaikan pada nilai WRI. Hal ini menandakan bahwa risiko bencana yang terjadi di brunei darussalam semakin lama semakin tinggi. Indonesia : Terlihat bahwa pada tahun 2018 dan 2020, mengalami penurunan nilai WRI dari tahun sebelumnya. Sedangkan pada tahun 2021, warna nya semakin gelap yang memiliki arti bahwa terjadi kenaikan yang lebih tinggi dari tahun-tahun sebelumnya pda risiko bencana. Malaysia : Seperti Brunei Darussalam, malaysia pada periode 2017 sampai 2021, memiliki kenaikan nilai WRI yang semakin lama semakin tinggi. Hal ini menandakan bahwa risiko bencana yang terjadi di malaysia semakin lama semakin tinggi. Filipina : Tidak seperti 4 negara lainnya, filipina justru mengalami penurunan WRI pada periode 5 tahun. Hal ini menandakan bahwa nilai risiko bencana dari tahun ke tahun semakin rendah. Thailand: Thailand mengalami kenaikan dari tahun ke tahun pada tahun 2017-2021, hal ini menandakan bahwa risiko bencana di negara tersebut menjadi semakin tinggi.
              Vietnam : Terlihat pada periode 2017 sampai 2021, memiliki penurunan WRI di lihat dari warna yang semakin terang. Singapura : Pada tahun 2017 sampai 2021, nilai WRI negara singapura semakin lama semakin meningkat karena data mereka semakin gelap. Timor Leste: Nilai dari WRI Timor Leste tahun 2017 sampai 2021, cenderung untuk menurun, karena warna yang semakin terang. Myanmar : Nilai dari WRI myanmar 2017-2021, semakin kecil ditandai dengan daerah yang semakin lama semakin terang.")
  })
  output$interpretasi1 <- renderUI({
    if(is.null(input$Upload) || (validasi()) ){
      fileNotUpload()
    } else {
      div(
        h1("Interpretasi"),
        tags$ul(
          tags$li(
            p(formulaText1(), style="font-size:16px;")
          ),
          tags$li(
            p("
              Stacked Barchart : Pada data diatas, terlhat bahwa negara yang memiliki tingkat kesiapan dalam menghadapi bencana (Vulnerability) tinggi (high) lebih banyak memiliki tingkat resiko bencana yang tinggi juga (exposure). Untuk negara yang memiliki tingkat kesiapan menghadapi bencana yang rendah, lebih banyak memiliki tingkat resiko bencana yang rendah juga. untuk negara yang memiliki tingkat kesiapan menengah, maka akan memiliki resiko bencana lebih banyak pada tingkat medium dan very high atau sangat tinggi. untuk yang memiliki tingkat kesiapan sangat tinggi atau very high, lebih banyak memiliki resiko bencana di tingkat medium dan high. untul yang tingkat kesiapannya rendah, lebih banyak memiliki tingkat resiko bencana yang dari very low dan low.
              ", style="font-size:16px;")
          ),
          tags$li(
            " 
            Scatter plot:
          Melalui scatterplot WRI dan Exposure, dapat dilihat bahwa kenaikan WRI berbanding lurus dengan
          kenaikan exposure. Oleh sebab itu, kita dapat menginterpretasikan bahwa kedua variabel tersebut
          memiliki korelasi positif. Dalam scatterplot antara WRI dengan Vulnerability/Susceptibility/Lack
          of Coping Capabilities/Lack of Adaptive Capacities terlihat cenderung linear dengan kenaikan variabel
          vulnerability/Susceptibility/Lack of Coping Capabilities/Lack of Adaptive Capacities sedikit 
          meningkatkan variabel WRI.
            "
          )
        )
        
      )
    }
    
  })
  
}

shinyApp(ui =ui, server = server)