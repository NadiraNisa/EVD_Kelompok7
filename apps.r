library(shiny)
library(ggplot2)
library(plotly)
library(gridExtra)
library(bslib)
library(corrplot)
library(readxl)

## --------------------------------------- UI ---------------------------------------
ui <- fluidPage(
    titlePanel("Ukuran Pemusatan dan Penyebaran Data"),
    sidebarLayout(
      # sidebar
      sidebarPanel(
        # sidebar
        selectInput("select", h3("Pilih Data"), choices = list("Upload Data" = 1,
                                                               "Data Tunggal" = 2, "Data Kelompok" = 3), selected = 1),
  
        fileInput("file","Pilih file Excel (.xlsx)")),
      
      
    #main  
    mainPanel(
      tabsetPanel(
        #tab 1
        tabPanel(
          #plot
          title = "Plot",
          plotlyOutput(outputId = "plot", width = "100%", height = "100%"),
          #tabel
          tableOutput(outputId = "table")
        ),
        tabPanel(
          # information
          title = "Information",
          textOutput(outputId = "info")
        )
      )
    )
)
)


server<-function(input, output) {
  
  data<-reactive({
    req(input$file)
    x<-read_excel(input$file$datapath)
    return(x)
  })
  
  output$data<-renderTable({
    data()
  })
  
  output$summary<-renderPrint({
    summary(data())
  })
  
  output$boxplot<-renderPlot({
    boxplot(data())
  })
  
  output$corrplot<-renderPlot({
    corrplot(cor(data()))
  })
}

#Menjalankan aplikasi rshiny
shinyApp(ui, server)
