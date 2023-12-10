# Install necessary packages if not installed
# install.packages(c("shiny", "ggplot2"))

# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(cowplot)

modus <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

multiple_modus <- function(x) {
  tab <- tabulate(match(x, x))
  max_freq <- max(tab)
  modes <- unique(x[tab == max_freq])
  return(modes)
}


# Sample data
set.seed(123)
initial_data <- data.frame(
  x = c(NULL),
  y = c(NULL)
)


# choice options data
choice_options <- c("Contoh Data", "Input Mandiri", "Input Nilai Numerik")

# Define UI
ui <- navbarPage(
  title="Mean, Median, Modus",
  tabPanel("Dashboard",
           # Sidebar layout with input and output definitions
           sidebarLayout(
             sidebarPanel(
               # Dropdown for selecting data
               selectInput(
                 "selected_data", 
                 "Pilih Data:", 
                 choices = choice_options
               ),
               
               # Conditional panel for additional inputs based on selection
               conditionalPanel(
                 condition = "input.selected_data == 'Input Nilai Numerik'",
                 textInput(
                   "custom_input", 
                   "masukkan nilai sampel, pisahkan dengan spasi:",
                   value = "7 8 8.5 9 9 9 10 10 10.2 10.5"
                 )
               ),
               
               conditionalPanel(
                 condition = "input.selected_data == 'Contoh Data'",
                 sliderInput("slider_input", "Atur Rentang:", min = 10, max = 200, value = 50)
               ),
               
               h4("Pilihlah:"), 
               
               checkboxInput("show_descriptive_statistics", "Tampilkan Statistik Deskriptif", value = TRUE),
               checkboxInput("show_histogram_boxplot", "Tampilkan Histogram dan Boxplot ", value = TRUE),
               
               # Button to save visualization
               downloadButton("save_btn", "Plot Pertama"),
               conditionalPanel(
                 condition = "input.show_histogram_boxplot == true",
                 downloadButton("save_btn1", "Histogram dan Boxplot"),
               ),
             ),
             
             # Show a plot in the main panel
             mainPanel(
               h3("Ringkasan Data"),
               plotOutput("first_plot", click="plot_click"),
               # Conditional panel for additional inputs based on selection
               conditionalPanel(
                 condition = "input.show_descriptive_statistics == true",
                 h3("Statistik Deskriptif:"),
                 DTOutput("summary_table"),
               ),
               conditionalPanel(
                 condition = "input.show_histogram_boxplot == true",
                 h3("Histogram dan Boxplot"),
                 plotOutput("plot")
               ),
             )
           )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive values for storing data
  rv <- reactiveValues(data = initial_data)
  
  # Click event to add points
  observeEvent(input$plot_click, {
    if (input$selected_data == "Input Mandiri") {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      rv$data <- rbind(rv$data, new_point)
    }
  })
  
  
  # Reactive expression for the selected data
  selected_data <- reactive({
    if (input$selected_data == "Input Mandiri") {
      return(rv$data$x)
    } else if (input$selected_data == "Input Nilai Numerik") {
      # Parse space-separated numeric values
      numeric_values <- as.numeric(strsplit(input$custom_input, " ")[[1]])
      return(numeric_values)
    } else if (input$selected_data == "Contoh Data") {
      excel_data <- read_excel("D:/Nadira Nisa Alwani/Pasca Sarjana/Kuliah/semester 3/eksplorasi dan visualisasi/dashboard/data/data kelompok.xlsx")
      array_data <- excel_data %>%
        select("nilai")
      
      max_data <- input$slider_input
      result_data <- array_data$nilai[1:max_data]
      return(result_data)
    }
  })
  
  
  summary_plot <- reactive({
    if (!is.null(selected_data())) {
      dataset <- data.frame(x = selected_data())
      mean_value <- mean(dataset$x)
      mean_value_str <- sprintf("%.2f", mean_value)
      median_value <- median(dataset$x, na.rm = TRUE)
      median_value_str <- sprintf("%.2f", median_value)
      
      mode_values <- multiple_modus(dataset$x)
      mode_values_str <- lapply(mode_values, function(mode_value) sprintf("%.2f", mode_value))
      
      # Create a data frame for mode values
      mode_data <- data.frame(x = as.numeric(mode_values), y = 1, label = "Modus")
      
      ggplot(data = dataset, aes(x = x, y = 1)) +
        geom_point(aes(shape = "Data"), size = 4, color = "#6d6942") +
        geom_point(aes(x = mean_value, y = 1, shape = "Mean"), size = 4, color = "#baae00") +
        geom_point(aes(x = median_value, y = 1, shape = "Median"), size = 4, color = "#249bc0") +
        geom_point(data = mode_data, aes(x = x, y = y, shape = label), size = 4, color = "#ff0000") +
        labs(title = "", x = "", y = "") +
        scale_shape_manual(
          name = "",
          values = c("Data" = 16, "Mean" = 17, "Median" = 18, "Modus" = 15),
          labels = c(
            "Data", 
            paste("Rataan (", mean_value_str, ")"), 
            paste("Nilai Tengah (Q2) (", median_value_str, ")"),
            paste("Modus (", paste(mode_values_str, collapse = ", "), ")")
          ),
          guide = guide_legend(override.aes = list(shape = c(16, 17, 18, 15), color = c("#6d6942", "#baae00", "#249bc0", "#ff0000")))
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#F2F3F8"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
        )
    } else {
      ggplot() +
        geom_blank() +
        scale_x_continuous(limits = c(0, 100)) +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "#F2F3F8"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
        )
    }
  })
  
  summary_table <- reactive({
    if (!is.null(selected_data())) {
      data_frame <- data.frame(x = selected_data())
      summary_data <- data_frame %>%
        summarise(
          mean = round(mean(x),3),
          Q1 = quantile(x, probs=0.25),
          median = round(median(x, na.rm = TRUE),3),
          Q3 = quantile(x, probs=0.75),
          standard_deviation = round(sd(x),3),
          min = round(min(x),3),
          max = round(max(x),3),
          mode = ifelse(length(multiple_modus(x)) == 1, multiple_modus(x)[1], paste(multiple_modus(x), collapse = ", "))
        )
      # Customize column names
      custom_column_names <- c("Rataan", "Q1", "Nilai tengah (Q2)", "Q3", "Standar deviasi", "Min", "Maks", "Modus")
      names(summary_data) <- custom_column_names
      
      datatable(summary_data, options = list(dom = 't'), rownames = FALSE)
    } else {
      empty_table <- data.frame(
        mean = NA,
        Q1 = NA,
        median = NA,
        Q3 = NA,
        standard_deviation = NA,
        min = NA,
        max = NA,
        mode = NA
      )
      # Customize column names
      custom_column_names <- c("Mean", "Q1", "Nilai Tengah (Q2)", "Q3", "Standard Deviation", "Min", "Maks", "Modus")
      names(empty_table) <- custom_column_names
      
      datatable(empty_table, options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  hist_plot <- reactive({
    if (!is.null(selected_data())) {
      dataset <- data.frame(x=selected_data())
      mean_value <- mean(dataset$x)
      mean_value_str <- sprintf("%.2f", mean_value)
      median_value <-median(dataset$x, na.rm=TRUE)
      median_value_str <- sprintf("%.2f", median_value)
      mode_values <- multiple_modus(dataset$x)
      mode_values_str <- lapply(mode_values, function(mode_value) sprintf("%.2f", mode_value))
      
      # Create a data frame for mode values
      mode_data <- data.frame(x = as.numeric(mode_values), y = 1, label = "Modus")
      
      # Define custom colors
      custom_colors <- c(Mean = "#baae00", Median = "#249bc0", Modus = "#ff0000")
      
      histogram_plot <- ggplot(dataset, aes(x = x)) +
        geom_histogram(binwidth = 10, boundary = 0, color = "black", fill = "#fae7e7") +
        geom_vline(aes(xintercept = mean_value, color = "Mean"), linetype = "dashed") +
        geom_vline(aes(xintercept = median_value, color = "Median"), linetype = "solid") +
        geom_vline(data = mode_data, aes(xintercept = x, color = "Modus"), linetype = "dotdash") +
        
        labs(title = "",
             x = "",
             y = "") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          legend.box = "vertical",
          legend.box.background = element_rect(fill = "white", color = "black")
        ) +
        guides(color = guide_legend(title = "Keterangan")) +
        scale_color_manual(
          name = "",
          values = custom_colors,
          labels = c("Rataan", "Nilai Tengah", "Modus"),
          guide = guide_legend(override.aes = list(shape = c(16, 17, 18, 15)))
        )
      
      boxplot_plot <- ggplot(dataset, aes(x = x, y = 1)) +
        geom_boxplot(width = 0.01, color = "black", fill = "#87BC95") +
        labs(title = "",
             x = "",
             y = "") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
      
      plot_grid(histogram_plot, boxplot_plot, nrow = 2, rel_heights = c(3, 1))
    }
  })
  
  is_show_histogram_boxplot <- reactive({
    input$show_histogram_boxplot
  })
  
  
  # generate mean median
  output$first_plot <- renderPlot({
    summary_plot()
  })
  
  # Generate summary table
  output$summary_table <- renderDT({
    summary_table()
  })
  
  
  # Generate histogram plot
  output$plot <- renderPlot({
    hist_plot()
  })
  
  # Save plot as PNG
  output$save_btn <- downloadHandler(
    filename = function() {
      paste("plot_", input$selected_data, "_distribution.png")
    },
    content = function(file) {
      ggsave(file, summary_plot())
    }
  )
  
  
  # Save plot as PNG
  output$save_btn1 <- downloadHandler(
    filename = function() {
      paste("plot_", input$selected_data, "_histogram.png")
    },
    content = function(file) {
      if (is_show_histogram_boxplot()) {
        ggsave(file, hist_plot())
      }
    }
  )
}

# Run the application
shinyApp(ui, server)
