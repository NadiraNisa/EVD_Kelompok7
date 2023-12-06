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

# Sample data
set.seed(123)
initial_data <- data.frame(
  x = c(NULL),
  y = c(NULL)
)


# choice options data
choice_options <- c("Sample Data", "Create your own", "Supply numeric values")

# Define UI
ui <- navbarPage(
  title="R Shiny Dashboard",
  tabPanel("Dashboard",
           # Sidebar layout with input and output definitions
           sidebarLayout(
             sidebarPanel(
               # Dropdown for selecting data
               selectInput(
                 "selected_data", 
                 "Select Initial Distribution of Points:", 
                 choices = choice_options
               ),
               
               # Conditional panel for additional inputs based on selection
               conditionalPanel(
                 condition = "input.selected_data == 'Supply numeric values'",
                 textInput(
                   "custom_input", 
                   "Enter sample values, seperated by spaces:",
                   value = "7 8 8.5 9 9 9 10 10 10.2 10.5"
                 )
               ),
               
               conditionalPanel(
                 condition = "input.selected_data == 'Sample Data'",
                 sliderInput("slider_input", "Select a Range:", min = 10, max = 200, value = 50)
               ),
               
               h4("Options:"), 
               
               checkboxInput("show_descriptive_statistics", "Show Descriptive Statistics", value = TRUE),
               checkboxInput("show_histogram_boxplot", "Show Historgram and Boxplot ", value = TRUE),
               
               # Button to save visualization
               downloadButton("save_btn", "First Plot"),
               conditionalPanel(
                 condition = "input.show_histogram_boxplot == true",
                 downloadButton("save_btn1", "Second Plot"),
               ),
             ),
             
             # Show a plot in the main panel
             mainPanel(
               h3("Summary Data"),
               plotOutput("first_plot", click="plot_click"),
               # Conditional panel for additional inputs based on selection
               conditionalPanel(
                 condition = "input.show_descriptive_statistics == true",
                 h3("Descriptive Statistics:"),
                 DTOutput("summary_table"),
               ),
               conditionalPanel(
                 condition = "input.show_histogram_boxplot == true",
                 h3("Histogram"),
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
    if (input$selected_data == "Create your own") {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      rv$data <- rbind(rv$data, new_point)
    }
  })
  
  
  # Reactive expression for the selected data
  selected_data <- reactive({
    if (input$selected_data == "Create your own") {
      return(rv$data$x)
    } else if (input$selected_data == "Supply numeric values") {
      # Parse space-separated numeric values
      numeric_values <- as.numeric(strsplit(input$custom_input, " ")[[1]])
      return(numeric_values)
    } else if (input$selected_data == "Sample Data") {
      excel_data <- read_excel("D:/MAGISTER/SEMESTER 3/evd/irsyifa/data kelompok.xlsx")
      array_data <- excel_data %>%
        select("nilai")
      
      max_data <- input$slider_input
      result_data <- array_data$nilai[1:max_data]
      return(result_data)
    }
  })
  
  
  summary_plot <- reactive({
    if (!is.null(selected_data())) {
      dataset <- data.frame(x=selected_data())
      mean_value <- mean(dataset$x)
      mean_value_str <- sprintf("%.2f", mean_value)
      median_value <-median(dataset$x, na.rm=TRUE)
      median_value_str <- sprintf("%.2f", median_value)
      mode_value <- modus(dataset$x)
      mode_value_str <- sprintf("%.2f", mode_value)
      
      ggplot(data=dataset, aes(x = x, y = 1)) +
        geom_point(aes(shape = "Data"), size = 4, color="blue") +
        geom_point(aes(x = mean_value, y = 1, shape = "Mean"), size = 4, color="red") +
        geom_point(aes(x = median_value, y = 1, shape = "Median"), size = 4, color="green") +
        geom_point(aes(x = mode_value, y = 1, shape = "Modus"), size = 4, color="yellow") +
        labs(title = "",
             x = "",
             y = "") +
        scale_shape_manual(
          name = "",
          values = c("Data" = 16, "Mean" = 17, "Median" = 18, "Modus" = 15),
          labels = c(
            "Data", 
            paste("Mean (", mean_value_str, ")"), 
            paste("Median (", median_value_str, ")"),
            paste("Modus (", mode_value_str, ")")
          ),
          guide = guide_legend(override.aes = list(shape = c(16, 17, 18, 15), color = c("blue", "red", "green", "yellow")))
        ) +
        theme_minimal() +
        theme(legend.position = "top") +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "grey"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
        )
    } else {
      ggplot() +
        geom_blank() +
        scale_x_continuous(limits = c(0, 100)) +
        theme_minimal() +
        theme(legend.position = "top") +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "grey"),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
        )
    }
  })
  
  summary_table <- reactive({
    if (!is.null(selected_data())) {
      # Convert selected_data() to a data frame
      data_frame <- data.frame(x = selected_data())
      summary_data <- data_frame %>%
        summarise(
          mean = round(mean(x),3),
          median = round(median(x, na.rm = TRUE),3),
          standard_deviation = round(sd(x),3),
          min = round(min(x),3),
          max = round(max(x),3),
          mode = round(modus(x),3)
        )
      datatable(summary_data, options = list(dom = 't'), rownames = FALSE)
    } else {
      empty_table <- data.frame(
        mean = NA,
        median = NA,
        standard_deviation = NA,
        min = NA,
        max = NA,
        mode = NA
      )
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
      mode_value <- modus(dataset$x)
      mode_value_str <- sprintf("%.2f", mode_value)
      
      histogram_plot <- ggplot(dataset, aes(x = x)) +
        geom_histogram(binwidth = 1, boundary = 0, color = "black", fill = "steelblue") +
        geom_point(aes(x = mean_value, y = 0, shape = "Mean"), size = 4, color="red") +
        geom_point(aes(x = median_value, y = 0, shape = "Median"), size = 4, color="green") +
        geom_point(aes(x = mode_value, y = 0, shape = "Modus"), size = 4, color="yellow") +
        labs(title = "",
             x = "",
             y = "") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        ) +
        guides(shape="none")
      
      boxplot_plot <- ggplot(dataset, aes(x = x, y = 1)) +
        geom_boxplot(width = 0.01, color = "black", fill = "steelblue") +
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
