# Install necessary packages if not installed
# install.packages(c("shiny", "ggplot2"))

# Load required libraries
library(shiny)
library(shinyjs)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(cowplot)

# Sample data for demonstration
data <- data.frame(
  Group = rep(c("A", "B", "C"), each = 10),
  Value = rnorm(30)
)
modus <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# choice options data
choice_options <- c("Sample Data", "Create your own", "Supply numeric values")

# Define UI
ui <- navbarPage(
  title="Mean, Median, Modus",
  tabPanel("Dashboard",
    useShinyjs(),  # Initialize shinyjs
    
    # plotOutput("first_plot", click = "plot_click"),
  
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
            value = "7 7.5 7.5 8 8.5 8.5 8.5 9"
          )
        ),
      
        conditionalPanel(
          condition = "input.selected_data == 'Sample Data'",
          sliderInput("slider_input", "Select a Range:", min = 10, max = 100, value = 80)
        ),
      
        # Button to generate visualization
        # actionButton("generate_btn", "Generate Visualization"),
      
        # Button to save visualization
        downloadButton("save_btn", "First Plot"),
        
        downloadButton("save_btn1", "Second Plot")
      ),
    
      # Show a plot in the main panel
      mainPanel(
        h3("Summary Data"),
        plotOutput("first_plot", click="plot_click"),
        h3("Descriptive Statistics:"),
        DTOutput("summary_table"),  # Output for the summary table,
        h3("Histogram"),
        plotOutput("plot")
      )
    )
  ),
  #tabPanel("About",
  #   fluidPage(
  #     # Add content for the "About" tab if needed
  #     h2("About this dashboard"),
  #     p("This is a Shiny dashboard example."),
  #     p("Feel free to customize it based on your needs.")
  #   )
  #)
)

# Define server logic
server <- function(input, output) {
  # Reactive expression for the selected data
  selected_data <- reactive({
    if (input$selected_data == "Create your own") {
      selected_list <- c()
      #observeEvent(input$plot_click, {
      #  clicked_point <- nearPoints(data, input$plot_click, allRows = TRUE, threshold = 5, maxpoints = 1)
      #})
      #selected_list <<- c(selected_list, clicked_point$X)
      data <- selected_list
      return(selected_list)
    } else if (input$selected_data == "Supply numeric values") {
      # Parse space-separated numeric values
      numeric_values <- as.numeric(strsplit(input$custom_input, " ")[[1]])
      data <- numeric_values
      return(numeric_values)
    } else if (input$selected_data == "Sample Data") {
      excel_data <- read_excel("D:/MAGISTER/SEMESTER 3/evd/irsyifaproject/tinggi badan.xlsx")
      array_data <- excel_data %>%
        select("Tinggi_Badan")
      
      max_data <- input$slider_input
      result_data <- array_data$Tinggi_Badan[1:max_data]
      data <- result_data
      return(result_data)
    }
  })
  
  
  #generate mean median
  output$first_plot <- renderPlot({
    
    #observeEvent(input$plot_click, {
     # if (input$selected_data == "Create your own") {
     #   clicked_point <- nearPoints(data, input$plot_click, allRows = TRUE, threshold = 5, maxpoints = 1)
     #   
        # Update the selected_list based on X-axis value
     #   selected_list(c(selected_list(), clicked_point$X))
     # }
    #})
    # 
    
    if (!is.null(selected_data())) {
      dataset <- data.frame(x=selected_data())
      mean_value <- mean(dataset$x)
      mean_value_str <- sprintf("%.2f", mean_value)
      median_value <-median(dataset$x, na.rm=TRUE)
      median_value_str <- sprintf("%.2f", median_value)
      
      ggplot(data=dataset, aes(x = x, y = 1)) +
        geom_point(aes(shape = "Data"), size = 4, color="blue") +
        geom_point(aes(x = mean_value, y = 1, shape = "Mean"), size = 4, color="red") +
        geom_point(aes(x = median_value, y = 1, shape = "Median"), size = 4, color="green") +
        labs(title = "",
             x = "",
             y = "") +
        scale_shape_manual(
          name = "",
          values = c("Data" = 16, "Mean" = 17, "Median" = 18),
          labels = c("Data", paste("Mean (", mean_value_str, ")"), paste("Median (", median_value_str, ")")),
          guide = guide_legend(override.aes = list(shape = c(16, 17, 18), color = c("blue", "red", "green")))
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
    }
  })
  
  # Generate summary table
  output$summary_table <- renderDT({
    if (!is.null(selected_data())) {
      # Convert selected_data() to a data frame
      data_frame <- data.frame(x = selected_data())
      summary_data <- data_frame %>%
        summarise(
          Mean = mean(x),
          Median = median(x, na.rm = TRUE),
          StdDeviasi = sd(x),
          Min = min(x),
          Max = max(x),
          modus = modus(x)
        )
      datatable(summary_data, options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  
  # Generate histogram plot
  output$plot <- renderPlot({
    if (!is.null(selected_data())) {
      dataset <- data.frame(x=selected_data())
      histogram_plot <- ggplot(dataset, aes(x = x)) +
        geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.7, color = "black", fill = "steelblue") +
        labs(title = "",
             x = "",
             y = "") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "black"),
          axis.ticks.x = element_blank()
        )
      
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
  
  # Save plot as PNG
  output$save_btn <- downloadHandler(
    filename = function() {
      paste("plot_", input$selected_data, "_distribution.png", sep = "")
    },
    content = function(file) {
      ggsave(file, output$first_plot())
    }
  )
  # Save plot as PNG
  output$save_btn1 <- downloadHandler(
    filename = function() {
      paste("plot_", input$selected_data, "_histogram.png", sep = "")
    },
    content = function(file) {
      ggsave(file, output$plot())
    }
  )
}

# Run the application
shinyApp(ui, server)
