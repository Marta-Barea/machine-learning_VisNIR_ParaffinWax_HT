# Loading required packages ----
library(shiny)
library(shinythemes)
library(openxlsx)
library(data.table)
library(stringr)
library(prospectr)
library(caret)
library(ranger)
library(reshape2)
library(ggplot2)
library(viridis)
library(fontawesome)

# Loading saved model ----
model <- readRDS("weighted_rf.rds")

# Preload test data ----
test_data <- openxlsx::read.xlsx("test_data.xlsx", sheet = 1)

# Server ----
server <- function(input, output) {
  data_submitted <- reactiveVal(FALSE)
  
  observeEvent(input$submitbutton, {
    data_submitted(TRUE)
  })
  
  uploaded_data <- reactive({
    req(input$file1)
    if (stringr::str_ends(input$file1$datapath, "csv")) {
      read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    } else if (stringr::str_ends(input$file1$datapath, "(xlsx|xls)")) {
      openxlsx::read.xlsx(input$file1$datapath, colNames = input$header, sheet = as.numeric(input$sheet))
    }
  })
  
  output$dataTable <- renderTable({
    req(data_submitted())
    head(uploaded_data())
  })
  
  output$spectraPlot <- renderPlot({
    req(data_submitted())
    df <- uploaded_data()
    df_numeric <- df[, sapply(df, is.numeric)]
    df_numeric <- cbind(Sample = df[, 1], df_numeric)
    df_numeric[,-1] <- msc(df_numeric[,-1])
    df_mean <- aggregate(. ~ Sample, df_numeric, mean)
    df_long <- melt(df_mean, id.vars = "Sample")
    df_long$variable <- as.numeric(as.character(df_long$variable))
    
    ggplot(data = df_long, aes(x = variable, y = value, color = Sample, group = Sample)) + 
      geom_line() +
      labs(x = "Wavelength (nm)", y = "Absorbance") +
      theme_minimal() + 
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 8),
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 10)) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      scale_x_continuous()  # Ensure a continuous X-axis
  })
  
  contents1 <- reactive({
    req(data_submitted())
    df <- uploaded_data()
    
    # Ensure columns are numeric
    df_numeric <- df[, sapply(df, is.numeric)]
    
    # Apply Savitzky-Golay filter
    df_sg <- savitzkyGolay(X = df_numeric, p = 3, w = 11, m = 1)
    
    # Modify column names in the Savitzky-Golay processed data to add "X" prefix if needed
    colnames(df_sg) <- ifelse(!startsWith(colnames(df_sg), "X"),
                              paste0("X", colnames(df_sg)),
                              colnames(df_sg))
    
    # Check for required variables
    expected_vars <- model$forest$independent.variable.names
    actual_vars <- colnames(df_sg)
    
    if (!all(expected_vars %in% actual_vars)) {
      stop("Error: Missing required variables: ", paste(setdiff(expected_vars, actual_vars), collapse = ", "))
    }
    
    # Make predictions
    predictions <- predict(model, data = df_sg)$predictions
    
    # Rename the column to "Hydroprocessing Grade"
    cbind.data.frame(Sample = rownames(df_sg), `Hydroprocessing Grade` = predictions)
  })
  
  output$predictions <- renderTable({
    req(data_submitted())
    isolate(contents1())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste0("test_data_", Sys.Date(), ".xlsx") },
    content = function(file) { openxlsx::write.xlsx(test_data, file, overwrite = TRUE) },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

# User Interface ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    .card { background-color: #f8f9fa; border-radius: 10px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); }
    .card-title { font-weight: bold; font-size: 1.5em; color: #343a40; margin-bottom: 10px; }
    .card-text { font-size: 1.1em; color: #6c757d; }
    .hero { background-color: #f4f7fc; text-align: center; padding: 50px 20px; margin-bottom: 20px; border-radius: 10px; }
    .hero h1 { font-size: 2.5em; font-weight: bold; color: #007bff; }
    .hero p { font-size: 1.3em; color: #6c757d; }
    .feature { text-align: center; padding: 20px; }
    .feature-icon { font-size: 3em; color: #007bff; margin-bottom: 10px; }
    .feature-text { font-size: 1.2em; color: #343a40; }
  "))),
  
  navbarPage("HydroGrade AI",
             
             # Home tab
             tabPanel("Home",
                      div(class = "hero",
                          h1("Welcome to HydroGrade AI"),
                          p("Advanced analysis of paraffins using AI and spectroscopy.")
                      ),
                      fluidRow(
                        column(12, div(class = "card",
                                       h3(class = "card-title", "What is HydroGrade AI?"),
                                       p(class = "card-text", "HydroGrade AI is an advanced application for analyzing paraffins using Vis-NIR spectroscopy. It employs powerful preprocessing techniques like Savitzky-Golay filtering and Multiplicative Scatter Correction (MSC) to clean and enhance spectral data."),
                                       p(class = "card-text", "The core of the app is a Weighted Random Forest model, designed for precision in hydroprocessing grade classification.")
                        ))
                      ),
                      fluidRow(
                        column(4, div(class = "feature",
                                      div(class = "feature-icon", icon("upload")),
                                      div(class = "feature-text", "Upload Data in .csv or .xlsx format.")
                        )),
                        column(4, div(class = "feature",
                                      div(class = "feature-icon", icon("chart-line")),
                                      div(class = "feature-text", "Visualize absorption spectra.")
                        )),
                        column(4, div(class = "feature",
                                      div(class = "feature-icon", icon("brain")),
                                      div(class = "feature-text", "Get AI-powered predictions.")
                        ))
                      )
             ),
             
             # Predictions tab (only shown after submission)
             tabPanel("Predictions",
                      titlePanel("Upload Your Data"),
                      sidebarLayout(
                        sidebarPanel(
                          downloadButton("downloadData", label = "Download Test Data"),
                          helpText("Download a sample dataset to test the application."),
                          tags$hr(),
                          fileInput("file1", "File Input:",
                                    multiple = FALSE,
                                    accept = c("text/csv", "xlsx/xls",
                                               "text/comma-separated-values,text/plain",
                                               ".csv",
                                               ".xlsx",
                                               ".xls")),
                          helpText("This app supports .csv, .txt, and .xlsx/.xls file formats."),
                          tags$hr(),
                          actionButton("submitbutton", "Submit"),
                          helpText("Click the 'Submit' button after uploading your data."),
                          tags$hr(),
                          checkboxInput("header", "Header", TRUE),
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          radioButtons("sheet", "Sheet",
                                       choices = c("Sheet 1" = 1,
                                                   "Sheet 2" = 2,
                                                   "Sheet 3" = 3),
                                       selected = 1),
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head")
                        ),
                        mainPanel(
                          # Title for uploaded data
                          conditionalPanel(
                            condition = "output.dataTable != null && input.submitbutton > 0",
                            tags$h3("Uploaded Data", style = "font-weight: bold; font-size: 1.5em; margin-top: 20px;")
                          ),
                          tableOutput("dataTable"),
                          
                          # Title and Spectra plot (after table)
                          conditionalPanel(
                            condition = "output.dataTable != null && input.submitbutton > 0",
                            tags$h3("Spectra Plot", style = "font-weight: bold; font-size: 1.5em; margin-top: 20px;"),
                            plotOutput("spectraPlot")
                          ),
                          
                          # Title for predictions
                          conditionalPanel(
                            condition = "output.predictions != null && input.submitbutton > 0",
                            tags$h3("Predictions (Hydroprocessing Grade)", style = "font-weight: bold; font-size: 1.5em; margin-top: 20px;")
                          ),
                          tableOutput("predictions")
                        )
                      )
             ),
             
             # Collaborators tab with improved preview of websites
             tabPanel("Collaborators",
                      fluidRow(
                        column(6, div(class = "card text-center",
                                      tags$img(src = "uca.png", alt = "University of Cádiz Logo", width = "80%"),
                                      h4("University of Cádiz"),
                                      p("Collaborating in advanced analytical methods and spectroscopy data analysis."),
                                      tags$a(href = "https://www.uca.es", target = "_blank", class = "btn btn-primary", "Visit Website")
                        )),
                        column(6, div(class = "card text-center",
                                      tags$img(src = "agr291.png", alt = "AGR-291 Logo", width = "80%"),
                                      h4("AGR-291 Research Group"),
                                      p("Experts in hydrocarbon characterization and spectroscopic techniques."),
                                      tags$a(href = "https://agr291.uca.es", target = "_blank", class = "btn btn-primary", "Visit Website")
                        ))
                      ),
                      fluidRow(
                        column(6, div(class = "card text-center",
                                      tags$img(src = "fundacioncepsa.png", alt = "Fundación Cepsa Logo", width = "80%"),
                                      h4("Cátedra Fundación Cepsa"),
                                      p("Providing financial support for the project's development."),
                                      tags$a(href = "https://catedrafundacioncepsa.uca.es", target = "_blank", class = "btn btn-primary", "Visit Website")
                        )),
                        column(6, div(class = "card text-center",
                                      tags$img(src = "nebux.png", alt = "Nebux Cloud Logo", width = "80%"),
                                      h4("Nebux Cloud S.L."),
                                      p("AI solutions for efficient data analysis."),
                                      tags$a(href = "https://nebux.cloud", target = "_blank", class = "btn btn-primary", "Visit Website")
                        ))
                      ),
                      fluidRow(
                        column(6, div(class = "card text-center",
                                      tags$img(src = "moeve.png", alt = "Moeve Global Logo", width = "80%"),
                                      h4("Moeve Global"),
                                      p("Providing financial support for the project's development."),
                                      tags$a(href = "https://www.moeveglobal.com/es/", target = "_blank", class = "btn btn-primary", "Visit Website")
                        )))
             )
  )
)

# Run the application ----
shinyApp(ui = ui, server = server)
