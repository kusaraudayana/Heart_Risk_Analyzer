library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(lubridate)

# Load data
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
heart_data <- read.csv("Heartdiseaseclean.csv")

# Convert relevant columns to factors
heart_data$Gender <- as.factor(heart_data$Gender)
heart_data$Angina <- as.factor(heart_data$Angina)
heart_data$Fasting_Bloodsugar <- as.factor(heart_data$Fasting_Bloodsugar)
heart_data$RestingECG <- as.factor(heart_data$RestingECG)
heart_data$ExerciseAngina <- as.factor(heart_data$ExerciseAngina)
heart_data$ST_Slope <- as.factor(heart_data$ST_Slope)
heart_data$HeartDisease <- as.factor(heart_data$HeartDisease)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "HeartRisk Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    ),
    selectInput("genderInput", "Select Gender:",
                choices = c("All", levels(heart_data$Gender))),
    sliderInput("ageInput", "Age Range:",
                min = min(heart_data$Age),
                max = max(heart_data$Age),
                value = c(min(heart_data$Age), max(heart_data$Age)))
  ),
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Introduction",
                    tags$div(style = "font-size:16px;line-height:1.7;",
                             tags$ul(
                               tags$li(tags$b(" Chest Pain Type (Angina)"),
                                       tags$ul(
                                         tags$li(tags$b("Typical:"), " Chest pain during exercise or stress, relieved by rest."),
                                         tags$li(tags$b("Atypical:"), " Chest pain that doesn't follow the usual pattern."),
                                         tags$li(tags$b("Non-Anginal:"), " Pain not related to the heart (e.g., stomach or muscle pain)."),
                                         tags$li(tags$b("Asymptomatic:"), " No chest pain, but still may have heart disease.")
                                       )
                               ),
                               tags$li(tags$b(" Resting Blood Pressure (in mm Hg)"),
                                       tags$ul(
                                         tags$li(tags$b("Normal:"), " Below 120"),
                                         tags$li(tags$b("Elevated:"), " 120–129"),
                                         tags$li(tags$b("High (Stage 1):"), " 130–139"),
                                         tags$li(tags$b("High (Stage 2):"), " 140 and above")
                                       )
                               ),
                               tags$li(tags$b(" Resting ECG (Electrocardiogram) Results"),
                                       tags$ul(
                                         tags$li(tags$b("Normal:"), " No signs of heart stress."),
                                         tags$li(tags$b("LVH (Left Ventricular Hypertrophy):"), " Thickened heart wall-can be a warning sign."),
                                         tags$li(tags$b("ST Abnormality:"), " Irregular heartbeat pattern, may suggest heart strain.")
                                       )
                               ),
                               tags$li(tags$b(" ST Segment Change After Exercise"),
                                       tags$ul(
                                         tags$li(tags$b("Up:"), " Heart responds well-usually a good sign."),
                                         tags$li(tags$b("Flat:"), " Could suggest restricted blood flow."),
                                         tags$li(tags$b("Down:"), " May point to heart problems under stress.")
                                       )
                               ),
                               tags$li(tags$b(" Exercise-Induced Angina"),
                                       tags$ul(
                                         tags$li(tags$b("Yes:"), " Chest pain occurs during physical activity."),
                                         tags$li(tags$b("No:"), " No chest pain with exercise.")
                                       )
                               )
                             )
                    )
                )
              )
      ),
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("heartDiseasePercentage"),
                valueBoxOutput("noHeartDiseasePercentage"),
                valueBoxOutput("avgCholesterolYes"),
                valueBoxOutput("avgCholesterolNo"),
                valueBoxOutput("avgRestingBPYes"),
                valueBoxOutput("avgRestingBPNo")
              ),
              fluidRow(
                box(
                  title = "Heart Disease Risk Across Categories", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 4,
                  selectInput("barplot_variable", "Select Variable:",
                              choices = c("Angina", "ST_Slope", "Gender", "Fasting_Bloodsugar", "RestingECG")),
                  plotOutput("groupedBarPlot", height = "300px")
                ),
                box(
                  title = "Scatterplot Analysis", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 4,
                  selectInput("y_var", "Select Y-axis Variable:",
                              choices = c("Cholesterol", "Resting_Bloodpressure", "Maximum_Heartrate")),
                  plotOutput("scatterPlot", height = "300px")
                ),
                box(
                  title = "Boxplot Analysis", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, width = 4,
                  selectInput("boxplot_variable", "Select Variable:",
                              choices = c("Resting_Bloodpressure", "Cholesterol")),
                  plotOutput("boxplots", height = "300px")
                )
              )
      ),
      # Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Description", status = "info", solidHeader = TRUE, width = 12,
                    p("Cardiovascular diseases (CVDs) remain the leading cause of mortality worldwide, responsible for nearly 31% of global deaths. Early detection and accurate risk assessment are crucial in preventing severe outcomes such as heart attacks and strokes. This dataset provides key health indicators, including age, cholesterol levels, blood pressure, and electrocardiogram results, to help analyze patterns and factors influencing heart disease. By leveraging these variables, we can gain valuable insights into CVD risks and enhance predictive modeling, ultimately contributing to better diagnosis, treatment, and prevention strategies.")
                ),
                box(title = "Heart Disease Data", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    DT::dataTableOutput("dataTable")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filteredData <- reactive({
    data <- heart_data
    if (input$genderInput != "All") {
      data <- data %>% filter(Gender == input$genderInput)
    }
    data <- data %>% filter(Age >= input$ageInput[1] & Age <= input$ageInput[2])
    return(data)
  })
  
  # Value Boxes (keep as is, only color adjustment if needed)
  output$heartDiseasePercentage <- renderValueBox({
    heart_disease_pct <- filteredData() %>%
      group_by(HeartDisease) %>%
      summarise(count = n()) %>%
      mutate(pct = count / sum(count) * 100) %>%
      filter(HeartDisease == "Yes") %>%
      pull(pct)
    valueBox(paste0(round(heart_disease_pct, 2), "%"),
             "Patients with Heart Disease",
             icon = icon("heart"),
             color = "red")
  })
  
  output$noHeartDiseasePercentage <- renderValueBox({
    no_heart_disease_pct <- filteredData() %>%
      group_by(HeartDisease) %>%
      summarise(count = n()) %>%
      mutate(pct = count / sum(count) * 100) %>%
      filter(HeartDisease == "No") %>%
      pull(pct)
    valueBox(paste0(round(no_heart_disease_pct, 2), "%"),
             "Patients without Heart Disease",
             icon = icon("heart"),
             color = "blue")
  })
  
  output$avgCholesterolYes <- renderValueBox({
    avg_cholesterol_yes <- filteredData() %>%
      filter(HeartDisease == "Yes") %>%
      summarise(mean_cholesterol=mean(Cholesterol, na.rm=TRUE)) %>%
      pull(mean_cholesterol)
    valueBox(round(avg_cholesterol_yes, 2),
             "Avg. Cholesterol (With Heart Disease)",
             icon=icon("prescription-bottle"),
             color="red")
  })
  
  output$avgCholesterolNo <- renderValueBox({
    avg_cholesterol_no <- filteredData() %>%
      filter(HeartDisease == "No") %>%
      summarise(mean_cholesterol=mean(Cholesterol, na.rm=TRUE)) %>%
      pull(mean_cholesterol)
    valueBox(round(avg_cholesterol_no, 2),
             "Avg. Cholesterol (Without Heart Disease)",
             icon=icon("prescription-bottle"),
             color="blue")
  })
  
  output$avgRestingBPYes <- renderValueBox({
    avg_bp_yes <- filteredData() %>%
      filter(HeartDisease == "Yes") %>%
      summarise(mean_bp=mean(Resting_Bloodpressure, na.rm=TRUE)) %>%
      pull(mean_bp)
    valueBox(round(avg_bp_yes, 2),
             "Avg. Resting Blood Pressure (With Heart Disease)",
             icon=icon("stethoscope"),
             color="red")
  })
  
  output$avgRestingBPNo <- renderValueBox({
    avg_bp_no <- filteredData() %>%
      filter(HeartDisease == "No") %>%
      summarise(mean_bp=mean(Resting_Bloodpressure, na.rm=TRUE)) %>%
      pull(mean_bp)
    valueBox(round(avg_bp_no, 2),
             "Avg. Resting Blood Pressure (Without Heart Disease)",
             icon=icon("stethoscope"),
             color="blue")
  })
  
  # Grouped Bar Plot with blue/red
  output$groupedBarPlot <- renderPlot({
    ggplot(filteredData(), aes(x=get(input$barplot_variable), fill=HeartDisease)) +
      geom_bar(position="dodge") +
      scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
      labs(title=paste("Heart Disease by", input$barplot_variable),
           x=input$barplot_variable,
           y="Count",
           fill="Heart Disease") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  })
  
  # Scatter Plot with dropdown for y variable and blue/red colors
  output$scatterPlot <- renderPlot({
    ggplot(filteredData(), aes_string(x="Age", y=input$y_var, color="HeartDisease")) +
      geom_point() +
      geom_smooth(method="lm", se=FALSE) +
      scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
      labs(title=paste(input$y_var, "vs Age"),
           x="Age",
           y=input$y_var,
           color="Heart Disease") +
      theme_minimal()
  })
  
  # Boxplots with blue/red
  output$boxplots <- renderPlot({
    ggplot(filteredData(), aes(x=HeartDisease, y=get(input$boxplot_variable), fill=HeartDisease)) +
      geom_boxplot() +
      scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
      labs(title=paste("Boxplot of", input$boxplot_variable,"by Heart Disease"),
           x="Heart Disease",
           y=input$boxplot_variable,
           fill="Heart Disease") +
      theme_minimal()
  })
  
  # Data Table
  output$dataTable <- DT::renderDataTable({
    datatable(filteredData(), options=list(pageLength=10))
  })
}

shinyApp(ui=ui, server=server)
