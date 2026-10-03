
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

hr_file <- "data/HR_Employee_Attrition_raw.csv"
telco_file <- "data/Telco_Customer_Churn_raw.csv"

if (!file.exists(hr_file)) {
  stop("HR dataset not found: ", hr_file)
}

if (!file.exists(telco_file)) {
  stop("Telco dataset not found: ", telco_file)
}

hr_raw <- read_csv(
  hr_file,
  show_col_types = FALSE,
  trim_ws = TRUE
)

telco_raw <- read_csv(
  telco_file,
  show_col_types = FALSE,
  trim_ws = TRUE
)

cat("\n====================================\n")
cat("DATASET VALIDATION\n")
cat("====================================\n")
cat("HR rows:", nrow(hr_raw), "\n")
cat("HR columns:", ncol(hr_raw), "\n")
cat("Telco rows:", nrow(telco_raw), "\n")
cat("Telco columns:", ncol(telco_raw), "\n")
cat("====================================\n\n")

if (nrow(hr_raw) != 1470) {
  stop(
    paste0(
      "Incorrect HR dataset. Expected 1470 rows but found ",
      nrow(hr_raw)
    )
  )
}

if (nrow(telco_raw) != 7043) {
  stop(
    paste0(
      "Incorrect Telco dataset. Expected 7043 rows but found ",
      nrow(telco_raw)
    )
  )
}

hr_raw <- hr_raw %>%
  mutate(
    AttritionFlag = if_else(
      Attrition == "Yes",
      1L,
      0L
    ),
    AgeBand = cut(
      Age,
      breaks = c(
        17,
        25,
        35,
        45,
        60
      ),
      labels = c(
        "18-25",
        "26-35",
        "36-45",
        "46-60"
      ),
      include.lowest = TRUE
    ),
    TenureBand = cut(
      YearsAtCompany,
      breaks = c(
        -1,
        2,
        5,
        10,
        Inf
      ),
      labels = c(
        "0-2 years",
        "3-5 years",
        "6-10 years",
        "11+ years"
      ),
      include.lowest = TRUE
    ),
    IncomeBand = cut(
      MonthlyIncome,
      breaks = c(
        -Inf,
        3000,
        6000,
        10000,
        Inf
      ),
      labels = c(
        "< 3k",
        "3k-6k",
        "6k-10k",
        "10k+"
      )
    )
  )

telco_raw <- telco_raw %>%
  mutate(
    TotalCharges = trimws(
      as.character(TotalCharges)
    ),
    TotalCharges = na_if(
      TotalCharges,
      ""
    ),
    TotalCharges = as.numeric(
      TotalCharges
    ),
    TotalCharges = if_else(
      tenure == 0 &
        is.na(TotalCharges),
      0,
      TotalCharges
    ),
    ChurnFlag = if_else(
      Churn == "Yes",
      1L,
      0L
    ),
    SeniorCitizenLabel = if_else(
      SeniorCitizen == 1,
      "Senior citizen",
      "Non-senior"
    ),
    TenureBand = cut(
      tenure,
      breaks = c(
        -1,
        6,
        12,
        24,
        48,
        72
      ),
      labels = c(
        "0-6 months",
        "7-12 months",
        "13-24 months",
        "25-48 months",
        "49-72 months"
      ),
      include.lowest = TRUE
    ),
    MonthlyChargeBand = cut(
      MonthlyCharges,
      breaks = c(
        -Inf,
        40,
        70,
        100,
        Inf
      ),
      labels = c(
        "< $40",
        "$40-$70",
        "$70-$100",
        "$100+"
      )
    ),
    ServiceCount = rowSums(
      across(
        c(
          PhoneService,
          MultipleLines,
          OnlineSecurity,
          OnlineBackup,
          DeviceProtection,
          TechSupport,
          StreamingTV,
          StreamingMovies
        ),
        ~ .x == "Yes"
      )
    )
  )

hr_departments <- sort(
  unique(
    hr_raw$Department
  )
)

hr_roles <- sort(
  unique(
    hr_raw$JobRole
  )
)

hr_overtime <- sort(
  unique(
    hr_raw$OverTime
  )
)

telco_contracts <- sort(
  unique(
    telco_raw$Contract
  )
)

telco_internet <- sort(
  unique(
    telco_raw$InternetService
  )
)

telco_payments <- sort(
  unique(
    telco_raw$PaymentMethod
  )
)

fmt_number <- function(
    x,
    digits = 0
) {
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return("—")
  }
  
  formatC(
    x,
    format = "f",
    digits = digits,
    big.mark = ","
  )
}

fmt_currency <- function(
    x,
    digits = 0
) {
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return("—")
  }
  
  paste0(
    "$",
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    )
  )
}

fmt_pct <- function(
    x,
    digits = 1
) {
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return("—")
  }
  
  paste0(
    formatC(
      x,
      format = "f",
      digits = digits
    ),
    "%"
  )
}

safe_mean <- function(x) {
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(
      NA_real_
    )
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}

kpi_card <- function(
    title,
    value
) {
  div(
    class = "kpi-card",
    div(
      class = "kpi-title",
      title
    ),
    div(
      class = "kpi-value",
      value
    )
  )
}

empty_plot <- function(
    message = "No records match the current filters."
) {
  ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = message,
      size = 5
    ) +
    xlim(
      0,
      1
    ) +
    ylim(
      0,
      1
    ) +
    theme_void()
}

apply_category_filter <- function(
    data,
    column,
    value
) {
  if (
    is.null(value) ||
    length(value) == 0 ||
    value == "All"
  ) {
    return(
      data
    )
  }
  
  data %>%
    filter(
      .data[[column]] == value
    )
}

hr_filtered <- function(
    data,
    department,
    role,
    age,
    years,
    overtime
) {
  data <- apply_category_filter(
    data,
    "Department",
    department
  )
  
  data <- apply_category_filter(
    data,
    "JobRole",
    role
  )
  
  data <- data %>%
    filter(
      Age >= age[1],
      Age <= age[2],
      YearsAtCompany >= years[1],
      YearsAtCompany <= years[2]
    )
  
  data <- apply_category_filter(
    data,
    "OverTime",
    overtime
  )
  
  data
}

telco_filtered <- function(
    data,
    contract,
    internet,
    tenure,
    charges,
    payment
) {
  data <- apply_category_filter(
    data,
    "Contract",
    contract
  )
  
  data <- apply_category_filter(
    data,
    "InternetService",
    internet
  )
  
  data <- data %>%
    filter(
      tenure >= tenure[1],
      tenure <= tenure[2],
      MonthlyCharges >= charges[1],
      MonthlyCharges <= charges[2]
    )
  
  data <- apply_category_filter(
    data,
    "PaymentMethod",
    payment
  )
  
  data
}

hr_filter_ui <- function(prefix) {
  tagList(
    selectInput(
      paste0(prefix, "_department"),
      "Department",
      choices = c(
        "All",
        hr_departments
      ),
      selected = "All"
    ),
    
    selectInput(
      paste0(prefix, "_role"),
      "Job Role",
      choices = c(
        "All",
        hr_roles
      ),
      selected = "All"
    ),
    
    sliderInput(
      paste0(prefix, "_age"),
      "Age Range",
      min = min(
        hr_raw$Age
      ),
      max = max(
        hr_raw$Age
      ),
      value = range(
        hr_raw$Age
      )
    ),
    
    sliderInput(
      paste0(prefix, "_years"),
      "Years at Company",
      min = min(
        hr_raw$YearsAtCompany
      ),
      max = max(
        hr_raw$YearsAtCompany
      ),
      value = range(
        hr_raw$YearsAtCompany
      )
    ),
    
    selectInput(
      paste0(prefix, "_overtime"),
      "Overtime",
      choices = c(
        "All",
        hr_overtime
      ),
      selected = "All"
    ),
    
    actionButton(
      paste0(prefix, "_reset"),
      "Reset",
      class = "btn-default"
    ),
    
    div(
      class = "filter-note",
      textOutput(
        paste0(
          prefix,
          "_filter_status"
        )
      )
    )
  )
}

telco_filter_ui <- function(prefix) {
  tagList(
    selectInput(
      paste0(prefix, "_contract"),
      "Contract Type",
      choices = c(
        "All",
        telco_contracts
      ),
      selected = "All"
    ),
    
    selectInput(
      paste0(prefix, "_internet"),
      "Internet / Service Type",
      choices = c(
        "All",
        telco_internet
      ),
      selected = "All"
    ),
    
    sliderInput(
      paste0(prefix, "_tenure"),
      "Tenure Range (months)",
      min = min(
        telco_raw$tenure
      ),
      max = max(
        telco_raw$tenure
      ),
      value = range(
        telco_raw$tenure
      )
    ),
    
    sliderInput(
      paste0(prefix, "_charges"),
      "Monthly Charges",
      min = min(
        telco_raw$MonthlyCharges
      ),
      max = max(
        telco_raw$MonthlyCharges
      ),
      value = range(
        telco_raw$MonthlyCharges
      )
    ),
    
    selectInput(
      paste0(prefix, "_payment"),
      "Payment Method",
      choices = c(
        "All",
        telco_payments
      ),
      selected = "All"
    ),
    
    actionButton(
      paste0(prefix, "_reset"),
      "Reset",
      class = "btn-default"
    ),
    
    div(
      class = "filter-note",
      textOutput(
        paste0(
          prefix,
          "_filter_status"
        )
      )
    )
  )
}

business_question_hr <- function(data) {
  if (
    nrow(data) == 0
  ) {
    return(
      "No records match the current filters."
    )
  }
  
  overtime_data <- data %>%
    group_by(
      OverTime
    ) %>%
    summarise(
      rate =
        mean(
          Attrition == "Yes"
        ) * 100,
      .groups = "drop"
    ) %>%
    arrange(
      desc(rate)
    )
  
  tenure_data <- data %>%
    group_by(
      TenureBand
    ) %>%
    summarise(
      rate =
        mean(
          Attrition == "Yes"
        ) * 100,
      .groups = "drop"
    ) %>%
    arrange(
      rate
    )
  
  paste0(
    "Business question: Does overtime and tenure provide a useful breakdown of observed attrition across departments? ",
    "The highest observed overtime attrition is ",
    overtime_data$OverTime[1],
    " at ",
    round(
      overtime_data$rate[1],
      1
    ),
    "%. The lowest observed tenure-band attrition is ",
    as.character(
      tenure_data$TenureBand[1]
    ),
    " at ",
    round(
      tenure_data$rate[1],
      1
    ),
    "%. These are descriptive associations, not causal conclusions."
  )
}

business_question_telco <- function(data) {
  if (
    nrow(data) == 0
  ) {
    return(
      "No records match the current filters."
    )
  }
  
  contract_data <- data %>%
    group_by(
      Contract
    ) %>%
    summarise(
      rate =
        mean(
          Churn == "Yes"
        ) * 100,
      .groups = "drop"
    ) %>%
    arrange(
      desc(rate)
    )
  
  tenure_data <- data %>%
    group_by(
      TenureBand
    ) %>%
    summarise(
      rate =
        mean(
          Churn == "Yes"
        ) * 100,
      .groups = "drop"
    ) %>%
    arrange(
      rate
    )
  
  paste0(
    "Business question: How do contract type and tenure jointly relate to observed churn, and which service/payment segment warrants further investigation? ",
    "The highest observed contract-level churn is ",
    contract_data$Contract[1],
    " at ",
    round(
      contract_data$rate[1],
      1
    ),
    "%. The lowest observed tenure-band churn is ",
    as.character(
      tenure_data$TenureBand[1]
    ),
    " at ",
    round(
      tenure_data$rate[1],
      1
    ),
    "%. These are descriptive associations, not causal conclusions."
  )
}

summary_table_hr <- function(data) {
  if (
    nrow(data) == 0
  ) {
    return(
      data.frame(
        Metric = character(),
        Mean = numeric(),
        Median = numeric(),
        Min = numeric(),
        Max = numeric()
      )
    )
  }
  
  tibble(
    Metric = c(
      "Age",
      "Monthly Income",
      "Years at Company",
      "Total Working Years",
      "Distance From Home"
    ),
    
    Mean = c(
      safe_mean(
        data$Age
      ),
      safe_mean(
        data$MonthlyIncome
      ),
      safe_mean(
        data$YearsAtCompany
      ),
      safe_mean(
        data$TotalWorkingYears
      ),
      safe_mean(
        data$DistanceFromHome
      )
    ),
    
    Median = c(
      median(
        data$Age,
        na.rm = TRUE
      ),
      median(
        data$MonthlyIncome,
        na.rm = TRUE
      ),
      median(
        data$YearsAtCompany,
        na.rm = TRUE
      ),
      median(
        data$TotalWorkingYears,
        na.rm = TRUE
      ),
      median(
        data$DistanceFromHome,
        na.rm = TRUE
      )
    ),
    
    Min = c(
      min(
        data$Age,
        na.rm = TRUE
      ),
      min(
        data$MonthlyIncome,
        na.rm = TRUE
      ),
      min(
        data$YearsAtCompany,
        na.rm = TRUE
      ),
      min(
        data$TotalWorkingYears,
        na.rm = TRUE
      ),
      min(
        data$DistanceFromHome,
        na.rm = TRUE
      )
    ),
    
    Max = c(
      max(
        data$Age,
        na.rm = TRUE
      ),
      max(
        data$MonthlyIncome,
        na.rm = TRUE
      ),
      max(
        data$YearsAtCompany,
        na.rm = TRUE
      ),
      max(
        data$TotalWorkingYears,
        na.rm = TRUE
      ),
      max(
        data$DistanceFromHome,
        na.rm = TRUE
      )
    )
  )
}

summary_table_telco <- function(data) {
  if (
    nrow(data) == 0
  ) {
    return(
      data.frame(
        Metric = character(),
        Mean = numeric(),
        Median = numeric(),
        Min = numeric(),
        Max = numeric()
      )
    )
  }
  
  tibble(
    Metric = c(
      "Tenure (months)",
      "Monthly Charges",
      "Total Charges",
      "Service Count"
    ),
    
    Mean = c(
      safe_mean(
        data$tenure
      ),
      safe_mean(
        data$MonthlyCharges
      ),
      safe_mean(
        data$TotalCharges
      ),
      safe_mean(
        data$ServiceCount
      )
    ),
    
    Median = c(
      median(
        data$tenure,
        na.rm = TRUE
      ),
      median(
        data$MonthlyCharges,
        na.rm = TRUE
      ),
      median(
        data$TotalCharges,
        na.rm = TRUE
      ),
      median(
        data$ServiceCount,
        na.rm = TRUE
      )
    ),
    
    Min = c(
      min(
        data$tenure,
        na.rm = TRUE
      ),
      min(
        data$MonthlyCharges,
        na.rm = TRUE
      ),
      min(
        data$TotalCharges,
        na.rm = TRUE
      ),
      min(
        data$ServiceCount,
        na.rm = TRUE
      )
    ),
    
    Max = c(
      max(
        data$tenure,
        na.rm = TRUE
      ),
      max(
        data$MonthlyCharges,
        na.rm = TRUE
      ),
      max(
        data$TotalCharges,
        na.rm = TRUE
      ),
      max(
        data$ServiceCount,
        na.rm = TRUE
      )
    )
  )
}

ui <- fluidPage(
  
  tags$head(
    includeCSS(
      "www/custom.css"
    )
  ),
  
  titlePanel(
    div(
      class = "app-title",
      "Interactive HR Attrition & Customer Churn Analytics Dashboard"
    )
  ),
  
  div(
    class = "app-subtitle",
    "Two-use-case RShiny dashboard | Use Case 1: HR Attrition | Use Case 6: Customer Churn"
  ),
  
  tabsetPanel(
    
    tabPanel(
      "Use Case 1 — HR Attrition",
      
      tabsetPanel(
        
        tabPanel(
          "Executive Overview",
          
          sidebarLayout(
            
            sidebarPanel(
              hr_filter_ui("hr1"),
              width = 3
            ),
            
            mainPanel(
              
              div(
                class = "kpi-row",
                uiOutput("hr1_kpi1"),
                uiOutput("hr1_kpi2"),
                uiOutput("hr1_kpi3"),
                uiOutput("hr1_kpi4"),
                uiOutput("hr1_kpi5")
              ),
              
              h4(
                "Observed Attrition by Department"
              ),
              
              plotlyOutput(
                "hr1_attrition",
                height = "350px"
              ),
              
              fluidRow(
                
                column(
                  6,
                  
                  h4(
                    "Salary Distribution by Department"
                  ),
                  
                  plotlyOutput(
                    "hr1_salary",
                    height = "350px"
                  )
                ),
                
                column(
                  6,
                  
                  h4(
                    "Tenure vs Monthly Income"
                  ),
                  
                  plotlyOutput(
                    "hr1_scatter",
                    height = "350px"
                  )
                )
              ),
              
              uiOutput(
                "hr1_findings"
              )
            )
          )
        ),
        
        tabPanel(
          "Detailed Analysis",
          
          sidebarLayout(
            
            sidebarPanel(
              hr_filter_ui("hr2"),
              width = 3
            ),
            
            mainPanel(
              
              h3(
                "Student-Defined Business Question"
              ),
              
              uiOutput(
                "hr2_question"
              ),
              
              div(
                class = "analysis-note",
                "Use the filters to examine attrition patterns. Results update automatically."
              ),
              
              h4(
                "Additional Analytical Breakdown — Overtime"
              ),
              
              plotOutput(
                "hr2_overtime",
                height = "400px"
              ),
              
              h4(
                "Filtered Segment Summary"
              ),
              
              tableOutput(
                "hr2_summary"
              )
            )
          )
        ),
        
        tabPanel(
          "Data Explorer",
          
          sidebarLayout(
            
            sidebarPanel(
              hr_filter_ui("hr3"),
              width = 3
            ),
            
            mainPanel(
              
              h4(
                "Filtered HR Records"
              ),
              
              DTOutput(
                "hr3_table"
              ),
              
              br(),
              
              downloadButton(
                "hr3_download",
                "Download Filtered HR Data"
              ),
              
              h4(
                "Summary Statistics"
              ),
              
              tableOutput(
                "hr3_stats"
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Use Case 6 — Customer Churn",
      
      tabsetPanel(
        
        tabPanel(
          "Executive Overview",
          
          sidebarLayout(
            
            sidebarPanel(
              telco_filter_ui("tc1"),
              width = 3
            ),
            
            mainPanel(
              
              div(
                class = "kpi-row",
                uiOutput("tc1_kpi1"),
                uiOutput("tc1_kpi2"),
                uiOutput("tc1_kpi3"),
                uiOutput("tc1_kpi4"),
                uiOutput("tc1_kpi5")
              ),
              
              h4(
                "Observed Churn by Contract"
              ),
              
              plotOutput(
                "tc1_contract",
                height = "350px"
              ),
              
              fluidRow(
                
                column(
                  6,
                  
                  h4(
                    "Tenure vs Monthly Charges"
                  ),
                  
                  plotlyOutput(
                    "tc1_scatter",
                    height = "350px"
                  )
                ),
                
                column(
                  6,
                  
                  h4(
                    "Monthly Charges Distribution"
                  ),
                  
                  plotlyOutput(
                    "tc1_box",
                    height = "350px"
                  )
                )
              ),
              
              uiOutput(
                "tc1_findings"
              )
            )
          )
        ),
        
        tabPanel(
          "Detailed Analysis",
          
          sidebarLayout(
            
            sidebarPanel(
              telco_filter_ui("tc2"),
              width = 3
            ),
            
            mainPanel(
              
              h3(
                "Student-Defined Business Question"
              ),
              
              uiOutput(
                "tc2_question"
              ),
              
              div(
                class = "analysis-note",
                "Use the filters to examine churn patterns. Results update automatically."
              ),
              
              h4(
                "Additional Analytical Breakdown — Payment Method"
              ),
              
              plotOutput(
                "tc2_payment",
                height = "400px"
              ),
              
              h4(
                "Filtered Segment Summary"
              ),
              
              tableOutput(
                "tc2_summary"
              )
            )
          )
        ),
        
        tabPanel(
          "Data Explorer",
          
          sidebarLayout(
            
            sidebarPanel(
              telco_filter_ui("tc3"),
              width = 3
            ),
            
            mainPanel(
              
              h4(
                "Filtered Customer Records"
              ),
              
              DTOutput(
                "tc3_table"
              ),
              
              br(),
              
              downloadButton(
                "tc3_download",
                "Download Filtered Customer Data"
              ),
              
              h4(
                "Summary Statistics"
              ),
              
              tableOutput(
                "tc3_stats"
              )
            )
          )
        )
      )
    )
  )
)

server <- function(
    input,
    output,
    session
) {
  
  update_hr_roles <- function(
    input_id,
    department
  ) {
    
    if (
      is.null(department) ||
      department == "All"
    ) {
      
      roles <- hr_roles
      
    } else {
      
      roles <- sort(
        unique(
          hr_raw$JobRole[
            hr_raw$Department == department
          ]
        )
      )
    }
    
    updateSelectInput(
      session,
      input_id,
      choices = c(
        "All",
        roles
      ),
      selected = "All"
    )
  }
  
  observeEvent(
    input$hr1_department,
    {
      update_hr_roles(
        "hr1_role",
        input$hr1_department
      )
    },
    ignoreInit = TRUE
  )
  
  observeEvent(
    input$hr2_department,
    {
      update_hr_roles(
        "hr2_role",
        input$hr2_department
      )
    },
    ignoreInit = TRUE
  )
  
  observeEvent(
    input$hr3_department,
    {
      update_hr_roles(
        "hr3_role",
        input$hr3_department
      )
    },
    ignoreInit = TRUE
  )
  
  observeEvent(
    input$hr1_reset,
    {
      
      updateSelectInput(
        session,
        "hr1_department",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "hr1_role",
        choices = c(
          "All",
          hr_roles
        ),
        selected = "All"
      )
      
      updateSliderInput(
        session,
        "hr1_age",
        value = range(
          hr_raw$Age
        )
      )
      
      updateSliderInput(
        session,
        "hr1_years",
        value = range(
          hr_raw$YearsAtCompany
        )
      )
      
      updateSelectInput(
        session,
        "hr1_overtime",
        selected = "All"
      )
    }
  )
  
  observeEvent(
    input$hr2_reset,
    {
      
      updateSelectInput(
        session,
        "hr2_department",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "hr2_role",
        choices = c(
          "All",
          hr_roles
        ),
        selected = "All"
      )
      
      updateSliderInput(
        session,
        "hr2_age",
        value = range(
          hr_raw$Age
        )
      )
      
      updateSliderInput(
        session,
        "hr2_years",
        value = range(
          hr_raw$YearsAtCompany
        )
      )
      
      updateSelectInput(
        session,
        "hr2_overtime",
        selected = "All"
      )
    }
  )
  
  observeEvent(
    input$hr3_reset,
    {
      
      updateSelectInput(
        session,
        "hr3_department",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "hr3_role",
        choices = c(
          "All",
          hr_roles
        ),
        selected = "All"
      )
      
      updateSliderInput(
        session,
        "hr3_age",
        value = range(
          hr_raw$Age
        )
      )
      
      updateSliderInput(
        session,
        "hr3_years",
        value = range(
          hr_raw$YearsAtCompany
        )
      )
      
      updateSelectInput(
        session,
        "hr3_overtime",
        selected = "All"
      )
    }
  )
  
  observeEvent(
    input$tc1_reset,
    {
      
      updateSelectInput(
        session,
        "tc1_contract",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "tc1_internet",
        selected = "All"
      )
      
      updateSliderInput(
        session,
        "tc1_tenure",
        value = range(
          telco_raw$tenure
        )
      )
      
      updateSliderInput(
        session,
        "tc1_charges",
        value = range(
          telco_raw$MonthlyCharges
        )
      )
      
      updateSelectInput(
        session,
        "tc1_payment",
        selected = "All"
      )
    }
  )
  
  observeEvent(
    input$tc2_reset,
    {
      
      updateSelectInput(
        session,
        "tc2_contract",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "tc2_internet",
        selected = "All"
      )
      
      updateSliderInput(
        session,
        "tc2_tenure",
        value = range(
          telco_raw$tenure
        )
      )
      
      updateSliderInput(
        session,
        "tc2_charges",
        value = range(
          telco_raw$MonthlyCharges
        )
      )
      
      updateSelectInput(
        session,
        "tc2_payment",
        selected = "All"
      )
    }
  )
  
  observeEvent(
    input$tc3_reset,
    {
      
      updateSelectInput(
        session,
        "tc3_contract",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "tc3_internet",
        selected = "All"
      )
      
      updateSliderInput(
        session,
        "tc3_tenure",
        value = range(
          telco_raw$tenure
        )
      )
      
      updateSliderInput(
        session,
        "tc3_charges",
        value = range(
          telco_raw$MonthlyCharges
        )
      )
      
      updateSelectInput(
        session,
        "tc3_payment",
        selected = "All"
      )
    }
  )
  
  hr1 <- reactive({
    hr_filtered(
      hr_raw,
      input$hr1_department,
      input$hr1_role,
      input$hr1_age,
      input$hr1_years,
      input$hr1_overtime
    )
  })
  
  hr2 <- reactive({
    hr_filtered(
      hr_raw,
      input$hr2_department,
      input$hr2_role,
      input$hr2_age,
      input$hr2_years,
      input$hr2_overtime
    )
  })
  
  hr3 <- reactive({
    hr_filtered(
      hr_raw,
      input$hr3_department,
      input$hr3_role,
      input$hr3_age,
      input$hr3_years,
      input$hr3_overtime
    )
  })
  
  tc1 <- reactive({
    telco_filtered(
      telco_raw,
      input$tc1_contract,
      input$tc1_internet,
      input$tc1_tenure,
      input$tc1_charges,
      input$tc1_payment
    )
  })
  
  tc2 <- reactive({
    telco_filtered(
      telco_raw,
      input$tc2_contract,
      input$tc2_internet,
      input$tc2_tenure,
      input$tc2_charges,
      input$tc2_payment
    )
  })
  
  tc3 <- reactive({
    telco_filtered(
      telco_raw,
      input$tc3_contract,
      input$tc3_internet,
      input$tc3_tenure,
      input$tc3_charges,
      input$tc3_payment
    )
  })
  
  output$hr1_kpi1 <- renderUI({
    d <- hr1()
    
    kpi_card(
      "Total Employees",
      fmt_number(
        nrow(d)
      )
    )
  })
  
  output$hr1_kpi2 <- renderUI({
    d <- hr1()
    
    kpi_card(
      "Total Attrition",
      fmt_number(
        sum(
          d$Attrition == "Yes"
        )
      )
    )
  })
  
  output$hr1_kpi3 <- renderUI({
    d <- hr1()
    
    rate <- if (
      nrow(d) == 0
    ) {
      NA_real_
    } else {
      mean(
        d$Attrition == "Yes"
      ) * 100
    }
    
    kpi_card(
      "Attrition Rate",
      fmt_pct(rate)
    )
  })
  
  output$hr1_kpi4 <- renderUI({
    d <- hr1()
    
    kpi_card(
      "Avg Monthly Income",
      fmt_currency(
        safe_mean(
          d$MonthlyIncome
        )
      )
    )
  })
  
  output$hr1_kpi5 <- renderUI({
    d <- hr1()
    
    kpi_card(
      "Avg Years at Company",
      fmt_number(
        safe_mean(
          d$YearsAtCompany
        ),
        1
      )
    )
  })
  
  output$hr1_attrition <- renderPlotly({
    
    d <- hr1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        ggplotly(
          empty_plot()
        )
      )
    }
    
    plot_data <- d %>%
      group_by(
        Department
      ) %>%
      summarise(
        AttritionRate =
          mean(
            Attrition == "Yes"
          ) * 100,
        .groups = "drop"
      )
    
    p <- ggplot(
      plot_data,
      aes(
        x = reorder(
          Department,
          AttritionRate
        ),
        y = AttritionRate
      )
    ) +
      
      geom_col() +
      
      geom_text(
        aes(
          label = paste0(
            round(
              AttritionRate,
              1
            ),
            "%"
          )
        ),
        hjust = -0.15
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0,
            0.12
          )
        )
      ) +
      
      labs(
        x = NULL,
        y = "Observed attrition rate (%)"
      ) +
      
      theme_minimal(
        base_size = 13
      )
    
    ggplotly(p)
  })
  
  output$hr1_salary <- renderPlotly({
    
    d <- hr1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        ggplotly(
          empty_plot()
        )
      )
    }
    
    p <- ggplot(
      d,
      aes(
        x = Department,
        y = MonthlyIncome,
        fill = Attrition
      )
    ) +
      
      geom_boxplot(
        outlier.alpha = 0.25
      ) +
      
      labs(
        x = NULL,
        y = "Monthly income",
        fill = "Attrition"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        axis.text.x =
          element_text(
            angle = 20,
            hjust = 1
          )
      )
    
    ggplotly(p)
  })
  
  output$hr1_scatter <- renderPlotly({
    
    d <- hr1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        ggplotly(
          empty_plot()
        )
      )
    }
    
    p <- ggplot(
      d,
      aes(
        x = YearsAtCompany,
        y = MonthlyIncome,
        color = Attrition
      )
    ) +
      
      geom_point(
        alpha = 0.65,
        size = 2
      ) +
      
      geom_smooth(
        aes(
          group = 1
        ),
        method = "lm",
        se = FALSE,
        color = "grey35"
      ) +
      
      labs(
        x = "Years at company",
        y = "Monthly income",
        color = "Attrition"
      ) +
      
      theme_minimal(
        base_size = 13
      )
    
    ggplotly(p)
  })
  
  output$hr1_findings <- renderUI({
    
    d <- hr1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        div(
          class = "finding-box",
          "No observations are available for the current filters."
        )
      )
    }
    
    dept <- d %>%
      group_by(
        Department
      ) %>%
      summarise(
        rate =
          mean(
            Attrition == "Yes"
          ) * 100,
        .groups = "drop"
      ) %>%
      arrange(
        desc(rate)
      )
    
    overtime <- d %>%
      group_by(
        OverTime
      ) %>%
      summarise(
        rate =
          mean(
            Attrition == "Yes"
          ) * 100,
        .groups = "drop"
      ) %>%
      arrange(
        desc(rate)
      )
    
    div(
      class = "finding-box",
      
      strong(
        "Evidence-based finding: "
      ),
      
      paste0(
        "Within the current filtered sample, ",
        dept$Department[1],
        " has the highest observed department attrition rate (",
        round(
          dept$rate[1],
          1
        ),
        "%). The overtime group with the higher observed attrition rate is ",
        overtime$OverTime[1],
        " (",
        round(
          overtime$rate[1],
          1
        ),
        "%). These are observed associations in the dataset, not causal conclusions."
      )
    )
  })
  
  output$hr1_filter_status <- renderText({
    paste(
      "Active records:",
      nrow(
        hr1()
      )
    )
  })
  
  output$hr2_filter_status <- renderText({
    paste(
      "Active records:",
      nrow(
        hr2()
      )
    )
  })
  
  output$hr3_filter_status <- renderText({
    paste(
      "Active records:",
      nrow(
        hr3()
      )
    )
  })
  
  output$hr2_question <- renderUI({
    
    div(
      class = "question-box",
      business_question_hr(
        hr2()
      )
    )
  })
  
  output$hr2_overtime <- renderPlot({
    
    d <- hr2()
    
    if (
      nrow(d) == 0
    ) {
      print(
        empty_plot()
      )
      return()
    }
    
    plot_data <- d %>%
      group_by(
        OverTime
      ) %>%
      summarise(
        AttritionRate =
          mean(
            Attrition == "Yes"
          ) * 100,
        .groups = "drop"
      )
    
    ymax <- max(
      plot_data$AttritionRate,
      na.rm = TRUE
    )
    
    ggplot(
      plot_data,
      aes(
        x = OverTime,
        y = AttritionRate
      )
    ) +
      
      geom_col() +
      
      geom_text(
        aes(
          label = paste0(
            round(
              AttritionRate,
              1
            ),
            "%"
          )
        ),
        vjust = -0.4,
        size = 5
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          ymax * 1.2
        ),
        expand = expansion(
          mult = c(
            0,
            0.02
          )
        )
      ) +
      
      labs(
        x = "Overtime",
        y = "Observed attrition rate (%)"
      ) +
      
      theme_minimal(
        base_size = 14
      )
  })
  
  output$hr2_summary <- renderTable({
    
    d <- hr2()
    
    if (
      nrow(d) == 0
    ) {
      return(
        data.frame(
          Message =
            "No rows match the current filters."
        )
      )
    }
    
    data.frame(
      Employees =
        nrow(d),
      
      Attrition =
        sum(
          d$Attrition == "Yes"
        ),
      
      AttritionRate =
        fmt_pct(
          mean(
            d$Attrition == "Yes"
          ) * 100
        ),
      
      AvgIncome =
        fmt_currency(
          safe_mean(
            d$MonthlyIncome
          )
        ),
      
      AvgYearsAtCompany =
        round(
          safe_mean(
            d$YearsAtCompany
          ),
          1
        )
    )
  })
  
  output$hr3_table <- renderDT({
    
    datatable(
      hr3() %>%
        select(
          EmployeeNumber,
          Age,
          Attrition,
          Department,
          JobRole,
          MonthlyIncome,
          OverTime,
          YearsAtCompany,
          AgeBand,
          TenureBand
        ),
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE,
        lengthMenu = c(
          10,
          25,
          50,
          100
        )
      ),
      rownames = FALSE
    )
  })
  
  output$hr3_stats <- renderTable({
    
    summary_table_hr(
      hr3()
    )
    
  }, digits = 2)
  
  output$hr3_download <- downloadHandler(
    
    filename = function() {
      paste0(
        "hr_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write_csv(
        hr3(),
        file
      )
    }
  )
  
  output$tc1_kpi1 <- renderUI({
    
    d <- tc1()
    
    kpi_card(
      "Total Customers",
      fmt_number(
        nrow(d)
      )
    )
  })
  
  output$tc1_kpi2 <- renderUI({
    
    d <- tc1()
    
    kpi_card(
      "Churned Customers",
      fmt_number(
        sum(
          d$Churn == "Yes"
        )
      )
    )
  })
  
  output$tc1_kpi3 <- renderUI({
    
    d <- tc1()
    
    rate <- if (
      nrow(d) == 0
    ) {
      NA_real_
    } else {
      mean(
        d$Churn == "Yes"
      ) * 100
    }
    
    kpi_card(
      "Churn Rate",
      fmt_pct(rate)
    )
  })
  
  output$tc1_kpi4 <- renderUI({
    
    d <- tc1()
    
    kpi_card(
      "Avg Monthly Charges",
      fmt_currency(
        safe_mean(
          d$MonthlyCharges
        ),
        2
      )
    )
  })
  
  output$tc1_kpi5 <- renderUI({
    
    d <- tc1()
    
    kpi_card(
      "Avg Tenure",
      paste0(
        fmt_number(
          safe_mean(
            d$tenure
          ),
          1
        ),
        " mo"
      )
    )
  })
  
  output$tc1_contract <- renderPlot({
    
    d <- tc1()
    
    if (
      nrow(d) == 0
    ) {
      print(
        empty_plot()
      )
      return()
    }
    
    plot_data <- d %>%
      group_by(
        Contract
      ) %>%
      summarise(
        ChurnRate =
          mean(
            Churn == "Yes"
          ) * 100,
        .groups = "drop"
      )
    
    ymax <- max(
      plot_data$ChurnRate,
      na.rm = TRUE
    )
    
    ggplot(
      plot_data,
      aes(
        x = Contract,
        y = ChurnRate
      )
    ) +
      
      geom_col() +
      
      geom_text(
        aes(
          label = paste0(
            round(
              ChurnRate,
              1
            ),
            "%"
          )
        ),
        vjust = -0.35,
        size = 4
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          ymax * 1.15
        ),
        expand = expansion(
          mult = c(
            0,
            0.02
          )
        )
      ) +
      
      labs(
        x = NULL,
        y = "Observed churn rate (%)"
      ) +
      
      theme_minimal(
        base_size = 13
      )
  })
  
  output$tc1_scatter <- renderPlotly({
    
    d <- tc1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        ggplotly(
          empty_plot()
        )
      )
    }
    
    p <- ggplot(
      d,
      aes(
        x = tenure,
        y = MonthlyCharges,
        color = Churn
      )
    ) +
      
      geom_point(
        alpha = 0.55,
        size = 2
      ) +
      
      geom_smooth(
        aes(
          group = 1
        ),
        method = "lm",
        se = FALSE,
        color = "grey35"
      ) +
      
      labs(
        x = "Tenure (months)",
        y = "Monthly charges",
        color = "Churn"
      ) +
      
      theme_minimal(
        base_size = 13
      )
    
    ggplotly(p)
  })
  
  output$tc1_box <- renderPlotly({
    
    d <- tc1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        ggplotly(
          empty_plot()
        )
      )
    }
    
    p <- ggplot(
      d,
      aes(
        x = Churn,
        y = MonthlyCharges,
        fill = Churn
      )
    ) +
      
      geom_boxplot(
        outlier.alpha = 0.25
      ) +
      
      labs(
        x = NULL,
        y = "Monthly charges",
        fill = "Churn"
      ) +
      
      theme_minimal(
        base_size = 13
      )
    
    ggplotly(p)
  })
  
  output$tc1_findings <- renderUI({
    
    d <- tc1()
    
    if (
      nrow(d) == 0
    ) {
      return(
        div(
          class = "finding-box",
          "No observations are available for the current filters."
        )
      )
    }
    
    contract <- d %>%
      group_by(
        Contract
      ) %>%
      summarise(
        rate =
          mean(
            Churn == "Yes"
          ) * 100,
        .groups = "drop"
      ) %>%
      arrange(
        desc(rate)
      )
    
    tenure <- d %>%
      group_by(
        TenureBand
      ) %>%
      summarise(
        rate =
          mean(
            Churn == "Yes"
          ) * 100,
        .groups = "drop"
      ) %>%
      arrange(
        desc(rate)
      )
    
    div(
      class = "finding-box",
      
      strong(
        "Evidence-based finding: "
      ),
      
      paste0(
        "Within the current filtered sample, ",
        contract$Contract[1],
        " has the highest observed contract-level churn rate (",
        round(
          contract$rate[1],
          1
        ),
        "%). The highest observed tenure-band churn is in ",
        as.character(
          tenure$TenureBand[1]
        ),
        " (",
        round(
          tenure$rate[1],
          1
        ),
        "%). These are descriptive associations, not causal conclusions."
      )
    )
  })
  
  output$tc1_filter_status <- renderText({
    
    paste(
      "Active records:",
      nrow(
        tc1()
      )
    )
  })
  
  output$tc2_filter_status <- renderText({
    
    paste(
      "Active records:",
      nrow(
        tc2()
      )
    )
  })
  
  output$tc3_filter_status <- renderText({
    
    paste(
      "Active records:",
      nrow(
        tc3()
      )
    )
  })
  
  output$tc2_question <- renderUI({
    
    div(
      class = "question-box",
      business_question_telco(
        tc2()
      )
    )
  })
  
  output$tc2_payment <- renderPlot({
    
    d <- tc2()
    
    if (
      nrow(d) == 0
    ) {
      print(
        empty_plot()
      )
      return()
    }
    
    plot_data <- d %>%
      group_by(
        PaymentMethod
      ) %>%
      summarise(
        ChurnRate =
          mean(
            Churn == "Yes"
          ) * 100,
        Customers =
          n(),
        .groups = "drop"
      )
    
    ymax <- max(
      plot_data$ChurnRate,
      na.rm = TRUE
    )
    
    ggplot(
      plot_data,
      aes(
        x = reorder(
          PaymentMethod,
          ChurnRate
        ),
        y = ChurnRate
      )
    ) +
      
      geom_col() +
      
      geom_text(
        aes(
          label = paste0(
            round(
              ChurnRate,
              1
            ),
            "%"
          )
        ),
        vjust = -0.35,
        size = 4
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        limits = c(
          0,
          ymax * 1.15
        ),
        expand = expansion(
          mult = c(
            0,
            0.02
          )
        )
      ) +
      
      labs(
        x = NULL,
        y = "Observed churn rate (%)"
      ) +
      
      theme_minimal(
        base_size = 13
      )
  })
  
  output$tc2_summary <- renderTable({
    
    d <- tc2()
    
    if (
      nrow(d) == 0
    ) {
      return(
        data.frame(
          Message =
            "No rows match the current filters."
        )
      )
    }
    
    data.frame(
      
      Customers =
        nrow(d),
      
      Churned =
        sum(
          d$Churn == "Yes"
        ),
      
      ChurnRate =
        fmt_pct(
          mean(
            d$Churn == "Yes"
          ) * 100
        ),
      
      AvgMonthlyCharges =
        fmt_currency(
          safe_mean(
            d$MonthlyCharges
          ),
          2
        ),
      
      AvgTenure =
        paste0(
          round(
            safe_mean(
              d$tenure
            ),
            1
          ),
          " months"
        ),
      
      AvgServiceCount =
        round(
          safe_mean(
            d$ServiceCount
          ),
          1
        )
    )
  })
  
  output$tc3_table <- renderDT({
    
    datatable(
      tc3() %>%
        select(
          customerID,
          gender,
          SeniorCitizenLabel,
          Partner,
          Dependents,
          tenure,
          InternetService,
          Contract,
          PaymentMethod,
          MonthlyCharges,
          TotalCharges,
          ServiceCount,
          TenureBand,
          Churn
        ),
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE,
        lengthMenu = c(
          10,
          25,
          50,
          100
        )
      ),
      rownames = FALSE
    )
  })
  
  output$tc3_stats <- renderTable({
    
    summary_table_telco(
      tc3()
    )
    
  }, digits = 2)
  
  output$tc3_download <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "telco_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write_csv(
        tc3(),
        file
      )
    }
  )
}

shinyApp(
  ui = ui,
  server = server
)
