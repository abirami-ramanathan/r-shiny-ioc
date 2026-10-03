# RShiny HR Attrition & Customer Churn Dashboard

This project implements the RShiny assignment for two approved use cases:

1. Use Case 1 — HR Attrition Analytics
2. Use Case 6 — Customer Churn Analytics

## Project structure

```text
RShiny_HR_Churn_Dashboard/
├── app.R
├── data/
│   ├── HR_Employee_Attrition_raw.csv
│   ├── Telco_Customer_Churn_raw.csv
│   └── clean/
├── R/
│   ├── data_prep.R
│   ├── helpers.R
│   └── plots.R
├── www/
│   └── custom.css
├── screenshots/
└── TECHNICAL_REPORT.md
```

## Required R packages

Run once in the RStudio Console:

```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "readr", "ggplot2",
  "plotly", "DT", "scales", "tibble"
))
```

## Run the project

Open this folder as an RStudio Project, open `app.R`, and click **Run App**.

Or run:

```r
setwd("path/to/RShiny_HR_Churn_Dashboard")
shiny::runApp()
```

The app first executes `R/data_prep.R`, which creates the clean analysis-ready CSVs under `data/clean/`, then launches the dashboard.

## What the dashboard covers

Each use case contains exactly three dashboard tabs:

- Executive Overview
- Detailed Analysis
- Data Explorer

### HR filters

- Department
- Job Role
- Age Range
- Years at Company
- Overtime

### HR KPIs

- Total Employees
- Total Attrition
- Attrition Rate
- Average Monthly Income
- Average Years at Company

### HR charts

- Attrition by Department — bar
- Salary Distribution by Department — box plot
- Years at Company vs Monthly Income — scatter
- Observed Attrition by Overtime — additional analysis

### Customer churn filters

- Contract Type
- Internet / Service Type
- Tenure Range
- Monthly Charges Range
- Payment Method

### Customer churn KPIs

- Total Customers
- Churned Customers
- Churn Rate
- Average Monthly Charges
- Average Tenure

### Customer churn charts

- Churn by Contract — bar
- Tenure vs Charges — scatter
- Monthly Charges Distribution — box plot
- Churn by Payment Method — additional analysis

## Advanced Shiny features included

- `eventReactive()` for Apply Filters actions
- Dynamic `updateSelectInput()` for HR Job Role filtering based on Department
- `conditionalPanel()` for contextual analysis guidance
- Plotly hover interaction
- `downloadHandler()` for filtered data export

## Data preparation

The assignment requires inspection, missing-value treatment, duplicate checks, data-type validation, category checks, at least one derived variable, suspicious-value checks, and a clean analysis-ready dataset. The `data_prep.R` script performs these steps and writes audit files to `data/clean/`.

For the Telco dataset, `TotalCharges` contains 11 blank values. All 11 belong to customers with `tenure = 0`, so the cleaning rule converts these blanks to numeric zero. No duplicate rows were found.

## Screenshots

After running the dashboard, save screenshots in `screenshots/`:

- `tab1_overview.png`
- `tab2_analysis.png`
- `tab3_data_explorer.png`

Take screenshots for both use cases, for example:

- `hr_overview.png`
- `hr_analysis.png`
- `hr_explorer.png`
- `churn_overview.png`
- `churn_analysis.png`
- `churn_explorer.png`
