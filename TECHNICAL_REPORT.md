# Technical Report — RShiny Interactive Analytics Dashboard

## 1. Objective

This dashboard demonstrates an end-to-end analytics workflow for two approved industry use cases from the assignment:

- Use Case 1 — HR Attrition Analytics
- Use Case 6 — Customer Churn Analytics

The assignment requires data preparation, reactive filtering, KPI computation, analytical visualization, data exploration, evidence-based findings, three tabs per use case, at least four charts per use case, interactive data tables, export functionality, advanced RShiny features, and one student-defined business question per use case.

## 2. Data preparation and audit

### HR Employee Attrition

The supplied HR dataset contains 1,470 rows and 35 columns. No missing cells and no duplicate rows were found.

Validation checks found no records with:
- Age outside 18–60
- YearsAtCompany outside 0–40
- MonthlyIncome less than or equal to 0
- YearsAtCompany greater than TotalWorkingYears
- Invalid Attrition labels
- Invalid OverTime labels

Derived variables:
- `AttritionFlag`
- `AgeBand`
- `TenureBand`
- `IncomeBand`

### Telco Customer Churn

The supplied Telco dataset contains 7,043 rows and 21 columns. No duplicate rows were found.

`TotalCharges` was stored as text and contained 11 blank values. All 11 blanks correspond to customers with `tenure = 0`. The cleaning rule converts those 11 blanks to numeric `0`, which is consistent with a customer who has no recorded tenure.

Derived variables:
- `ChurnFlag`
- `SeniorCitizenLabel`
- `TenureBand`
- `MonthlyChargeBand`
- `ServiceCount`

## 3. Dashboard architecture

### Data layer

Raw CSV files are loaded with `readr::read_csv()`. The preparation script checks structure, missing values, duplicates, types and suspicious values.

### Transformation layer

`dplyr` and `tidyr` are used for cleaning, filtering, grouping and summary calculations. Derived variables are generated before dashboard rendering.

### Reactive layer

`eventReactive()` is used so the Apply Filters button controls filtering. KPI cards, plots, findings and data tables all consume the corresponding reactive filtered dataset.

### Visualization layer

`ggplot2` creates the analytical plots and `plotly::ggplotly()` provides hover interaction.

### UI layer

The dashboard uses `fluidPage()`, `sidebarLayout()`, `tabsetPanel()` and custom KPI cards. Each selected use case has exactly three tabs:

1. Executive Overview
2. Detailed Analysis
3. Data Explorer

### Data Explorer

`DT::renderDT()` provides search, sorting, pagination and interactive display. A `downloadHandler()` exports the currently filtered records.

## 4. Business questions

### HR Attrition

Does overtime and tenure provide a useful breakdown of observed attrition across departments?

The dashboard compares observed attrition rates across overtime status and tenure bands while allowing the user to filter department, job role, age and tenure.

### Customer Churn

How do contract type and tenure jointly relate to observed churn, and which service/payment segment warrants further investigation?

The dashboard compares observed churn across contract, tenure, internet/service type and payment method.

## 5. Evidence-based observations from the supplied data

These are descriptive observations from the supplied datasets and should not be interpreted as causal conclusions.

### HR

Overall observed attrition in the supplied HR dataset is 16.1%.

Department-level observed attrition rates are approximately:
- Human Resources: 19.0%
- Research & Development: 13.8%
- Sales: 20.6%

Observed attrition is also higher in the overtime group:
- Overtime = Yes: approximately 30.5%
- Overtime = No: approximately 10.4%

### Customer churn

Overall observed churn in the supplied Telco dataset is 26.5%.

Contract-level observed churn rates are approximately:
- Month-to-month: 42.7%
- One year: 11.3%
- Two year: 2.8%

Observed churn by tenure band is approximately:
- 0–6 months: 52.9%
- 7–12 months: 35.9%
- 13–24 months: 28.7%
- 25–48 months: 20.4%
- 49–72 months: 9.5%

The dashboard is intended for exploratory analytics, so these patterns identify segments for further investigation rather than proving why attrition or churn occurs.

## 6. How to run

1. Open the project folder in RStudio.
2. Install the required packages once.
3. Open `app.R`.
4. Click **Run App**.
5. Test every filter and the Reset button.
6. Check that KPIs and charts change after Apply Filters.
7. Test Data Explorer search, sorting, pagination and download.
8. Capture screenshots for the final report.
