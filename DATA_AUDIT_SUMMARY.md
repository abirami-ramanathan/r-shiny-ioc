# DATA AUDIT SUMMARY

Source: Assignment brief and the two supplied CSV files.

HR Employee Attrition
- Rows: 1470
- Columns: 35
- Missing cells: 0
- Duplicate rows: 0
- Overall attrition rate: 16.1%

Telco Customer Churn
- Rows: 7043
- Columns: 21
- Duplicate rows: 0
- Blank TotalCharges values before cleaning: 11
- Those blank TotalCharges rows all have tenure = 0.
- Overall churn rate: 26.5%

HR suspicious-value checks
- Age outside 18-60: 0
- YearsAtCompany outside 0-40: 0
- MonthlyIncome <= 0: 0
- YearsAtCompany > TotalWorkingYears: 0
- Invalid Attrition labels: 0
- Invalid OverTime labels: 0

Telco suspicious-value checks
- Tenure outside 0-72: 0
- MonthlyCharges <= 0: 0
- Invalid Churn labels: 0
- Invalid Contract labels: 0
