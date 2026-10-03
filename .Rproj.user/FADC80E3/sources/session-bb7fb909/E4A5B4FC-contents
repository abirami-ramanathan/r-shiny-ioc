library(readr)
library(dplyr)
library(tidyr)

hr_raw <- read_csv("data/HR_Employee_Attrition_raw.csv", show_col_types = FALSE, trim_ws = TRUE)
telco_raw <- read_csv("data/Telco_Customer_Churn_raw.csv", show_col_types = FALSE, trim_ws = TRUE)

audit_hr <- tibble(
  metric = c("Rows", "Columns", "Missing cells", "Duplicate rows"),
  value = c(nrow(hr_raw), ncol(hr_raw), sum(is.na(hr_raw)), sum(duplicated(hr_raw)))
)

audit_telco_before <- tibble(
  metric = c("Rows", "Columns", "Blank TotalCharges values", "Duplicate rows"),
  value = c(
    nrow(telco_raw),
    ncol(telco_raw),
    sum(trimws(telco_raw$TotalCharges) == ""),
    sum(duplicated(telco_raw))
  )
)

# HR validation and derived variables.
hr_clean <- hr_raw %>%
  mutate(
    AttritionFlag = if_else(Attrition == "Yes", 1L, 0L),
    AgeBand = cut(
      Age,
      breaks = c(17, 25, 35, 45, 60),
      labels = c("18-25", "26-35", "36-45", "46-60"),
      include.lowest = TRUE
    ),
    TenureBand = cut(
      YearsAtCompany,
      breaks = c(-1, 2, 5, 10, Inf),
      labels = c("0-2 years", "3-5 years", "6-10 years", "11+ years"),
      include.lowest = TRUE
    ),
    IncomeBand = cut(
      MonthlyIncome,
      breaks = c(-Inf, 3000, 6000, 10000, Inf),
      labels = c("< 3k", "3k-6k", "6k-10k", "10k+")
    )
  )

hr_suspicious <- tibble(
  check = c(
    "Age outside 18-60",
    "YearsAtCompany outside 0-40",
    "MonthlyIncome <= 0",
    "YearsAtCompany > TotalWorkingYears",
    "Invalid Attrition label",
    "Invalid OverTime label"
  ),
  count = c(
    sum(hr_clean$Age < 18 | hr_clean$Age > 60),
    sum(hr_clean$YearsAtCompany < 0 | hr_clean$YearsAtCompany > 40),
    sum(hr_clean$MonthlyIncome <= 0),
    sum(hr_clean$YearsAtCompany > hr_clean$TotalWorkingYears),
    sum(!hr_clean$Attrition %in% c("Yes", "No")),
    sum(!hr_clean$OverTime %in% c("Yes", "No"))
  )
)

# Telco validation, numeric conversion, missing-value treatment and derived variables.
telco_clean <- telco_raw %>%
  mutate(
    TotalCharges = na_if(trimws(TotalCharges), ""),
    TotalCharges = as.numeric(TotalCharges),
    TotalCharges = if_else(tenure == 0 & is.na(TotalCharges), 0, TotalCharges),
    ChurnFlag = if_else(Churn == "Yes", 1L, 0L),
    SeniorCitizenLabel = if_else(SeniorCitizen == 1, "Senior citizen", "Non-senior"),
    TenureBand = cut(
      tenure,
      breaks = c(-1, 6, 12, 24, 48, 72),
      labels = c("0-6 months", "7-12 months", "13-24 months", "25-48 months", "49-72 months"),
      include.lowest = TRUE
    ),
    MonthlyChargeBand = cut(
      MonthlyCharges,
      breaks = c(-Inf, 40, 70, 100, Inf),
      labels = c("< $40", "$40-$70", "$70-$100", "$100+")
    ),
    ServiceCount = rowSums(across(
      c(PhoneService, MultipleLines, OnlineSecurity, OnlineBackup,
        DeviceProtection, TechSupport, StreamingTV, StreamingMovies),
      ~ .x == "Yes"
    ))
  )

audit_telco_after <- tibble(
  metric = c(
    "Missing cells after cleaning",
    "Remaining TotalCharges missing",
    "Duplicate rows",
    "Negative/invalid tenure",
    "Non-positive MonthlyCharges",
    "Invalid Churn label"
  ),
  value = c(
    sum(is.na(telco_clean)),
    sum(is.na(telco_clean$TotalCharges)),
    sum(duplicated(telco_clean)),
    sum(telco_clean$tenure < 0 | telco_clean$tenure > 72),
    sum(telco_clean$MonthlyCharges <= 0),
    sum(!telco_clean$Churn %in% c("Yes", "No"))
  )
)

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)
write_csv(hr_clean, "data/clean/hr_clean.csv", na = "")
write_csv(telco_clean, "data/clean/telco_clean.csv", na = "")
write_csv(audit_hr, "data/clean/hr_audit.csv")
write_csv(audit_telco_before, "data/clean/telco_audit_before.csv")
write_csv(audit_telco_after, "data/clean/telco_audit_after.csv")
write_csv(hr_suspicious, "data/clean/hr_suspicious_checks.csv")

cat("\nHR audit:\n")
print(audit_hr)
cat("\nTelco audit before cleaning:\n")
print(audit_telco_before)
cat("\nTelco audit after cleaning:\n")
print(audit_telco_after)
cat("\nHR suspicious-value checks:\n")
print(hr_suspicious)
