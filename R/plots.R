hr_attrition_by_department <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  p <- data %>%
    group_by(Department) %>%
    summarise(
      AttritionRate = mean(Attrition == "Yes") * 100,
      .groups = "drop"
    ) %>%
    ggplot(aes(x = reorder(Department, AttritionRate), y = AttritionRate, text = paste0(
      "Department: ", Department, "<br>Observed attrition: ", round(AttritionRate, 1), "%"
    ))) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "Observed attrition rate (%)") +
    theme_minimal(base_size = 13)
  p
}

hr_salary_box <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    ggplot(aes(x = Department, y = MonthlyIncome, fill = Attrition)) +
    geom_boxplot(outlier.alpha = 0.25) +
    labs(x = NULL, y = "Monthly income", fill = "Attrition") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

hr_tenure_income <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    ggplot(aes(
      x = YearsAtCompany, y = MonthlyIncome, color = Attrition,
      text = paste0(
        "Job role: ", JobRole,
        "<br>Years at company: ", YearsAtCompany,
        "<br>Monthly income: $", formatC(MonthlyIncome, format = "f", big.mark = ","),
        "<br>Attrition: ", Attrition
      )
    )) +
    geom_point(alpha = 0.65, size = 2.4) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, linewidth = 0.8, color = "grey35") +
    labs(x = "Years at company", y = "Monthly income", color = "Attrition") +
    theme_minimal(base_size = 13)
}

hr_overtime_attrition <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    group_by(OverTime) %>%
    summarise(
      AttritionRate = mean(Attrition == "Yes") * 100,
      Employees = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = OverTime, y = AttritionRate, text = paste0(
      "Overtime: ", OverTime,
      "<br>Employees: ", Employees,
      "<br>Observed attrition: ", round(AttritionRate, 1), "%"
    ))) +
    geom_col() +
    labs(x = "Overtime", y = "Observed attrition rate (%)") +
    theme_minimal(base_size = 13)
}

telco_churn_by_contract <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    group_by(Contract) %>%
    summarise(
      ChurnRate = mean(Churn == "Yes") * 100,
      Customers = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = Contract, y = ChurnRate, text = paste0(
      "Contract: ", Contract,
      "<br>Customers: ", Customers,
      "<br>Observed churn: ", round(ChurnRate, 1), "%"
    ))) +
    geom_col() +
    labs(x = NULL, y = "Observed churn rate (%)") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
}

telco_tenure_charges <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    ggplot(aes(
      x = tenure, y = MonthlyCharges, color = Churn,
      text = paste0(
        "Tenure: ", tenure, " months",
        "<br>Monthly charges: $", round(MonthlyCharges, 2),
        "<br>Churn: ", Churn
      )
    )) +
    geom_point(alpha = 0.55, size = 2) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, linewidth = 0.8, color = "grey35") +
    labs(x = "Tenure (months)", y = "Monthly charges", color = "Churn") +
    theme_minimal(base_size = 13)
}

telco_charge_box <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    ggplot(aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
    geom_boxplot(outlier.alpha = 0.25) +
    labs(x = NULL, y = "Monthly charges", fill = "Churn") +
    theme_minimal(base_size = 13)
}

telco_payment_churn <- function(data) {
  if (nrow(data) == 0) return(empty_plot())
  data %>%
    group_by(PaymentMethod) %>%
    summarise(
      ChurnRate = mean(Churn == "Yes") * 100,
      Customers = n(),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = reorder(PaymentMethod, ChurnRate), y = ChurnRate, text = paste0(
      "Payment: ", PaymentMethod,
      "<br>Customers: ", Customers,
      "<br>Observed churn: ", round(ChurnRate, 1), "%"
    ))) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "Observed churn rate (%)") +
    theme_minimal(base_size = 13)
}
