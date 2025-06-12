# Clear the environment
rm(list = ls())  # Remove all objects from the environment

# Load necessary libraries
library(broom)
library(caret)
library(dplyr)
library(effects)
library(flextable)
library(ggplot2)
library(gt)
library(gtsummary)
library(jtools)
library(kableExtra)
library(knitr)
library(lubridate)
library(officer)
library(rcompanion)
library(readr)
library(stringr)
library(tidyverse)
library(vcd)

# Load the dataset
sr_data <- read.csv("Data/311_SR_all_years_combined.csv")

# Step 1: Create a department_full_name code-name mapping table
department_lookup <- data.frame(
  department = c("ANML", "BPS_", "BTDT", "BWSC", "CHT_", "DISB", "HS_D", "HS_O", "HS_V", "HS_W", 
                 "HS_Y", "INFO", "ISD", "No Q", "PARK", "PROP", "PWDx", "ONS_", "GEN_", "BPD_", "BHA_", 
                 "DND_", "Temp"),
  department_full_name = c("Animal Control", "Boston Public Schools", "Transportation Department", 
                           "Water and Sewer Commission", "City Hall Truck", "Disability Commission", 
                           "Disabilities/ADA", "Housing Office of Civil Rights", "Veterans Call log", 
                           "Women’s Commission", "Youthline", "Information Channel", "Inspectional Services", 
                           "No queue assigned", "Parks", "Property Management", "Public Works", 
                           "Office of Neighborhood Services", "General Services", "Boston Police Department", 
                           "Boston Housing Authority", "Department of Neighborhood Development", 
                           "Temporary or Seasonal Services"),
  stringsAsFactors = FALSE
)

district_lookup <- data.frame(
  city_council_district = as.character(1:9),
  city_council_district_details = c(
    "Charlestown, East Boston, North End (Sal LaMattina)",
    "Downtown, South Boston, South End (Bill Linehan)",
    "Dorchester (Frank Baker)",
    "Mattapan (Charles Yancey)",
    "Hyde Park, Roslindale (Timothy McCarthy)",
    "Jamaica Plain, West Roxbury (Matt O’Malley)",
    "Roxbury (Tito Jackson)",
    "Back Bay, Beacon Hill, Fenway/Kenmore, Mission Hill, West End (Josh Zakim)",
    "Allston/Brighton (Mark Ciommo)"
  ),
  stringsAsFactors = FALSE
)

sr_data <- sr_data %>%
  left_join(department_lookup, by = "department")

sr_data <- sr_data %>%
  mutate(city_council_district = as.character(city_council_district)) %>%
  left_join(district_lookup, by = "city_council_district")



# Convert 'open_dt' and 'closed_dt' to datetime objects and calculate resolution time in hours
sr_data$open_dt <- as.Date(sr_data$open_dt, format="%Y-%m-%d")
sr_data$closed_dt <- as.Date(sr_data$closed_dt, format="%Y-%m-%d")
sr_data$response_time <- as.numeric(difftime(sr_data$closed_dt, sr_data$open_dt, units = "days"))
sr_data$resolution_time <- as.numeric(difftime(sr_data$closed_dt, sr_data$open_dt, units = "hours"))

# -----------------------------
# Descriptive Statistics
# -----------------------------

# Get a summary of the dataset
summary(sr_data)

# Clean the data (if applicable, assuming `data_clean` is a processed version of `sr_data`)
data_clean <- sr_data

# Neighborhood-level Summary: Group data by neighborhood and calculate statistics
neighborhood_summary <- data_clean %>% 
  group_by(neighborhood) %>% 
  summarize( 
    total_requests = n(),  # Total number of requests per neighborhood
    pct_on_time = mean(on_time == "ONTIME", na.rm = TRUE) * 100,  # Percentage of on-time requests
    avg_response_time = mean(response_time, na.rm = TRUE)  # Average response time
  ) %>% 
  arrange(desc(total_requests))  # Sort neighborhoods by total requests

# Yearly Summary: Group data by year and calculate statistics
yearly_summary <- data_clean %>% 
  group_by(Year) %>% 
  summarize( 
    total_requests = n(),  # Total number of requests per year
    pct_on_time = mean(on_time == "ONTIME", na.rm = TRUE) * 100,  # Percentage of on-time requests
    avg_response_time = mean(response_time, na.rm = TRUE),  # Average response time per year
    median_response_time = median(response_time, na.rm = TRUE)  # Median response time per year
  )

# Yearly on-time statistics: Group by year and summarize on-time vs overdue requests
yearly_ontime_stats <- data_clean %>% 
  group_by(Year) %>% 
  summarize( 
    total_requests = n(),  # Total number of requests per year
    ontime_requests = sum(on_time == "ONTIME", na.rm = TRUE),  # Total on-time requests
    late_requests = sum(on_time == "OVERDUE", na.rm = TRUE),  # Total overdue requests
    pct_on_time = mean(on_time == "ONTIME", na.rm = TRUE) * 100,  # Percentage of on-time requests
    avg_response_time = mean(response_time, na.rm = TRUE)  # Average response time per year
  )

# Save the on-time stats as a CSV file
write.csv(yearly_ontime_stats, "Outputs/Project/3/yearly_ontime_stats.csv", row.names = FALSE)

# -----------------------------
# Graph 1: Line Chart of On-time Resolution Percentage by Year
# -----------------------------

q1_plot1 <- ggplot(yearly_summary, aes(x = Year, y = pct_on_time, group = 1)) + 
  geom_line(size = 1.2, color = "steelblue") +  # Line for on-time resolution percentage
  geom_point(size = 3, color = "darkblue") +  # Points on the line
  geom_text(aes(label = sprintf("%.1f%%", pct_on_time)), vjust = -1) +  # Display percentage labels
  labs(title = "On-time Resolution Percentage Trend (2015-2019)", 
       subtitle = "Has service request resolution improved over time?", 
       x = "Year",
       y = "% Resolved On Time") + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(face = "bold", size = 14), 
    axis.title = element_text(face = "bold"), 
    panel.grid.minor = element_blank() 
  )

# Save the plot
ggsave("Outputs/Project/3/ontime_resolution_trend.png", q1_plot1, width = 10, height = 6, dpi = 300)

# -----------------------------
# Graph 2: Stacked Bar Chart Showing Volume and Resolution Status by Year
# -----------------------------

# Calculate the count of requests by resolution status per year
yearly_status_counts <- data_clean %>% 
  group_by(Year, on_time) %>% 
  summarize(count = n()) %>% 
  ungroup()

q1_plot2 <- ggplot(yearly_status_counts, aes(x = Year, y = count, fill = on_time)) + 
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold") +  # Display the count in each bar
  scale_fill_manual(values = c("ONTIME" = "#2E8B57", "OVERDUE" = "#CD5C5C")) +  # Custom color scheme
  labs(title = "Service Request Volume and Resolution Status by Year", 
       subtitle = "Comparison of on-time vs. overdue resolutions", 
       x = "Year", y = "Number of Requests", fill = "Resolution Status") + 
  theme_minimal() + 
  theme( 
    legend.position = "bottom", 
    plot.title = element_text(face = "bold", size = 14), 
    axis.title = element_text(face = "bold") 
  )

# Save the plot
ggsave("Outputs/Project/3/yearly_resolution_status.png", q1_plot2, width = 10, height = 6, dpi = 300)

# -----------------------------
# Department vs. Case Status Contingency Table
# -----------------------------

# Create contingency table for department and case status
dept_status_table <- table(data_clean$department, data_clean$case_status) 
dept_status_df <- as.data.frame(dept_status_table)  # Convert to data frame
names(dept_status_df) <- c("Department", "Status", "Count")  # Rename columns

# Calculate percentages for each department
dept_status_summary <- dept_status_df %>% 
  group_by(Department) %>% 
  mutate(Total = sum(Count), 
         Percentage = round(Count / Total * 100, 1)) %>%  # Calculate percentage of cases per department
  arrange(desc(Total), Department, Status)  # Sort by total cases, department, and status

# Save the summary to CSV
write.csv(dept_status_summary, "Outputs/Project/3/dept_status_summary.csv", row.names = FALSE)

# Chi-squared test for department vs. case status
chisq_dept_status <- chisq.test(dept_status_table)  # Perform Chi-square test
print(chisq_dept_status)  # Display test result

# -----------------------------
# Top 5 Departments Case Status Distribution Plot
# -----------------------------

# Get the top 5 departments by request volume
top_departments <- data_clean %>% 
  count(department) %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(department)

# Filter the department status data for top departments
dept_status_top <- dept_status_summary %>% 
  filter(Department %in% top_departments)

# Plot the case status distribution for the top 5 departments
q2_plot1 <- ggplot(dept_status_top, aes(x = department, y = percentage, fill = case_status)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() +  # Flip the coordinates for better readability
  scale_fill_brewer(palette = "Set2") + 
  labs(title = "Case Status Distribution by Top 5 Departments", 
       subtitle = "Focus on most frequent service request handlers", 
       x = "Department", y = "Percentage", fill = "Case Status") + 
  theme_minimal() + 
  theme( 
    plot.title = element_text(face = "bold"), 
    legend.position = "bottom" 
  )

# -----------------------------
# Multiple Regression Analysis
# -----------------------------

# Run linear regression model with department, neighborhood, and year as predictors
resolution_model <- lm(response_time ~ department + neighborhood + Year, data = data_clean)
model_summary <- summary(resolution_model)  # Summarize the model

# Display coefficients and statistics
coef_data <- as.data.frame(coef(summary(resolution_model))) 
coef_data$Variable <- rownames(coef_data)  # Add variable names as a column
coef_data <- coef_data %>% 
  filter(abs(`t value`) > 2) %>%  # Filter for significant coefficients
  arrange(desc(abs(`t value`))) %>% 
  select(Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)

# Save regression coefficients to CSV
write.csv(coef_data, "Outputs/Project/3/resolution_time_factors.csv", row.names = FALSE)

# Boxplot for department resolution times
dept_response_data <- data_clean %>% 
  filter(!is.na(response_time)) %>%  # Remove rows with NA response time
  group_by(department) %>% 
  summarize(
    median_time = median(response_time, na.rm = TRUE), 
    count = n()
  ) %>% 
  arrange(desc(count)) %>% 
  head(10)  # Get top 10 departments by request volume

# Plot boxplot for top 10 departments' response times
top_depts <- dept_response_data$department
q3_plot1 <- ggplot(data_clean %>% filter(department %in% top_depts),
                   aes(x = reorder(department, response_time, FUN = median), y = response_time)) + 
  geom_boxplot(aes(fill = department), alpha = 0.7, outlier.shape = NA) + 
  coord_flip() +  # Flip coordinates for better readability
  ylim(0, quantile(data_clean$response_time, 0.95, na.rm = TRUE)) +  # Limit y-axis to 95th percentile
  scale_fill_brewer(palette = "Spectral") + 
  labs(title = "Resolution Time Distribution by Department", 
       subtitle = "Boxplots for top 10 departments by request volume", 
       x = "Department", y = "Resolution Time (hours)") + 
  theme_minimal() + 
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold", size = 14), 
    axis.title = element_text(face = "bold")
  )

# Save the boxplot
ggsave("Outputs/Project/3/dept_resolution_time.png", q3_plot1, width = 12, height = 8, dpi = 300)

# -----------------------------
# Logistic Regression for On-time Resolution
# -----------------------------

# Clean and prepare data for logistic regression
clean_data <- sr_data %>%
  mutate(
    on_time_binary = ifelse(on_time == "ONTIME", 1, 0),  # Convert on_time to binary (1 = ONTIME, 0 = LATE)
    year = year(open_dt)  # Extract year from open_dt
  ) %>%
  filter(!is.na(on_time_binary) & !is.na(year))  # Remove rows with missing values

# Build the logistic regression model
logit_model <- glm(on_time_binary ~ year, data = clean_data, family = "binomial")

# Display the summary of the logistic regression model
summary(logit_model)

# Calculate yearly on-time resolution rate and total cases
yearly_rate <- clean_data %>%
  group_by(year) %>%
  summarise(
    on_time_rate = mean(on_time_binary),
    total_cases = n()
  )

# Display yearly statistics
print(yearly_rate)

# Plot on-time resolution rate by year
ggplot(yearly_rate, aes(x = year, y = on_time_rate)) + 
  geom_line(color = "blue", linewidth = 1.2) + 
  geom_point(size = 3) + 
  labs(title = "On-Time Resolution Rate by Year (2015–2019)", 
       x = "Year", 
       y = "On-Time Rate") + 
  theme_minimal()

# Calculate and display Odds Ratios
exp(coef(logit_model))

# -----------------------------
# Chi-Square Test: Department vs. Case Status
# -----------------------------

# Prepare the contingency table for Chi-square test
table_data <- table(sr_data$department_full_name, sr_data$case_status)

# Filter departments with at least 50 total cases
valid_depts <- rowSums(table_data) >= 50
filtered_table <- table_data[valid_depts, ]

# Print filtered contingency table
print("Filtered Contingency Table:")
print(filtered_table)

# Perform the Chi-square test
chi_result <- chisq.test(filtered_table)

# Show test result and expected counts
print("Chi-Square Test Result:")
print(chi_result)
print("Expected Counts:")
print(chi_result$expected)


# Get the observed values
observed_values <- as.vector(filtered_table)

# Get the expected values
expected_values <- as.vector(chi_result$expected)

# Calculate the chi-square components (Observed - Expected)^2 / Expected
chi_sq_components <- (observed_values - expected_values)^2 / expected_values

# Create a data frame for better compatibility with flextable
department_names <- rep(rownames(filtered_table), ncol(filtered_table))  # Repeat departments
case_status_names <- rep(colnames(filtered_table), each = nrow(filtered_table))  # Repeat case statuses

# Create a data frame with the Chi-Square details
chi_sq_details_df <- data.frame(
  Department = department_names,
  Case_Status = case_status_names,
  Observed = observed_values,
  Expected = expected_values,
  Chi_Square_Component = chi_sq_components
)

# Create a flextable from the data frame
chi_sq_ft <- flextable(chi_sq_details_df)
chi_sq_ft <- theme_vanilla(chi_sq_ft)
chi_sq_ft <- color(chi_sq_ft, part = "footer", color = "#666666")

# Sample size and critical value information (to be modified accordingly)
sample_size <- sum(filtered_table)  # Total sample size
alpha <- 0.05  # Significance level
critical_value <- qchisq(1 - alpha, df = (length(rownames(filtered_table)) - 1) * (length(colnames(filtered_table)) - 1))  # Chi-Square critical value

# Add footer lines with summary information
chi_sq_ft <- set_caption(chi_sq_ft, caption = "Chi-Square Test: Department vs Case Status")
chi_sq_ft <- add_footer_lines(chi_sq_ft, paste("Sample Size:", sample_size))
chi_sq_ft <- add_footer_lines(chi_sq_ft, paste("Critical Value (alpha =", alpha, "):", round(critical_value, 2)))
chi_sq_ft <- add_footer_lines(chi_sq_ft, paste("Chi-Square Statistic:", round(chi_result$statistic, 2)))

# Create a Word document and add the flextable
library(officer)
doc <- read_docx()  # Create a new Word document
doc <- doc %>%
  body_add_flextable(chi_sq_ft)  # Add the flextable to the document

# Save the document to the desired path
output_path <- "Outputs/Project/3/chi_square_test_results.docx"
print(doc, target = output_path)

# Print confirmation
cat("Word document has been saved to:", output_path)


# -----------------------------
# Linear Regression for Resolution Time
# -----------------------------


# Drop rows where resolution time is NA or zero
sr_data_lr <- sr_data %>%
  filter(!is.na(resolution_time) & resolution_time > 0)

# Remove rows with missing department_full_name or city_council_district_details
sr_data_lr <- sr_data_lr %>%
  filter(!is.na(department_full_name) & !is.na(city_council_district_details))

# Convert 'department_full_name', 'city_council_district_details' and 'Year' to factors (categorical variables)
sr_data_lr$department_full_name <- as.factor(sr_data_lr$department_full_name)
sr_data_lr$city_council_district_details <- as.factor(sr_data_lr$city_council_district_details)
sr_data_lr$Year <- as.factor(sr_data_lr$Year) 

# Run the linear regression model
model <- lm(resolution_time ~ department_full_name + city_council_district_details + Year, data = sr_data_lr)

# Display the summary of the model
summary(model)


# Arrange multiple plots in one window
par(mfrow = c(2, 2))
plot(model, which = 1)  # Residuals vs Fitted
plot(model, which = 2)  # Normal Q-Q Plot
plot(model, which = 3)  # Scale-Location Plot
plot(model, which = 5)  # Residuals vs Leverage Plot


eff_dept_fullname <- effect("department_full_name", model)

# Convert the effect object to a data frame
eff_df <- as.data.frame(eff_dept_fullname)

# Create the plot
plot_dept <- ggplot(eff_df, aes(x = department_full_name, y = fit)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  labs(
    title = "Effect of Department on Resolution Time (Hrs)",
    x = "Department Full Name",
    y = eff_dept_fullname$y.name  # Automatically uses the response variable name
  ) +
  theme_bw() + # A clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10), # Rotate X labels
    axis.title.x = element_text(margin = margin(t = 10)), # Add some space for X axis title
    plot.title = element_text(hjust = 0.5) # Center plot title
  )

print(plot_dept)


eff_city_council <- effect("city_council_district_details", model)

# Convert the effect object to a data frame
eff_city_council_df <- as.data.frame(eff_city_council)

eff_city_council_df$city_council_district_details <- str_wrap(eff_city_council_df$city_council_district_details, width = 40)

plot_cc <- ggplot(eff_city_council_df, aes(x = city_council_district_details, y = fit)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  labs(
    x = "Neighborhood",
    y = eff_city_council$y.name
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_flip()

print(plot_cc)


eff_year <- effect("Year", model)

# Convert the effect object to a data frame
eff_year_df <- as.data.frame(eff_year)

# Create the plot
plot_year <- ggplot(eff_year_df, aes(x = Year, y = fit)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
  labs(
    #title = "Effect of Neighborhood on Resolution Time (Hrs)",
    x = "Year",
    y = eff_year$y.name  # Automatically uses the response variable name
  ) +
  theme_bw() + # A clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11), # Rotate X labels
    axis.title.x = element_text(margin = margin(t = 12)), # Add some space for X axis title
    axis.text.y = element_text(size = 11), 
    plot.title = element_text(hjust = 0.5) # Center plot title
  )

print(plot_year)




# Create the summary table using gtsummary
tbl <- 
  list(model) %>% 
  map(
    ~ tbl_regression(.x, conf.int = F) %>%  # Regression table for model
      add_glance_table(
        label = list(sigma ~ "Residual standard error"),  # Add residual error
        include = c(adj.r.squared, AIC, sigma)  # Include R-squared, AIC, and residual error
      )
  )

#Extract the first element from the list (the regression table)
tbl_regression <- tbl[[1]]

# Print the summary table
tbl_regression

# Export the table as a DOCX file using gt
tbl_regression %>%
  as_gt() %>%
  gt::gtsave(filename = "Outputs/Project/3/summary_table.docx")



# Fit the models
model1 <- lm(resolution_time ~ Year, data = sr_data_lr)
model2 <- lm(resolution_time ~ Year + department_full_name, data = sr_data_lr)
model3 <- lm(resolution_time ~ Year + department_full_name + city_council_district_details, data = sr_data_lr)

# Create a list of models
models <- list(model1, model2, model3)

# Generate a list of regression tables with model summaries
tbl_list <- models %>%
  map(~ tbl_regression(.x, conf.int = FALSE) %>%
        add_glance_table(include = c(adj.r.squared, AIC, sigma),
                         label = list(sigma ~ "Residual Std. Error"))
  )

# Merge all regression tables side-by-side for comparison
tbl_combined <- tbl_merge(
  tbls = tbl_list,
  tab_spanner = c("**Model 1: Year**", "**Model 2: Year + department_full_name**", "**Model 3: Year + department_full_name + City Council District**")
)


# Print the combined table
tbl_combined

sd(sr_data_lr$resolution_time)

# Export the table as a DOCX file
tbl_combined %>%
  as_gt() %>%
  gtsave(filename = "Outputs/Project/3/CoefficientComparisonTable.docx")

