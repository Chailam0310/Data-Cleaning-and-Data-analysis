library(dplyr)
library(plyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(VIM)
options(scipen = 999)

# import dataset
df = read.csv("C:\\Users\\chaiz\\Desktop\\R language\\Guideline\\credit score classification data.csv")
df

# check structure of database
head(df)
tail(df)
str(df)
summary(df)

# Data Cleaning Start

names(df) <- tolower(names(df))
colnames(df)

# Replace all empty column to NA
df <- mutate_all(df, ~replace(., . == "", NA)) # dplyr

colSums(is.na(df)) # check columns that are not available

# Sort the data by 'Customer_ID' and 'Month'
df$month <- factor(df$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August"))
df <- df %>% arrange(customer_id, month)

## Function to Change data according to common value sort by customer_id
replace_value_with_most_common <- function(df, customer_id_col, name_col) {
  # For each customer_id, find the name with the highest count
  df_counts <- df %>%
    group_by(!!sym(customer_id_col), !!sym(name_col)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    ungroup()
  
  df_most_common <- df_counts %>%
    filter(!is.na(!!sym(name_col))) %>%
    group_by(!!sym(customer_id_col)) %>%
    slice(which.max(count)) %>%
    ungroup()
  
  # Merge the most common name back to the original dataframe
  df <- left_join(df, df_most_common, by = customer_id_col, suffix = c("", ".most_common"))
  
  # Replace different names with the most common name for each customer_id
  df[[name_col]] <- ifelse(is.na(df[[paste0(name_col, ".most_common")]]), df[[name_col]], df[[paste0(name_col, ".most_common")]])
  
  # Remove redundant columns and rename duplicate 'name' column
  df <- df %>%
    select(-starts_with(paste0(name_col, ".most_common")), -count)
  
  return(df)
}

## Function to Replace NA according to common value sort by customer_id
replace_na_with_most_common <- function(df, customer_id_col, name_col) {
  # For each customer_id, find the name with the highest count
  df_counts <- df %>%
    group_by(!!sym(customer_id_col), !!sym(name_col)) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    ungroup()
  
  df_most_common <- df_counts %>%
    filter(!is.na(!!sym(name_col))) %>%
    group_by(!!sym(customer_id_col)) %>%
    slice(which.max(count)) %>%
    ungroup()
  
  # Merge the most common name back to the original dataframe
  df <- left_join(df, df_most_common, by = customer_id_col, suffix = c("", ".most_common"))
  
  # Replace different names with the most common name for each customer_id
  na_values <- is.na(df[[name_col]])
  
  # Replace NA values in df[[name_col]] with values from df[[paste0(name_col, ".most_common")]]
  df[[name_col]][na_values] <- df[[paste0(name_col, ".most_common")]][na_values]
  
  # Remove redundant columns and rename duplicate 'name' column
  df <- df %>%
    select(-starts_with(paste0(name_col, ".most_common")), -count)
  
  return(df)
}


# Name, Occupation, SSN, Monthly_Inhand_Salary, Interest_Rate
df <- replace_value_with_most_common(df, "customer_id", "name")
df <- replace_value_with_most_common(df, "customer_id", "occupation")
df <- replace_value_with_most_common(df, "customer_id", "ssn")
df <- replace_value_with_most_common(df, "customer_id", "monthly_inhand_salary")
df <- replace_value_with_most_common(df, "customer_id", "interest_rate")

# Age
df$age <- gsub("[_-]", "", df$age)
df$age <- as.numeric(df$age)
df$age <- ifelse(df$age < 21, df$age + 20, df$age)

df <- replace_value_with_most_common(df, "customer_id", "age")

# Annual Income (done) (checked)
df$annual_income <- gsub("_", "", df$annual_income)
df$annual_income <- sprintf("%.2f", as.numeric(df$annual_income))
df <- replace_value_with_most_common(df, "customer_id", "annual_income")

# Num_of_Bank and Num_of_Credit Card
df$num_bank_accounts <- gsub("-", "", df$num_bank_accounts)
df <- replace_value_with_most_common(df, "customer_id", "num_bank_accounts")
df$num_bank_accounts[df$num_bank_accounts == 0] <- 1

df <- replace_value_with_most_common(df, "customer_id", "num_credit_card")

# Num_of_Loan
df$num_of_loan <- gsub("[_-]", "", df$num_of_loan)
for (i in 1:nrow(df)) {
  if (!is.na(df$type_of_loan[i]) && nchar(df$type_of_loan[i]) > 0) {
    
    loan_types <- unlist(strsplit(df$type_of_loan[i], ", "))
    
    num_loan_types <- length(loan_types)
    if (num_loan_types != df$num_of_loan[i]) {
      df$num_of_loan[i] <- num_loan_types
    }
  } else if (is.na(df$type_of_loan[i]) || tolower(df$type_of_loan[i]) == "n/a") {  # 如果type_of_loan列为NA或"N/A"，将num_of_loan列设置为0
    df$num_of_loan[i] <- 0
  }
}

# Type_of_Loan
df$type_of_loan <- ifelse(is.na(df$type_of_loan), "Not Applicable", df$type_of_loan)

# Delay_from_due_date
df$delay_from_due_date <- gsub("_", "", df$delay_from_due_date)
as.data.frame(table(df$delay_from_due_date))

# Num_of_Delayed_Payment
df$num_of_delayed_payment <- gsub("[-_]", "", df$num_of_delayed_payment)

# use table to check which value will be outlier (found most correct numbers below 30)
frequency_table <- table(df$num_of_delayed_payment, exclude = NULL)

df$num_of_delayed_payment<-lapply(df$num_of_delayed_payment,function(x) as.numeric(x))

df$num_of_delayed_payment = replace(df$num_of_delayed_payment, df$num_of_delayed_payment > 30, NA)
any(df$num_of_delayed_payment > 30)
df <- replace_na_with_most_common(df, "customer_id", "num_of_delayed_payment")


# Changed_Credit_Limit
df$changed_credit_limit<- gsub("_", NA, df$changed_credit_limit)
df <- replace_na_with_most_common(df, "customer_id", "changed_credit_limit")

# Num_Credit_Inquiries

# use table to check which value will be outlier (found most correct numbers below 20)
frequency_table <- table(df$num_credit_inquiries, exclude = NULL)
frequency_table

df$num_credit_inquiries <- as.numeric(as.character(df$num_credit_inquiries))
df$num_credit_inquiries[df$num_credit_inquiries > 20] <- NA

any(df$num_credit_inquiries > 20) # check any data more than 20 or not

df$customer_id <- as.factor(df$customer_id)

# Perform hot deck imputation
df <- hotdeck(
  df,
  variable = "num_credit_inquiries",
  domain_var = "customer_id"
)
df <- subset(df, select = -num_credit_inquiries_imp)
df$customer_id <- as.character(df$customer_id)

# Credit_Mix
df$credit_mix <- gsub("_", NA, df$credit_mix)
df <- replace_value_with_most_common(df, "customer_id", "credit_mix")

# Outstanding_Debt

df$outstanding_debt <- gsub("_", "", df$outstanding_debt)

# Credit_Utilization_Ratio - Nothing Cleaned

# Credit_History_Age
# Change to months only
df <- df %>%
  dplyr::mutate(
    credit_history_age = if_else(is.na(credit_history_age), NA_real_, as.numeric(str_extract(credit_history_age, "\\d+")) * 12 + as.numeric(str_extract(str_extract(credit_history_age, "and\\s\\d+"), "\\d+")))
  )

# Loop forward through subsequent rows until finding a non-empty value or reaching the last row within each customer_id group
for (i in 1:nrow(df)) {
  if (is.na(df$credit_history_age[i])) {
    loop_counter <- 1
    found_non_empty <- FALSE
    
    # Loop backwards through previous rows until finding a non-empty value or reaching the first row
    while ((i - loop_counter) > 0 && !found_non_empty && loop_counter <= 7) {
      # Check if previous row's credit_history_age is not empty
      if (!is.na(df$credit_history_age[i - loop_counter]) && df$customer_id[i - loop_counter] == df$customer_id[i]) {
        # Update current row's credit_history_age
        df$credit_history_age[i] <- df$credit_history_age[i - loop_counter] + loop_counter
        # Set flag to true
        found_non_empty <- TRUE
      } else {
        # Increment loop counter
        loop_counter <- loop_counter + 1
      }
    }
    
    loop_counter <- 1
    
    # Continue loop until a non-empty value is found or reaching the last row
    while ((i + loop_counter) <= nrow(df) && !found_non_empty && loop_counter <= 7) {
      if (!is.na(df$credit_history_age[i + loop_counter]) && df$customer_id[i + loop_counter] == df$customer_id[i]) {
        df$credit_history_age[i] <- df$credit_history_age[i + loop_counter] - loop_counter
        found_non_empty <- TRUE
      } else {
        loop_counter <- loop_counter + 1
      }
    }
  }
}
# Change back to years and months
df$credit_history_age <- paste(floor(df$credit_history_age / 12), "Years and", df$credit_history_age %% 12, "Months")

# Payment_of_Min_Amount
df$payment_of_min_amount[df$payment_of_min_amount == "NM"] <- NA
df <- replace_value_with_most_common(df, "customer_id", "payment_of_min_amount")

# Total_EMI_per_month
frequency_table <- table(df$total_emi_per_month, exclude = NULL) # 

options(max.print = 15000)
frequency_table

df$total_emi_per_month <- as.numeric(as.character(df$total_emi_per_month))
df$total_emi_per_month = ifelse(df$total_emi_per_month %% 1 == 0 & df$total_emi_per_month != 0, NA, df$total_emi_per_month)

df <- df %>% arrange(customer_id, month)
df$customer_id <- as.factor(df$customer_id)

# Perform hot deck imputation
df <- hotdeck(
  df,
  variable = "total_emi_per_month",
  domain_var = "customer_id"
)
df <- subset(df, select = -total_emi_per_month_imp)
df$customer_id <- as.character(df$customer_id)

# Amount_invested_monthly
df$amount_invested_monthly <- ifelse(grepl("__", df$amount_invested_monthly), NA, df$amount_invested_monthly)
df$amount_invested_monthly <- as.numeric(df$amount_invested_monthly)
median_value <- median(df$amount_invested_monthly, na.rm = TRUE)
df$amount_invested_monthly[is.na(df$amount_invested_monthly)] <- median_value


# Payment_Behaviour
df$payment_behaviour <- gsub("!@9#%8", NA, df$payment_behaviour)
df <- replace_na_with_most_common(df, "customer_id", "payment_behaviour")


# Monthly_Balance
df$monthly_balance <- gsub("_", "", df$monthly_balance)
df$monthly_balance[df$monthly_balance < 0] <- NA
median_value <- median(df$monthly_balance, na.rm = TRUE)
df$monthly_balance[is.na(df$monthly_balance)] <- median_value

# Credit_Score - nothing changed
as.data.frame(table(df$credit_score))

# Remove duplicated
df <- unique(df)
summary(df)
colnames(df) <- tools::toTitleCase(colnames(df))
colSums(is.na(df))
View(df)

