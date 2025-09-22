# ========================================

#  Load required libraries
# ========================================
library(data.table)
library(ggplot2)
library(dplyr)

# ========================================

#  Reload the datasets saved in previous step
# ========================================
data <- fread("data/data_clean.csv")
holidays <- fread("data/holidays_clean.csv")
items <- fread("data/items_clean.csv")
oil <- fread("data/oil_clean.csv")
stores <- fread("data/stores_clean2.csv")
transactions <- fread("data/transactions_clean.csv")

# ========================================
# 1. Inspect data structure
# ========================================
names(data); str(data)
names(holidays); str(holidays)
names(items); str(items)
names(oil); str(oil)
names(stores); str(stores)
names(transactions); str(transactions)

# ========================================
# 2. Check missing values
# ========================================
colSums(is.na(data))
colSums(is.na(holidays))
colSums(is.na(items))
colSums(is.na(oil))
colSums(is.na(stores))
colSums(is.na(transactions))

# ========================================
# 3. Summary statistics
# ========================================
summary(data)
summary(oil)
summary(transactions)

# ========================================
# 4. Convert date columns
# ========================================
data$date <- as.Date(data$date)
oil$date <- as.Date(oil$date)
transactions$date <- as.Date(transactions$date)
holidays$date <- as.Date(holidays$date)

# ========================================
# 5. Simple visualizations
# ========================================

# 
# Trend of sales over time (example)(unit base trend)
ggplot(data, aes(x = date, y = unit_sales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Over Time",
       x = "Date",
       y = "Sales")

# 
# Oil price trend
ggplot(oil, aes(x = date, y = oil_price)) +
  geom_line(color = "red") +
  labs(title = "Oil Prices Over Time",
       x = "Date",
       y = "Price")
