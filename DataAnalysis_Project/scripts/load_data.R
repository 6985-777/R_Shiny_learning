library(data.table)

#  Load the datasets
data <- fread("data/data_clean.csv")
holidays <- fread("data/holidays_clean.csv")
items <- fread("data/items_clean.csv")
oil <- fread("data/oil_clean.csv")
stores <- fread("data/stores_clean2.csv")
transactions <- fread("data/transactions_clean.csv")

# ilk 5 satırı kontrol edelim
head(data)
head(holidays)
head(items)
head(oil)
head(stores)
head(transactions)

#Print dataset dimensions
dim(data); dim(holidays);dim(items); dim(oil); dim(stores); dim(transactions)


#Preview the first 5 rows
head(data)
head(holidays)
head(items)
head(oil)
head(stores)
head(transactions)






