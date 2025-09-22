suppressPackageStartupMessages({
  library(data.table)   
  library(dplyr)        
})

data_clean <- fread("data/data_clean.csv")
items_clean   <- fread("data/items_clean.csv")
stores_clean2 <- fread("data/stores_clean2.csv")

# Control
cat("items_clean boyutları:", dim(items_clean), "\n")
cat("stores_clean2 boyutları:", dim(stores_clean2), "\n")

head(items_clean)
head(stores_clean2)
head(data_clean)



# Sütun isimlerini gör
names(items_clean)
names(stores_clean2)
names(data_clean)
# Ortak olan sütunlar
intersect(names(items_clean), names(stores_clean2))

# Farklı olan sütunlar
setdiff(names(items_clean), names(stores_clean2))
setdiff(names(stores_clean2), names(items_clean))



##############
max_family_wb <- function(data_clean, items_clean, stores_clean2) {
  library(dplyr)
  
  data_clean %>%
    left_join(items_clean,  by = "item_nbr") %>%
    left_join(stores_clean2, by = "store_nbr") %>%
    filter(state == "West Bengal") %>%
    group_by(family) %>%
    summarise(total_sales = sum(unit_sales, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_sales)) %>%
    slice(1) %>%
    print()
}


max_family_wb(data_clean, items_clean, stores_clean2)

#family' i siralama

list_families_wb <- function(data_clean, items_clean, stores_clean2) {
  library(dplyr)
  
  data_clean %>%
    left_join(items_clean,  by = "item_nbr") %>%
    left_join(stores_clean2, by = "store_nbr") %>%
    filter(state == "West Bengal") %>%
    group_by(family) %>%
    summarise(total_sales = sum(unit_sales, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_sales)) %>%
    print()
}
list_families_wb(data_clean, items_clean, stores_clean2)


#grafik
list_families_wb <- function(data_clean, items_clean, stores_clean2) {
  library(dplyr)
  library(ggplot2)
  
  # Tabloyu hazırla
  wb_families <- data_clean %>%
    left_join(items_clean,  by = "item_nbr") %>%
    left_join(stores_clean2, by = "store_nbr") %>%
    filter(state == "West Bengal") %>%
    group_by(family) %>%
    summarise(total_sales = sum(unit_sales, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_sales))
  
  # Tabloyu ekrana yazdır
  print(wb_families)
  
  # Grafik
  ggplot(wb_families, aes(x = reorder(family, total_sales), y = total_sales)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "West Bengal'de Satılan Ürünler (Family Bazında)",
         x = "Family",
         y = "Toplam Satış") +
    theme_minimal()
}

list_families_wb(data_clean, items_clean, stores_clean2)






