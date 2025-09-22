# =============================================================
# app.R — Interactive Shiny Dashboard (Colorblind-friendly)
# Türkçe: İnteraktif gösterge paneli. Erişilebilir (renk-körü dostu) tema.
# English: Interactive dashboard. Accessible (colorblind-friendly) theme.
# =============================================================

# ---------------------------
# 0) Libraries / Kütüphaneler
# ---------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(scales)
})

# ---------------------------------------
# 1) Load data / Verileri yükle (CSV)
# ---------------------------------------
message("[Shiny] Loading datasets ...")

DT_data          <- fread("data/data_clean.csv")
DT_holidays      <- fread("data/holidays_clean.csv")
DT_items         <- fread("data/items_clean.csv")
DT_oil           <- fread("data/oil_clean.csv")
DT_stores        <- fread("data/stores_clean2.csv")
DT_transactions  <- fread("data/transactions_clean.csv")

setDT(DT_data); setDT(DT_holidays); setDT(DT_items); setDT(DT_oil); setDT(DT_stores); setDT(DT_transactions)
for (DT in list(DT_data, DT_holidays, DT_oil, DT_transactions)) {
  if ("date" %in% names(DT)) DT[, date := as.Date(date)]
}

# Standardize column names
rename_col <- function(dt, candidates, standard) {
  for (cand in candidates) {
    if (cand %in% names(dt) && cand != standard) { setnames(dt, cand, standard); break }
  }
}
rename_col(DT_stores, c("store_nbr","store","store_id"), "store_nbr")
rename_col(DT_stores, c("state","State","province"), "state")
rename_col(DT_stores, c("city","City","town"), "city")
rename_col(DT_stores, c("type","store_type","Type"), "store_type")
rename_col(DT_items,  c("item_nbr","item_id","sku"), "item_nbr")
rename_col(DT_items,  c("family","Family","item_family"), "family")
rename_col(DT_items,  c("perishable","is_perishable","Perishable"), "perishable")
rename_col(DT_data,   c("store_nbr","store","store_id"), "store_nbr")
rename_col(DT_data,   c("item_nbr","item_id","sku"), "item_nbr")
rename_col(DT_data,   c("sales","Sales","units","unit_sales"), "sales")
rename_col(DT_oil,    c("dcoilwtico","oil_price","Price","WTI"), "oil_price")

# Type consistency fix
DT_data[,  store_nbr := as.integer(store_nbr)]
DT_stores[,store_nbr := as.integer(store_nbr)]
DT_data[,  item_nbr  := as.integer(item_nbr)]
DT_items[, item_nbr  := as.integer(item_nbr)]

setkey(DT_data, store_nbr, item_nbr, date)
setkey(DT_stores, store_nbr)
setkey(DT_items, item_nbr)
setkey(DT_oil, date)
setkey(DT_holidays, date)

# ----------------------------------------------------
# 2) Visual defaults — Accessible theme & palette
# ----------------------------------------------------
okabe_ito <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
cb_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 8)),
      legend.position = "bottom"
    )
}

theme_set(cb_theme())

# -----------------------------
# 3) UI / Arayüz
# -----------------------------
ui <- fluidPage(
  titlePanel("Sales Analytics Dashboard — Interactive"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "dr", label = "Date Range / Tarih Aralığı",
        start = min(DT_data$date, na.rm = TRUE), end = max(DT_data$date, na.rm = TRUE),
        min   = min(DT_data$date, na.rm = TRUE), max = max(DT_data$date, na.rm = TRUE)
      ),
      selectInput(
        "state", label = "State / Eyalet",
        choices = c("All", sort(unique(na.omit(DT_stores$state)))), selected = "All"
      ),
      selectInput(
        "stype", label = "Store Type / Mağaza Tipi",
        choices = c("All", sort(unique(na.omit(DT_stores$store_type)))), selected = "All"
      ),
      selectizeInput(
        "family", label = "Item Family / Ürün Ailesi",
        choices = c("All", sort(unique(na.omit(DT_items$family)))), selected = "All"
      ),
      checkboxInput("only_weekend", label = "Only Weekends / Sadece Hafta Sonu", value = FALSE),
      checkboxInput("only_holiday", label = "Only Holidays / Sadece Resmi Tatil", value = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Q1 — Top Store", plotOutput("q1_plot"), DTOutput("q1_table")),
        tabPanel("Q2 — West-Bengal Family", plotOutput("q2_plot"), DTOutput("q2_table")),
        tabPanel("Q3 — Oil vs Sales", plotOutput("q3_ts"), plotOutput("q3_sc"), DTOutput("q3_stats")),
        tabPanel("Q4 — Weekdays & Holidays", plotOutput("q4_wd"), plotOutput("q4_we"), plotOutput("q4_hd")),
        tabPanel("Q5 — Negative Sales", plotOutput("q5_hist"), plotOutput("q5_topstores"), plotOutput("q5_ts")),
        tabPanel("Q6 — Store Types", plotOutput("q6_plot"), DTOutput("q6_table")),
        tabPanel("Q7 — States excl. WB", plotOutput("q7_plot"), DTOutput("q7_table")),
        tabPanel("Q8 — Perishability", plotOutput("q8_plot"), DTOutput("q8_table")),
        tabPanel("Q9 — Top Items", plotOutput("q9_plot"), DTOutput("q9_table"))
      )
    )
  )
)

# ---------------------------------------
# 4) Server / Sunucu mantığı
# ---------------------------------------
server <- function(input, output, session) {
  
  filtered_sales <- reactive({
    dt <- DT_data[date >= input$dr[1] & date <= input$dr[2]]
    if (input$state != "All") dt <- merge(dt, DT_stores[state == input$state, .(store_nbr)], by = "store_nbr")
    if (input$stype != "All") dt <- merge(dt, DT_stores[store_type == input$stype, .(store_nbr)], by = "store_nbr")
    if (input$family != "All") dt <- merge(dt, DT_items[family == input$family, .(item_nbr)], by = "item_nbr")
    if (input$only_weekend) dt <- dt[wday(date, week_start = 1) %in% c(6,7)]
    if (input$only_holiday) dt <- merge(dt, unique(DT_holidays[, .(date)]), by = "date")
    dt
  })
  
  # ---------------- Q1 -----------------
  output$q1_plot <- renderPlot({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = store_nbr][order(-total_sales)]
    dt <- merge(dt, DT_stores[, .(store_nbr, city, state, store_type)], by = "store_nbr", all.x = TRUE)
    dt <- dt[order(-total_sales)]  # ensure descending order
    validate(need(nrow(dt) > 0, "No data / Veri yok"))
    ggplot(dt[1:min(.N, 15)], aes(x = reorder(paste0(store_nbr, " — ", city), total_sales), y = total_sales)) +
      geom_col(fill = okabe_ito[3]) + coord_flip() +
      labs(title = "Top Stores by Total Sales / Toplam Satışa Göre Mağazalar",
           x = "Store — City / Mağaza — Şehir", y = "Total Sales / Toplam Satış") +
      scale_y_continuous(labels = label_comma())
  })
  output$q1_table <- renderDT({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = store_nbr][order(-total_sales)]
    dt <- merge(dt, DT_stores[, .(store_nbr, city, state, store_type)], by = "store_nbr", all.x = TRUE)
    dt <- dt[order(-total_sales)]
    datatable(dt, options = list(pageLength = 10), rownames = FALSE)
  })
  

  
  # ---------------- Q2 -----------------
  # TR: West-Bengal'da en çok satan ürün ailesi
  # EN: Most sold item family in West-Bengal
  output$q2_plot <- renderPlot({
    wb <- DT_stores[state == "West-Bengal", .(store_nbr)]
    dt <- DT_data[wb, on = "store_nbr", nomatch = 0L]
    dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
    fam <- dt[, .(total_sales = sum(sales, na.rm = TRUE)), by = family][order(-total_sales)]
    topN <- fam[1:min(15, .N)]
    ggplot(topN, aes(x = reorder(family, total_sales), y = total_sales)) +
      geom_col(fill = okabe_ito[5]) + coord_flip() +
      labs(title = "West-Bengal — Top Families by Sales",
           x = "Family / Aile", y = "Total Sales / Toplam Satış") +
      scale_y_continuous(labels = label_comma())
  })
  output$q2_table <- renderDT({
    wb <- DT_stores[state == "West-Bengal", .(store_nbr)]
    dt <- DT_data[wb, on = "store_nbr", nomatch = 0L]
    dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
    fam <- dt[, .(total_sales = sum(sales, na.rm = TRUE)), by = family][order(-total_sales)]
    datatable(fam, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---------------- Q3 -----------------
  # TR: Petrol fiyatı & satış ilişkisi — zaman serisi + scatter + korelasyon tablosu
  # EN: Oil vs Sales — time series + scatter + correlation table
  output$q3_ts <- renderPlot({
    dt <- filtered_sales()[, .(daily_sales = sum(sales, na.rm = TRUE)), by = date][order(date)]
    oil <- DT_oil[date >= input$dr[1] & date <= input$dr[2]]
    ser <- merge(dt, oil[, .(date, oil_price)], by = "date", all.x = TRUE)
    ser[, sales_z := as.numeric(scale(daily_sales))]
    ser[, oil_z   := as.numeric(scale(oil_price))]
    ggplot(ser, aes(x = date)) +
      geom_line(aes(y = sales_z), linewidth = 0.7, color = okabe_ito[3]) +
      geom_line(aes(y = oil_z),   linewidth = 0.7, linetype = "dashed", color = okabe_ito[6]) +
      labs(title = "Daily Sales (solid) vs Oil (dashed) — Z-scores",
           x = "Date / Tarih", y = "Z-score")
  })
  output$q3_sc <- renderPlot({
    dt <- filtered_sales()[, .(daily_sales = sum(sales, na.rm = TRUE)), by = date][order(date)]
    oil <- DT_oil[date >= input$dr[1] & date <= input$dr[2]]
    ser <- merge(dt, oil[, .(date, oil_price)], by = "date", all.x = TRUE)
    ggplot(ser, aes(x = oil_price, y = daily_sales)) +
      geom_point(alpha = 0.5, color = okabe_ito[4]) +
      geom_smooth(method = "lm", se = TRUE, color = okabe_ito[1]) +
      labs(title = "Oil Price vs Daily Sales",
           x = "Oil Price / Petrol Fiyatı", y = "Daily Sales / Günlük Satış")
  })
  output$q3_stats <- renderDT({
    dt <- filtered_sales()[, .(daily_sales = sum(sales, na.rm = TRUE)), by = date]
    oil <- DT_oil[date >= input$dr[1] & date <= input$dr[2]]
    ser <- merge(dt, oil[, .(date, oil_price)], by = "date", all.x = TRUE)
    ser[, `:=`(d_sales = c(NA, diff(daily_sales)), d_oil = c(NA, diff(oil_price)))]
    out <- data.table(
      metric = c("pearson","spearman","diff_pearson"),
      value  = c(
        suppressWarnings(cor(ser$daily_sales, ser$oil_price, use = "complete.obs", method = "pearson")),
        suppressWarnings(cor(ser$daily_sales, ser$oil_price, use = "complete.obs", method = "spearman")),
        suppressWarnings(cor(ser$d_sales, ser$d_oil, use = "complete.obs", method = "pearson"))
      )
    )
    datatable(out, options = list(dom = 't'), rownames = FALSE)
  })
  
  # ---------------- Q4 -----------------
  # TR: Haftanın günleri, hafta sonu ve tatil etkileri
  # EN: Weekdays, weekend and holiday effects
  output$q4_wd <- renderPlot({
    sd <- filtered_sales()[, .(sales = sum(sales, na.rm = TRUE)), by = .(date, store_nbr)]
    sd[, weekday := wday(date, label = TRUE, abbr = TRUE, week_start = 1)]
    ag <- sd[, .(avg_sales = mean(sales), med_sales = median(sales)), by = weekday]
    ggplot(ag, aes(x = weekday, y = avg_sales)) +
      geom_col(fill = okabe_ito[2]) +
      labs(title = "Average Sales by Weekday / Günlere Göre Ortalama Satış",
           x = "Weekday / Gün", y = "Average Sales / Ortalama")
  })
  output$q4_we <- renderPlot({
    sd <- filtered_sales()[, .(sales = sum(sales, na.rm = TRUE)), by = .(date, store_nbr)]
    sd[, is_weekend := wday(date, week_start = 1) %in% c(6,7)]
    ggplot(sd, aes(x = is_weekend, y = sales)) +
      geom_boxplot(outlier.alpha = 0.2, fill = okabe_ito[7]) +
      labs(title = "Weekend vs Weekday / Hafta Sonu vs Hafta İçi",
           x = "Is Weekend? / Hafta Sonu?", y = "Sales / Satış")
  })
  output$q4_hd <- renderPlot({
    sd <- filtered_sales()[, .(sales = sum(sales, na.rm = TRUE)), by = .(date, store_nbr)]
    hdays <- unique(DT_holidays[, .(date)])
    sd[, is_holiday := date %in% hdays$date]
    ggplot(sd, aes(x = is_holiday, y = sales)) +
      geom_boxplot(outlier.alpha = 0.2, fill = okabe_ito[5]) +
      labs(title = "Holiday vs Non-Holiday / Tatil vs Değil",
           x = "Is Holiday? / Tatil?", y = "Sales / Satış")
  })
  
  # ---------------- Q5 -----------------
  # TR: Negatif satış görselleştirmeleri
  # EN: Negative sales visualizations
  output$q5_hist <- renderPlot({
    neg <- filtered_sales()[sales < 0]
    if (nrow(neg) == 0) return(NULL)
    neg[, neg_mag := abs(sales)]
    ggplot(neg, aes(x = neg_mag)) +
      geom_histogram(bins = 50, fill = okabe_ito[6]) +
      labs(title = "Negative Sales Magnitudes / Negatif Satış Büyüklükleri",
           x = "|sales|", y = "Count / Sayı")
  })
  output$q5_topstores <- renderPlot({
    neg <- filtered_sales()[sales < 0]
    if (nrow(neg) == 0) return(NULL)
    agg <- neg[, .N, by = store_nbr][order(-N)][1:min(15, .N)]
    ggplot(agg, aes(x = reorder(factor(store_nbr), N), y = N)) +
      geom_col(fill = okabe_ito[4]) + coord_flip() +
      labs(title = "Top Stores by Negative Rows / Negatif Satır — Mağazalar",
           x = "Store Nbr / Mağaza No", y = "# Negative Rows / Negatif Satır")
  })
  output$q5_ts <- renderPlot({
    neg <- filtered_sales()[sales < 0]
    if (nrow(neg) == 0) return(NULL)
    ts <- neg[, .N, by = date][order(date)]
    ggplot(ts, aes(x = date, y = N)) +
      geom_line(linewidth = 0.9, color = okabe_ito[3]) +
      labs(title = "Negative Rows Over Time / Zaman İçinde Negatif Satırlar",
           x = "Date / Tarih", y = "Count / Sayı")
  })
  
  # ---------------- Q6 -----------------
  output$q6_plot <- renderPlot({
    st <- DT_stores
    if (input$state != "All") st <- st[state == input$state]
    gg <- st[, .N, by = store_type][order(-N)]
    ggplot(gg, aes(x = reorder(store_type, N), y = N)) +
      geom_col(fill = okabe_ito[8]) + coord_flip() +
      labs(title = "Store Type Frequency / Mağaza Tipi Sıklığı",
           x = "Store Type / Mağaza Tipi", y = "Count / Sayı")
  })
  output$q6_table <- renderDT({
    st <- DT_stores
    if (input$state != "All") st <- st[state == input$state]
    gg <- st[, .N, by = store_type][order(-N)]
    datatable(gg, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---------------- Q7 -----------------
  output$q7_plot <- renderPlot({
    st <- DT_stores[state != "West-Bengal"]
    gg <- st[, .N, by = state][order(-N)][1:min(15, .N)]
    ggplot(gg, aes(x = reorder(state, N), y = N)) +
      geom_col(fill = okabe_ito[2]) + coord_flip() +
      labs(title = "States by #Stores (excl. West-Bengal)",
           x = "State / Eyalet", y = "# Stores / Mağaza Sayısı")
  })
  output$q7_table <- renderDT({
    st <- DT_stores[state != "West-Bengal"]
    gg <- st[, .N, by = state][order(-N)]
    datatable(gg, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---------------- Q8 -----------------
  output$q8_plot <- renderPlot({
    it <- copy(DT_items)
    if (!"perishable" %in% names(it)) it[, perishable := NA_integer_]
    it[, perishable := as.integer(perishable)]
    it[, perishable_label := ifelse(perishable == 1, "Perishable", "Non-Perishable")]
    cc <- it[, .N, by = perishable_label][order(-N)]
    ggplot(cc[!is.na(perishable_label)], aes(x = perishable_label, y = N)) +
      geom_col(fill = okabe_ito[5]) +
      labs(title = "Items by Perishability / Ürün Dayanıklılığı",
           x = "Category / Kategori", y = "Count / Sayı")
  })
  output$q8_table <- renderDT({
    it <- copy(DT_items)
    if (!"perishable" %in% names(it)) it[, perishable := NA_integer_]
    it[, perishable := as.integer(perishable)]
    it[, perishable_label := ifelse(perishable == 1, "Perishable", "Non-Perishable")]
    cc <- it[, .N, by = perishable_label][order(-N)]
    datatable(cc, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ---------------- Q9 -----------------
  output$q9_plot <- renderPlot({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = item_nbr][order(-total_sales)]
    dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
    topN <- dt[1:min(20, .N)]
    ggplot(topN, aes(x = reorder(paste0(item_nbr, ifelse(!is.na(family), paste0(" — ", family), "")), total_sales), y = total_sales)) +
      geom_col(fill = okabe_ito[6]) + coord_flip() +
      labs(title = "Top Items by Total Sales / En Çok Satan Ürünler",
           x = "Item — Family / Ürün — Aile", y = "Total Sales / Toplam Satış") +
      scale_y_continuous(labels = label_comma())
  })
  output$q9_table <- renderDT({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = item_nbr][order(-total_sales)]
    dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
    datatable(dt, options = list(pageLength = 10), rownames = FALSE)
  })
}

# -----------------------------
# 5) Run app / Uygulamayı çalıştır
# -----------------------------
shinyApp(ui, server)
