# app.R — Interactive Shiny Dashboard (Colorblind-friendly, merged)
# =============================================================

# ---------------------------
# 0) Libraries 
# ---------------------------
suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(ggplot2)
  library(lubridate)
  library(DT)
  library(scales)
  library(dplyr)
})


`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------
# 1) Load data 
# ------------------------------------------------
message("[Shiny] Loading datasets ...")

##################################################################
#1

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

# Type consistency 
DT_data[,  store_nbr := as.integer(store_nbr)]
DT_stores[,store_nbr := as.integer(store_nbr)]
DT_data[,  item_nbr  := as.integer(item_nbr)]
DT_items[, item_nbr  := as.integer(item_nbr)]

# Keys
setkey(DT_data, store_nbr, item_nbr, date)
setkey(DT_stores, store_nbr)
setkey(DT_items, item_nbr)
setkey(DT_oil, date)
setkey(DT_holidays, date)

# ----------------------------------------------------
#####################################################################
# 2) Visual defaults — Accessible theme & palette
# ----------------------------------------------------
okabe_ito <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
cb_theme <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title       = element_text(face = "bold"),
      plot.title       = element_text(face = "bold", hjust = 0, margin = margin(b = 8)),
      legend.position  = "bottom"
    )
}
theme_set(cb_theme())

# -----------------------------
#####################################################################
# 3) UI 
# -----------------------------
ui <- fluidPage(
  titlePanel("Sales Analytics Dashboard — Interactive"),
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput(
        "dr", label = "Date Range ",
        start = min(DT_data$date, na.rm = TRUE),
        end   = max(DT_data$date, na.rm = TRUE),
        min   = min(DT_data$date, na.rm = TRUE),
        max   = max(DT_data$date, na.rm = TRUE)
      ),
      selectInput(
        "state", label = "State ",
        choices = c("All", sort(unique(na.omit(DT_stores$state)))) , selected = "All"
      ),
      selectInput(
        "stype", label = "Store Type ",
        choices = c("All", sort(unique(na.omit(DT_stores$store_type)))) , selected = "All"
      ),
      selectizeInput(
        "family", label = "Item Family ",
        choices = c("All", sort(unique(na.omit(DT_items$family)))) , selected = "All"
      ),
      checkboxInput("only_weekend", label = "Only Weekends ", value = FALSE),
      checkboxInput("only_holiday", label = "Only Holidays ", value = FALSE)
    ),
    mainPanel(
      
      fluidRow(
        column(3, uiOutput("kpi_total")),
        column(3, uiOutput("kpi_top_store")),
        column(3, uiOutput("kpi_top_item")),
        column(3, uiOutput("kpi_neg_rows"))
      ),
      tags$hr(),
      tabsetPanel(
        tabPanel("Q1 — Top Store",
                 downloadButton("dl_q1", "Download CSV"),
                 plotOutput("q1_plot"), DTOutput("q1_table")),
        tabPanel("Q2 — West-Bengal Family",
                 downloadButton("dl_q2", "Download CSV"),
                 plotOutput("q2_plot"), DTOutput("q2_table")),
        tabPanel("Q3 — Oil vs Sales",
                 plotOutput("q3_ts"), plotOutput("q3_sc"), DTOutput("q3_stats")),
        tabPanel("Q4 — Weekdays & Holidays",
                 plotOutput("q4_wd"), plotOutput("q4_we"), plotOutput("q4_hd")),
        tabPanel("Q5 — Negative Sales",
                 plotOutput("q5_hist"), plotOutput("q5_topstores"), plotOutput("q5_ts")),
        tabPanel("Q6 — Store Types", plotOutput("q6_plot"), DTOutput("q6_table")),
        tabPanel("Q7 — States excl. WB", plotOutput("q7_plot"), DTOutput("q7_table")),
        tabPanel("Q8 — Perishability", plotOutput("q8_plot"), DTOutput("q8_table")),
        tabPanel("Q9 — Top Items", plotOutput("q9_plot"), DTOutput("q9_table"))
      )
    )
  ),
 
  tags$style(HTML("
    .kpi {background:#f8f9fa;border:1px solid #e5e7eb;border-radius:12px;padding:14px;margin-bottom:12px;}
    .kpi h4{margin:0 0 4px 0;font-weight:600;font-size:14px;color:#374151;}
    .kpi .value{font-size:22px;font-weight:700;color:#111827;}
  "))
)

# ---------------------------------------
#####################################################################
# 4) Server 
# ---------------------------------------

server <- function(input, output, session) {
  
  # --------- Main filter ----------
  #####################################################################
  #4.1
  filtered_sales <- reactive({
    
    start_date <- (input$dr %||% c(min(DT_data$date, na.rm = TRUE), max(DT_data$date, na.rm = TRUE)))[1]
    end_date   <- (input$dr %||% c(min(DT_data$date, na.rm = TRUE), max(DT_data$date, na.rm = TRUE)))[2]
    
    dt <- DT_data[date >= start_date & date <= end_date]
    
    if (!is.null(input$state) && input$state != "All") {
      dt <- merge(dt, DT_stores[state == input$state, .(store_nbr)], by = "store_nbr")
    }
    if (!is.null(input$stype) && input$stype != "All") {
      dt <- merge(dt, DT_stores[store_type == input$stype, .(store_nbr)], by = "store_nbr")
    }
    if (!is.null(input$family) && input$family != "All") {
      dt <- merge(dt, DT_items[family == input$family, .(item_nbr)], by = "item_nbr")
    }
    if (isTRUE(input$only_weekend)) {
      dt <- dt[wday(date, week_start = 1) %in% c(6, 7)]
    }
    if (isTRUE(input$only_holiday)) {
      dt <- merge(dt, unique(DT_holidays[, .(date)]), by = "date")
    }
    dt
  }) %>% bindCache(input$dr, input$state, input$stype, input$family, input$only_weekend, input$only_holiday)
  
  
  #####################################################################
  #4.2
  # --------- KPI's ----------
  output$kpi_total <- renderUI({
    dt <- filtered_sales()
    div(class = "kpi",
        h4("Total Sales "),
        div(class = "value", scales::comma(sum(dt$sales, na.rm = TRUE)))
    )
  })
  output$kpi_top_store <- renderUI({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = store_nbr][order(-total_sales)]
    top <- head(dt, 1)
    label <- if (nrow(top) > 0) paste0("Store ", top$store_nbr) else "—"
    div(class = "kpi", h4("Top Store "), div(class = "value", label))
  })
  output$kpi_top_item <- renderUI({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = item_nbr][order(-total_sales)]
    top <- head(dt, 1)
    label <- if (nrow(top) > 0) paste0("Item ", top$item_nbr) else "—"
    div(class = "kpi", h4("Top Item "), div(class = "value", label))
  })
  output$kpi_neg_rows <- renderUI({
    dt <- filtered_sales()
    div(class = "kpi", h4("Negative Rows / Negatif Satır"),
        div(class = "value", sum(dt$sales < 0, na.rm = TRUE)))
  })
  
  #####################################################################
  #4.3
  # ---------------- Q1 -----------------
  output$q1_plot <- renderPlot({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = store_nbr][order(-total_sales)]
    dt <- merge(dt, DT_stores[, .(store_nbr, city, state, store_type)], by = "store_nbr", all.x = TRUE)
    dt <- dt[order(-total_sales)]  # ensure descending order
    validate(need(nrow(dt) > 0, "No data "))
    ggplot(dt[1:min(.N, 15)], aes(x = reorder(paste0(store_nbr, " — ", city), total_sales), y = total_sales)) +
      geom_col(fill = okabe_ito[3]) + coord_flip() +
      labs(title = "Top Stores by Total Sales ",
           x = "Store — City ", y = "Total Sales ") +
      scale_y_continuous(labels = label_comma())
  })
  output$q1_table <- renderDT({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = store_nbr][order(-total_sales)]
    dt <- merge(dt, DT_stores[, .(store_nbr, city, state, store_type)], by = "store_nbr", all.x = TRUE)
    dt <- dt[order(-total_sales)]
    datatable(dt, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$dl_q1 <- downloadHandler(
    filename = function() paste0("Q1_top_stores_", Sys.Date(), ".csv"),
    content  = function(file) {
      dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = store_nbr][order(-total_sales)]
      dt <- merge(dt, DT_stores[, .(store_nbr, city, state, store_type)], by = "store_nbr", all.x = TRUE)
      data.table::fwrite(dt, file)
    }
  )
  
  #####################################################################
  #4.4
  # ---------------- Q2 -----------------
  # West-Bengal families 
  
  output$q2_plot <- renderPlot({
    
    df_data   <- as.data.frame(DT_data)
    df_items  <- as.data.frame(DT_items)
    df_stores <- as.data.frame(DT_stores)
    
    
    df_temp <- df_data
    
    df_temp$unit_sales_std <- dplyr::coalesce(df_temp$sales, df_temp$unit_sales)
    
    df_temp <- dplyr::left_join(df_temp, df_items, by = "item_nbr")
    
    df_temp <- dplyr::left_join(df_temp, df_stores, by = "store_nbr")
    
    df_temp <- dplyr::filter(df_temp, state %in% c("West Bengal", "West-Bengal", "WB"))
    
    wb_families <- dplyr::summarise(
      dplyr::group_by(df_temp, family),
      total_sales = sum(unit_sales_std, na.rm = TRUE),
      .groups = "drop"
    )
    
    wb_families <- dplyr::arrange(wb_families, dplyr::desc(total_sales))
    
    
    #     wb_families <- df_data %>%
    #     mutate(
    #      unit_sales_std = dplyr::coalesce(.data$sales, .data$unit_sales)
    #   ) %>%
    #  left_join(df_items,  by = "item_nbr") %>%
    # left_join(df_stores, by = "store_nbr") %>%
    #filter(state %in% c("West Bengal", "West-Bengal", "WB")) %>%
    #group_by(family) %>%
    #summarise(total_sales = sum(unit_sales_std, na.rm = TRUE), .groups = "drop") %>%
    #arrange(desc(total_sales))
    
    validate(need(nrow(wb_families) > 0, "No data "))
    
    ggplot(wb_families, aes(x = reorder(family, total_sales), y = total_sales)) +
      geom_col(fill = okabe_ito[5]) +
      coord_flip() +
      labs(
        title = "West Bengal — Top Families by Sales",
        x = "Family ",
        y = "Total Sales "
      ) +
      scale_y_continuous(labels = scales::label_comma()) +
      cb_theme()
  })
  
  output$q2_table <- renderDT({
    
    req(DT_data, DT_items, DT_stores)
    df_data   <- as.data.frame(if (shiny::is.reactive(DT_data)) DT_data() else DT_data,   stringsAsFactors = FALSE)
    df_items  <- as.data.frame(if (shiny::is.reactive(DT_items)) DT_items() else DT_items, stringsAsFactors = FALSE)
    df_stores <- as.data.frame(if (shiny::is.reactive(DT_stores)) DT_stores() else DT_stores, stringsAsFactors = FALSE)
    
  
    needed_data_keys <- c("item_nbr", "store_nbr")
    missing_data_keys <- setdiff(needed_data_keys, names(df_data))
    validate(need(length(missing_data_keys) == 0,
                  paste("missing colomn(s) of df_data:", paste(missing_data_keys, collapse = ", "))))
    
    validate(need("item_nbr" %in% names(df_items),  "There is no 'item_nbr' column in the df_items dataframe"))
    validate(need("store_nbr" %in% names(df_stores), "There is no 'store_nbr' column in the df_stores dataframe"))
    
 
    state_candidates <- c("state","State","STATE","state_name","region","province")
    hit <- state_candidates[state_candidates %in% names(df_stores)]
    validate(need(length(hit) > 0,
                  "There is no column like 'state' (or state_name / region / province) in the df_stores dataframe."))
    
    if (hit[1] != "state") {
      df_stores <- dplyr::rename(df_stores, state = dplyr::all_of(hit[1]))
    }
    
    
    sales_vec      <- if ("sales" %in% names(df_data)) df_data[["sales"]] else rep(NA_real_, nrow(df_data))
    unit_sales_vec <- if ("unit_sales" %in% names(df_data)) df_data[["unit_sales"]] else rep(NA_real_, nrow(df_data))
    
    wb_families <- df_data %>%
      dplyr::mutate(unit_sales_std = dplyr::coalesce(sales_vec, unit_sales_vec)) %>%
      dplyr::left_join(df_items,  by = "item_nbr") %>%
      dplyr::left_join(df_stores, by = "store_nbr") %>%
      
      dplyr::mutate(state_norm = dplyr::case_when(
        state %in% c("West Bengal", "West-Bengal", "WB") ~ "West Bengal",
        TRUE ~ state
      )) %>%
      dplyr::filter(state_norm == "West Bengal") %>%
      dplyr::group_by(family) %>%
      dplyr::summarise(total_sales = sum(unit_sales_std, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total_sales))
    
    validate(need(nrow(wb_families) > 0, "No data "))
    
    DT::datatable(wb_families, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  output$dl_q2 <- downloadHandler(
    filename = function() paste0("Q2_west_bengal_families_", Sys.Date(), ".csv"),
    content  = function(file) {
      wb <- DT_stores[state == "West-Bengal", .(store_nbr)]
      dt <- DT_data[wb, on = "store_nbr", nomatch = 0L]
      dt <- dt[date >= (input$dr %||% c(min(DT_data$date), max(DT_data$date)))[1] &
                 date <= (input$dr %||% c(min(DT_data$date), max(DT_data$date)))[2]]
      dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
      fam <- dt[, .(total_sales = sum(sales, na.rm = TRUE)), by = family][order(-total_sales)]
      data.table::fwrite(fam, file)
    }
  )
  
  #####################################################################
  #4.5
  # ---------------- Q3 -----------------
  output$q3_ts <- renderPlot({
    dt  <- filtered_sales()[, .(daily_sales = sum(sales, na.rm = TRUE)), by = date][order(date)]
    oil <- DT_oil[date >= input$dr[1] & date <= input$dr[2]]
    ser <- merge(dt, oil[, .(date, oil_price)], by = "date", all.x = TRUE)
    ser[, sales_z := as.numeric(scale(daily_sales))]
    ser[, oil_z   := as.numeric(scale(oil_price))]
    ggplot(ser, aes(x = date)) +
      geom_line(aes(y = sales_z), linewidth = 0.7, color = okabe_ito[3]) +
      geom_line(aes(y = oil_z),   linewidth = 0.7, linetype = "dashed", color = okabe_ito[6]) +
      labs(title = "Daily Sales (solid) vs Oil (dashed) — Z-scores",
           x = "Date ", y = "Z-score")
  })
  
  output$q3_sc <- renderPlot({
    dt  <- filtered_sales()[, .(daily_sales = sum(sales, na.rm = TRUE)), by = date][order(date)]
    oil <- DT_oil[date >= input$dr[1] & date <= input$dr[2]]
    ser <- merge(dt, oil[, .(date, oil_price)], by = "date", all.x = TRUE)
    ggplot(ser, aes(x = oil_price, y = daily_sales)) +
      geom_point(alpha = 0.5, color = okabe_ito[4]) +
      geom_smooth(method = "lm", se = TRUE, color = okabe_ito[1]) +
      labs(title = "Oil Price vs Daily Sales",
           x = "Oil Price ", y = "Daily Sales ")
  })
  
  output$q3_stats <- renderDT({
    dt  <- filtered_sales()[, .(daily_sales = sum(sales, na.rm = TRUE)), by = date]
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
  
  #####################################################################
  #4.6
  # ---------------- Q4 -----------------
  output$q4_wd <- renderPlot({
    sd <- filtered_sales()[, .(sales = sum(sales, na.rm = TRUE)), by = .(date, store_nbr)]
    sd[, weekday := wday(date, label = TRUE, abbr = TRUE, week_start = 1)]
    ag <- sd[, .(avg_sales = mean(sales), med_sales = median(sales)), by = weekday]
    ggplot(ag, aes(x = weekday, y = avg_sales)) +
      geom_col(fill = okabe_ito[2]) +
      labs(title = "Average Sales by Weekday ",
           x = "Weekday ", y = "Average Sales ")
  })
  output$q4_we <- renderPlot({
    sd <- filtered_sales()[, .(sales = sum(sales, na.rm = TRUE)), by = .(date, store_nbr)]
    sd[, is_weekend := wday(date, week_start = 1) %in% c(6,7)]
    ggplot(sd, aes(x = is_weekend, y = sales)) +
      geom_boxplot(outlier.alpha = 0.2, fill = okabe_ito[7]) +
      labs(title = "Weekend vs Weekday ",
           x = "Is Weekend? ", y = "Sales ")
  })
  output$q4_hd <- renderPlot({
    sd <- filtered_sales()[, .(sales = sum(sales, na.rm = TRUE)), by = .(date, store_nbr)]
    hdays <- unique(DT_holidays[, .(date)])
    sd[, is_holiday := date %in% hdays$date]
    ggplot(sd, aes(x = is_holiday, y = sales)) +
      geom_boxplot(outlier.alpha = 0.2, fill = okabe_ito[5]) +
      labs(title = "Holiday vs Non-Holiday ",
           x = "Is Holiday? ", y = "Sales ")
  })
  
  #####################################################################
  #4.7
  # ---------------- Q5 -----------------
  output$q5_hist <- renderPlot({
    neg <- filtered_sales()[sales < 0]
    if (nrow(neg) == 0) return(NULL)
    neg[, neg_mag := abs(sales)]
    ggplot(neg, aes(x = neg_mag)) +
      geom_histogram(bins = 50, fill = okabe_ito[6]) +
      labs(title = "Negative Sales Magnitudes ",
           x = "|sales|", y = "Count ")
  })
  output$q5_topstores <- renderPlot({
    neg <- filtered_sales()[sales < 0]
    if (nrow(neg) == 0) return(NULL)
    agg <- neg[, .N, by = store_nbr][order(-N)][1:min(15, .N)]
    ggplot(agg, aes(x = reorder(factor(store_nbr), N), y = N)) +
      geom_col(fill = okabe_ito[4]) + coord_flip() +
      labs(title = "Top Stores by Negative Rows ",
           x = "Store Nbr ", y = "# Negative Rows ")
  })
  output$q5_ts <- renderPlot({
    neg <- filtered_sales()[sales < 0]
    if (nrow(neg) == 0) return(NULL)
    ts <- neg[, .N, by = date][order(date)]
    ggplot(ts, aes(x = date, y = N)) +
      geom_line(linewidth = 0.9, color = okabe_ito[3]) +
      labs(title = "Negative Rows Over Time ",
           x = "Date", y = "Count")
  })
  
  #####################################################################
  #4.8
  # ---------------- Q6 -----------------
  output$q6_plot <- renderPlot({
    st <- DT_stores
    if (input$state != "All") st <- st[state == input$state]
    gg <- st[, .N, by = store_type][order(-N)]
    ggplot(gg, aes(x = reorder(store_type, N), y = N)) +
      geom_col(fill = okabe_ito[8]) + coord_flip() +
      labs(title = "Store Type Frequency",
           x = "Store Type", y = "Count")
  })
  output$q6_table <- renderDT({
    st <- DT_stores
    if (input$state != "All") st <- st[state == input$state]
    gg <- st[, .N, by = store_type][order(-N)]
    datatable(gg, options = list(pageLength = 10), rownames = FALSE)
  })
  
  #####################################################################
  #4.9
  # ---------------- Q7 -----------------
  
  
  # st <- DT_stores[state != "West-Bengal"]
  
  output$q7_plot <- renderPlot({
    st <- DT_stores[!state %in% c("West-Bengal", "West Bengal", "west bengal", "WEST BENGAL")]
    gg <- st[, .N, by = state][order(-N)][1:min(15, .N)]
    ggplot(gg, aes(x = reorder(state, N), y = N)) +
      geom_col(fill = okabe_ito[2]) + coord_flip() +
      labs(title = "States by #Stores (excl. West-Bengal)",
           x = "State ", y = "# Stores")
  })
  output$q7_table <- renderDT({
    st <- DT_stores[!state %in% c("West-Bengal", "West Bengal", "west bengal", "WEST BENGAL")]
    gg <- st[, .N, by = state][order(-N)]
    datatable(gg, options = list(pageLength = 10), rownames = FALSE)
  })
  
  #####################################################################
  #4.10
  # ---------------- Q8 -----------------
  output$q8_plot <- renderPlot({
    it <- copy(DT_items)
    if (!"perishable" %in% names(it)) it[, perishable := NA_integer_]
    it[, perishable := as.integer(perishable)]
    it[, perishable_label := ifelse(perishable == 1, "Perishable", "Non-Perishable")]
    cc <- it[, .N, by = perishable_label][order(-N)]
    ggplot(cc[!is.na(perishable_label)], aes(x = perishable_label, y = N)) +
      geom_col(fill = okabe_ito[5]) +
      labs(title = "Items by Perishability",
           x = "Category", y = "Count")
  })
  output$q8_table <- renderDT({
    it <- copy(DT_items)
    if (!"perishable" %in% names(it)) it[, perishable := NA_integer_]
    it[, perishable := as.integer(perishable)]
    it[, perishable_label := ifelse(perishable == 1, "Perishable", "Non-Perishable")]
    cc <- it[, .N, by = perishable_label][order(-N)]
    datatable(cc, options = list(pageLength = 10), rownames = FALSE)
  })
  
  #####################################################################
  #4.11
  # ---------------- Q9 -----------------
  output$q9_plot <- renderPlot({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = item_nbr][order(-total_sales)]
    dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
    topN <- dt[1:min(20, .N)]
    ggplot(topN, aes(x = reorder(paste0(item_nbr, ifelse(!is.na(family), paste0(" — ", family), "")), total_sales), y = total_sales)) +
      geom_col(fill = okabe_ito[6]) + coord_flip() +
      labs(title = "Top Items by Total Sales ",
           x = "Item — Family ", y = "Total Sales ") +
      scale_y_continuous(labels = label_comma())
  })
  output$q9_table <- renderDT({
    dt <- filtered_sales()[, .(total_sales = sum(sales, na.rm = TRUE)), by = item_nbr][order(-total_sales)]
    dt <- merge(dt, DT_items[, .(item_nbr, family)], by = "item_nbr", all.x = TRUE)
    datatable(dt, options = list(pageLength = 10), rownames = FALSE)
  })
}

# -----------------------------
#####################################################################
#5
# 5) Run app 
# -----------------------------
shinyApp(ui, server)
