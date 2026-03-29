# Load necessary libraries
library(shiny)
library(tidyverse)
library(quantmod)
library(bslib)
library(DT)

#------------------------------------------------------------------------------
# GLOBAL DATA SETUP & FUNCTIONS (Runs ONCE when the app starts)
#------------------------------------------------------------------------------
# Define our 16 U.S. Stocks
stock_choices <- sort(c(
  "MSFT",
  "AAPL",
  "GOOG",
  "NVDA",
  "JPM",
  "GS",
  "V",
  "JNJ",
  "UNH",
  "WMT",
  "PG",
  "XOM",
  "CAT",
  "TRV",
  "NEE",
  "AIG"
))

# Set start date to match your desired study period
start_date_global <- as.Date("2016-01-01")
# 1. as.Date("2026-02-27")
# 2. Sys.Date()
end_date_global <- Sys.Date()

# Safely download data into a list
price_list <- list()
for (ticker in stock_choices) {
  tryCatch(
    {
      temp_data <- suppressWarnings(
        getSymbols(
          ticker,
          src = "yahoo",
          from = start_date_global,
          to = end_date_global,
          auto.assign = FALSE
        )
      )
      # using Cl() to match Excel's STOCKHISTORY
      price_list[[ticker]] <- Cl(temp_data)
    },
    error = function(e) {
      message(paste("Skipping", ticker, "- could not download data."))
    }
  )
}

# Merge and clean master dataset
master_prices_xts <- do.call(merge, price_list)
# CHANGE THIS LINE TO CLEAR THE .Close SUFFIX
colnames(master_prices_xts) <- gsub("\\.Close", "", colnames(master_prices_xts))

master_prices <- data.frame(
  Date = index(master_prices_xts),
  coredata(master_prices_xts)
)
stock_choices <- intersect(stock_choices, colnames(master_prices))

# --- Custom Tree Plotting Function ---
plot_tree <- function(
  V,
  S,
  title,
  N,
  show_early = NULL,
  log_scale = FALSE,
  show_xlab = TRUE,
  show_x_ticks = TRUE
) {
  yvals <- S[S > 0]

  if (log_scale) {
    y_coord <- function(p) log(p)
    y_all <- log(yvals)
    y_label <- "Stock price - log scale ($)"
  } else {
    y_coord <- function(p) p
    y_all <- yvals
    y_label <- "Stock price ($)"
  }

  plot(
    NULL,
    xlim = c(-0.5, N + 1.2),
    ylim = c(min(y_all) * 0.97, max(y_all) * 1.02),
    xlab = if (show_xlab) "Time step (i)" else "",
    ylab = y_label,
    main = title,
    yaxt = if (log_scale) "n" else "s",
    xaxt = if (show_x_ticks) "s" else "n"
  )

  if (log_scale) {
    pretty_vals <- pretty(yvals, n = 8)
    pretty_vals <- pretty_vals[
      pretty_vals > 0 &
        pretty_vals >= min(yvals) * 0.95 &
        pretty_vals <= max(yvals) * 1.05
    ]
    axis(
      2,
      at = log(pretty_vals),
      labels = sprintf("%.0f", pretty_vals),
      las = 1,
      cex.axis = 0.75,
      tcl = -0.3
    ) # text size control
  }

  for (i in 0:N) {
    for (j in 0:i) {
      price <- S[j + 1, i + 1]
      val <- V[j + 1, i + 1]
      yp <- y_coord(price)

      if (i < N) {
        lines(
          c(i, i + 1),
          c(yp, y_coord(S[j + 2, i + 2])),
          col = "grey70",
          lwd = 0.6
        )
        lines(
          c(i, i + 1),
          c(yp, y_coord(S[j + 1, i + 2])),
          col = "grey70",
          lwd = 0.6
        )
      }

      if (!is.null(show_early) && show_early[j + 1, i + 1]) {
        col <- "red"
        cex_pt <- 1.2
      } else if (val > 0) {
        col <- "navy"
        cex_pt <- 0.8
      } else {
        col <- "grey50"
        cex_pt <- 0.5
      }
      points(i, yp, pch = 16, cex = cex_pt, col = col)

      lbl <- sprintf("%.2f", val)
      if (!is.null(show_early) && show_early[j + 1, i + 1]) {
        lbl <- paste0(lbl, " (E)")
      }
      text(
        i,
        yp,
        lbl,
        cex = 0.52,
        pos = 3,
        offset = 0.35, # text size control
        col = ifelse(
          !is.null(show_early) && show_early[j + 1, i + 1],
          "red",
          "black"
        )
      )
    }
  }

  if (!is.null(show_early)) {
    leg_text <- c("Early exercise (E)", "Continuation", "Zero option value")
    leg_col <- c("red", "navy", "grey50")
    leg_pt_cex <- c(1.2, 0.8, 0.5)
  } else {
    leg_text <- c("Positive option value", "Zero option value")
    leg_col <- c("navy", "grey50")
    leg_pt_cex <- c(0.8, 0.5)
  }

  legend(
    "topleft",
    legend = leg_text,
    col = leg_col,
    pch = 16,
    pt.cex = leg_pt_cex,
    cex = 0.8,
    bg = "white"
  ) # text size control
}

#------------------------------------------------------------------------------
# UI DEFINITION
#------------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "united"),
  titlePanel("CRR Binomial Option Pricing Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuration"),

      selectizeInput(
        "selected_stock",
        "Select underlying asset:",
        choices = stock_choices,
        selected = "MSFT",
        multiple = FALSE
      ),

      dateRangeInput(
        "date_range",
        "Historical volatility period:",
        start = "2016-03-01",
        end = "2026-02-27",
        min = start_date_global,
        max = Sys.Date()
      ),

      hr(),
      h4("Option Parameters"),
      numericInput(
        "time_steps",
        "Time steps (N):",
        value = 10,
        min = 1,
        max = 50
      ),
      numericInput(
        "t_mat",
        "Maturity (Years):",
        value = 1,
        min = 0.1,
        step = 0.1
      ),
      numericInput(
        "risk_free",
        "Risk-free rate (r):",
        value = 0.038,
        step = 0.001
      ),
      numericInput(
        "strike_mult",
        "Strike Multiplier (% of S0):",
        value = 1.05,
        step = 0.01
      )
    ),

    mainPanel(
      # Top Header / Summary Stats Panel
      uiOutput("summary_boxes"),
      br(),

      tabsetPanel(
        id = "tabs",

        # TAB 1: Data
        tabPanel(
          "1. Underlying Asset Data",
          br(),
          h4("Asset Log Returns"),
          DTOutput("log_returns_tbl"),
          downloadButton(
            "download_data",
            "Download App Data as CSV",
            class = "btn-primary"
          ),
          br(),
          br(),
        ),

        # TAB 2: Stock Tree
        tabPanel(
          "2. Stock Price Tree",
          br(),
          h4("CRR Binomial Stock Price Tree"),
          plotOutput("plot_stock_tree", height = "600px")
        ),

        # TAB 3: European Option
        tabPanel(
          "3. European Put",
          br(),
          h4("European Vanilla Put Option Value"),
          uiOutput("euro_summary"),
          br(),
          plotOutput("plot_euro_tree", height = "800px")
        ),

        # TAB 4: Bermudan
        tabPanel(
          "4. Bermudan Put",
          br(),
          h4("Bermudan Put Option Value"),
          uiOutput("berm_summary"),
          br(),
          plotOutput("plot_berm_tree", height = "800px")
        )
      )
    )
  )
)

#------------------------------------------------------------------------------
# SERVER LOGIC
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  # 1. Reactive Data Filter
  stock_data <- reactive({
    req(input$selected_stock)

    df <- master_prices %>%
      select(Date, close = all_of(input$selected_stock)) %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
      drop_na()

    df <- df %>% mutate(log_ret = c(NA, diff(log(close))))
    df
  })

  # 2. Reactive Tree Parameters
  tree_params <- reactive({
    df <- stock_data()
    req(nrow(df) > 10)

    log_rets <- df$log_ret[-1] # remove NA
    S0 <- tail(df$close, 1)

    var_daily <- var(log_rets)
    std_daily <- sd(log_rets)
    sigma <- std_daily * sqrt(252)

    N <- input$time_steps
    T_mat <- input$t_mat
    dt <- T_mat / N

    u <- exp(sigma * sqrt(dt))
    d <- 1 / u

    r <- input$risk_free
    K <- input$strike_mult * S0
    q <- (exp(r * dt) - d) / (u - d)
    disc <- exp(-r * dt)

    list(
      S0 = S0,
      var_daily = var_daily,
      std_daily = std_daily,
      sigma = sigma,
      N = N,
      dt = dt,
      u = u,
      d = d,
      r = r,
      K = K,
      q = q,
      disc = disc
    )
  })

  # 3. Reactive Price & Option Trees
  calc_trees <- reactive({
    p <- tree_params()
    N <- p$N

    # Stock Tree
    S <- matrix(0, N + 1, N + 1)
    for (i in 0:N) {
      for (j in 0:i) {
        S[j + 1, i + 1] <- p$S0 * p$u^j * p$d^(i - j)
      }
    }

    # European Put Tree
    V_eu <- matrix(0, N + 1, N + 1)
    for (j in 0:N) {
      V_eu[j + 1, N + 1] <- max(p$K - S[j + 1, N + 1], 0)
    }
    for (i in (N - 1):0) {
      for (j in 0:i) {
        V_eu[j + 1, i + 1] <- p$disc *
          (p$q * V_eu[j + 2, i + 2] + (1 - p$q) * V_eu[j + 1, i + 2])
      }
    }

    # Bermudan Put Tree
    V_bm <- matrix(0, N + 1, N + 1)
    early <- matrix(FALSE, N + 1, N + 1)
    for (j in 0:N) {
      V_bm[j + 1, N + 1] <- max(p$K - S[j + 1, N + 1], 0)
    }
    for (i in (N - 1):0) {
      for (j in 0:i) {
        cont <- p$disc *
          (p$q * V_bm[j + 2, i + 2] + (1 - p$q) * V_bm[j + 1, i + 2])
        exer <- p$K - S[j + 1, i + 1]
        V_bm[j + 1, i + 1] <- max(cont, exer)
        early[j + 1, i + 1] <- (exer > cont) & (exer > 0)
      }
    }

    list(S = S, V_eu = V_eu, V_bm = V_bm, early = early)
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste(
        "shiny_app_data_",
        input$selected_stock,
        "_",
        Sys.Date(),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      # Fetch the raw, unrounded data for accurate comparison
      df_export <- stock_data()
      write.csv(df_export, file, row.names = FALSE)
    }
  )

  # --- SUMMARY UI AT TOP ---
  output$summary_boxes <- renderUI({
    p <- tree_params()
    trees <- calc_trees()

    v0_eu <- trees$V_eu[1, 1]
    v0_bm <- trees$V_bm[1, 1]
    prem <- v0_bm - v0_eu

    arrow_html <- ifelse(
      prem > 0.0001,
      "<span style='color:green; font-weight:bold;'>&#9650;</span>",
      ""
    )

    tagList(
      # One unified box for parameters
      card(
        card_header(
          "Model Parameters & Volatility Metrics",
          class = "bg-primary text-white"
        ),
        card_body(
          HTML(sprintf(
            "<div style='font-size: 1.1em;'>
             <b>S0:</b> $%.2f &nbsp;|&nbsp; <b>Strike (K):</b> $%.2f &nbsp;|&nbsp; <b>Var:</b> %.6f &nbsp;|&nbsp; <b>SD:</b> %.6f &nbsp;|&nbsp; <b>Ann. Vol (\u03c3):</b> %.6f <br>
             <b>dt:</b> %.4f &nbsp;|&nbsp; <b>N:</b> %d &nbsp;|&nbsp; <b>u:</b> %.6f &nbsp;|&nbsp; <b>d:</b> %.6f &nbsp;|&nbsp; <b>q:</b> %.6f &nbsp;|&nbsp; <b>r:</b> %.1f%%
             </div>",
            p$S0,
            p$K,
            p$var_daily,
            p$std_daily,
            p$sigma,
            p$dt,
            p$N,
            p$u,
            p$d,
            p$q,
            p$r * 100
          ))
        )
      ),
      # Prices boxes below
      layout_column_wrap(
        width = 1 / 3,
        value_box(
          title = "European Put (V0)",
          value = sprintf("$%.4f", v0_eu),
          theme = "success"
        ),
        value_box(
          title = "Bermudan Put (V0)",
          value = sprintf("$%.4f", v0_bm),
          theme = "success"
        ),
        value_box(
          title = "Early Exercise Premium",
          value = HTML(sprintf("$%.4f %s", prem, arrow_html)),
          theme = "warning"
        )
      )
    )
  })

  # --- TAB 1 OUTPUTS ---
  output$log_returns_tbl <- renderDT({
    df <- stock_data() %>% drop_na() %>% arrange(desc(Date))

    # Format and rename columns
    df <- df %>%
      mutate(
        Close = round(close, 2),
        `Daily log returns` = round(log_ret, 4)
      ) %>%
      select(Date, Close, `Daily log returns`)

    datatable(df, options = list(pageLength = 10, dom = 'tp'), rownames = FALSE)
  })

  # --- TAB 2 OUTPUTS ---
  output$plot_stock_tree <- renderPlot({
    p <- tree_params()
    trees <- calc_trees()
    S <- trees$S
    N <- p$N

    par(mar = c(4, 4, 3, 1))
    plot(
      NULL,
      xlim = c(-0.5, N + 1.2),
      ylim = c(min(S[S > 0]) * 0.93, max(S) * 1.04),
      xlab = "Time step (i)",
      ylab = "Stock price ($)",
      main = sprintf(
        "CRR Binomial Stock Price Tree (%s, N = %d)",
        input$selected_stock,
        N
      )
    )

    for (i in 0:N) {
      for (j in 0:i) {
        price <- S[j + 1, i + 1]
        if (i < N) {
          lines(
            c(i, i + 1),
            c(price, S[j + 2, i + 2]),
            col = "steelblue",
            lwd = 0.7
          )
          lines(
            c(i, i + 1),
            c(price, S[j + 1, i + 2]),
            col = "steelblue",
            lwd = 0.7
          )
        }
        points(i, price, pch = 16, cex = 0.6, col = "navy")
        text(
          i,
          price,
          sprintf("%.2f", price),
          cex = 0.52,
          pos = 3,
          offset = 0.35
        ) # text size control
      }
    }
  })

  # --- TAB 3 OUTPUTS ---
  output$euro_summary <- renderUI({
    p <- tree_params()
    trees <- calc_trees()
    HTML(sprintf(
      "<b>S0:</b> $%.2f | <b>Strike (K):</b> $%.2f | <b>Option Value (V0):</b> $%.4f",
      p$S0,
      p$K,
      trees$V_eu[1, 1]
    ))
  })

  output$plot_euro_tree <- renderPlot({
    p <- tree_params()
    trees <- calc_trees()

    layout(matrix(1:2), heights = c(1.8, 3.2))
    par(oma = c(0, 0, 0, 0))

    # Top plot - log scale
    par(mar = c(0, 4, 3, 1))
    plot_tree(
      trees$V_eu,
      trees$S,
      sprintf(
        "European Put (K=%.2f, r=%.1f%%)  |  V0 = %.4f",
        p$K,
        p$r * 100,
        trees$V_eu[1, 1]
      ),
      N = p$N,
      log_scale = TRUE,
      show_xlab = FALSE,
      show_x_ticks = FALSE
    )

    # Bottom plot - linear scale
    par(mar = c(4, 4, 0.5, 1), mgp = c(2, 1, 0))
    plot_tree(trees$V_eu, trees$S, title = "", N = p$N, log_scale = FALSE)
  })

  # --- TAB 4 OUTPUTS ---
  output$berm_summary <- renderUI({
    p <- tree_params()
    trees <- calc_trees()
    prem <- trees$V_bm[1, 1] - trees$V_eu[1, 1]
    HTML(sprintf(
      "<b>S0:</b> $%.2f | <b>Strike (K):</b> $%.2f | <b>Option Value (V0):</b> $%.4f | <b>Premium:</b> $%.4f",
      p$S0,
      p$K,
      trees$V_bm[1, 1],
      prem
    ))
  })

  output$plot_berm_tree <- renderPlot({
    p <- tree_params()
    trees <- calc_trees()

    layout(matrix(1:2), heights = c(1.8, 3.2))
    par(oma = c(0, 0, 0, 0))

    # Top plot - log scale
    par(mar = c(0, 4, 3, 1))
    plot_tree(
      trees$V_bm,
      trees$S,
      sprintf(
        "Bermudan Put (K=%.2f, r=%.1f%%)  |  V0 = %.4f",
        p$K,
        p$r * 100,
        trees$V_bm[1, 1]
      ),
      N = p$N,
      log_scale = TRUE,
      show_xlab = FALSE,
      show_x_ticks = FALSE,
      show_early = trees$early
    )

    # Bottom plot - linear scale
    par(mar = c(4, 4, 0.5, 1), mgp = c(2, 1, 0))
    plot_tree(
      trees$V_bm,
      trees$S,
      title = "",
      N = p$N,
      log_scale = FALSE,
      show_early = trees$early
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
