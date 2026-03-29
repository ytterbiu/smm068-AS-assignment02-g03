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
stock_choices_default <- sort(c(
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

start_date_global <- as.Date("2016-01-01")

# Encapsulate the download logic so it can be called both at startup and on
# refresh (supports re-download with progress indicator).
download_all_stocks <- function(tickers, from, to) {
  price_list <- list()
  for (ticker in tickers) {
    tryCatch(
      {
        temp_data <- suppressWarnings(
          getSymbols(
            ticker,
            src = "yahoo",
            from = from,
            to = to,
            auto.assign = FALSE
          )
        )
        price_list[[ticker]] <- Cl(temp_data)
      },
      error = function(e) {
        message(paste("Skipping", ticker, "- could not download data."))
      }
    )
  }

  if (length(price_list) == 0) {
    return(NULL)
  }

  merged <- do.call(merge, price_list)
  colnames(merged) <- gsub("\\.Close", "", colnames(merged))
  data.frame(Date = index(merged), coredata(merged))
}

# Initial download at startup
last_refresh_time <- Sys.time()
master_prices <- download_all_stocks(
  stock_choices_default,
  start_date_global,
  Sys.Date()
)
# intersect happens before ui is created, so selectizeInput is always
# initialised with only tickers that actually downloaded successfully.
stock_choices <- intersect(stock_choices_default, colnames(master_prices))


# --- Custom Tree Plotting Function ---
# Node convention (0-indexed):
#   Column i+1  → time step i.
#   Row    j+1  → node reached by j up-moves and (i-j) down-moves.
#   j = 0  → all-down path (lowest price at that step).
#   j = i  → all-up  path (highest price at that step).
# Backward induction children of node (j, i):
#   Up-move   child → (j+1, i+1)  i.e. matrix indices [j+2, i+2]
#   Down-move child → (j,   i+1)  i.e. matrix indices [j+1, i+2]
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
    )
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
        offset = 0.35,
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
  )
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
        selected = if ("MSFT" %in% stock_choices) "MSFT" else stock_choices[1],
        multiple = FALSE
      ),

      dateRangeInput(
        "date_range",
        "Historical volatility period:",
        start = "2016-03-01",
        end = "2026-02-27", # set to match project specifications
        min = start_date_global,
        max = Sys.Date()
      ),

      # show when data was last fetched
      div(
        style = "font-size: 0.85em; color: #666; margin-top: 4px;",
        textOutput("last_refresh_label")
      ),

      # Refresh button triggers re-download with progress indicator
      actionButton(
        "refresh_data",
        "Refresh Market Data",
        icon = icon("rotate"),
        class = "btn-sm btn-outline-secondary mt-1 mb-2 w-100"
      ),

      # dividend disclaimer
      div(
        class = "alert alert-warning p-2 mt-2",
        style = "font-size: 0.8em;",
        tags$b(
          icon("triangle-exclamation"),
          " Dividend adjustment not applied."
        ),
        tags$br(),
        "Several tickers (JNJ, PG, WMT, XOM, etc.) pay meaningful dividends.
         In a rigorous pricing context this would reduce call prices and
         increase put prices relative to the values shown here."
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

      # renamed from `r` to `r_f` to avoid shadowing R's built-in r()
      numericInput("r_f", "Risk-free rate (r):", value = 0.038, step = 0.001),

      # min = 0.01 prevents zero or negative strikes at the input level
      numericInput(
        "strike_mult",
        "Strike Multiplier (% of S0):",
        value = 1.05,
        step = 0.01,
        min = 0.01
      )
    ),

    mainPanel(
      uiOutput("summary_boxes"),
      br(),

      tabsetPanel(
        id = "tabs",

        # TAB 1: Data
        tabPanel(
          "1. Underlying Asset Data",
          br(),
          h4("Asset Log Returns"),
          # explain why the first log-return is NA
          div(
            class = "alert alert-info p-2 mb-2",
            style = "font-size: 0.85em;",
            icon("circle-info"),
            " The most recent row (if the end date has been set to today) shows ",
            tags$strong("NA"),
            " for the log return.
              Log returns require two consecutive closing prices, so the earliest
              observation in the filtered window has no prior price to difference against."
          ),
          DTOutput("log_returns_tbl"),
          br(),
          downloadButton(
            "download_data",
            "Download App Data as CSV",
            class = "btn-primary"
          ),
          br(),
          br()
        ),

        # TAB 2: Stock Price Tree
        tabPanel(
          "2. Stock Price Tree",
          br(),
          h4("CRR Binomial Stock Price Tree"),
          uiOutput("stock_tree_warning"),
          plotOutput("plot_stock_tree", height = "600px")
        ),

        # TAB 3: European Put
        tabPanel(
          "3. European Put",
          br(),
          h4("European Vanilla Put Option Value"),
          uiOutput("euro_summary"),
          br(),
          uiOutput("euro_tree_warning"),
          plotOutput("plot_euro_tree", height = "800px")
        ),

        # TAB 4: Bermudan/American Put
        # Note: Early exercise at every node is the American contract
        #  a Bermudan only permits exercise on a fixed discrete set of dates
        #  which is kind of what we have here with given timesteps, but if users
        #  edit settings too much then this will change
        tabPanel(
          "4. Bermudan/American Put",
          br(),
          h4("Bermudan/American Put Option Value"),
          div(
            class = "alert alert-info p-2 mb-2",
            style = "font-size: 0.85em;",
            icon("circle-info"),
            " ",
            tags$strong("American vs. Bermudan: "),
            "This tab prices an ",
            tags$strong("American"),
            " put, which allows
             early exercise at ",
            tags$em("every"),
            " node in the tree.
             A Bermudan option would only allow exercise on a pre-specified
             subset of dates."
          ),
          uiOutput("amer_summary"),
          br(),
          uiOutput("amer_tree_warning"), # claude suggestion
          plotOutput("plot_amer_tree", height = "800px")
        )
      )
    )
  )
)


#------------------------------------------------------------------------------
# SERVER LOGIC
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive store for market data - allows the Refresh button to replace it
  # without restarting the app
  market_data_rv <- reactiveVal(
    list(
      prices = master_prices,
      choices = stock_choices,
      refreshed = last_refresh_time
    )
  )

  # re-download on button click with a progress indicator
  observeEvent(input$refresh_data, {
    withProgress(
      message = "Downloading market data \u2014 please wait...",
      value = 0,
      {
        setProgress(0.1, detail = "Contacting Yahoo Finance...")
        new_prices <- download_all_stocks(
          stock_choices_default,
          start_date_global,
          Sys.Date()
        )
        setProgress(0.85, detail = "Updating reactive store...")
        new_choices <- intersect(stock_choices_default, colnames(new_prices))
        market_data_rv(list(
          prices = new_prices,
          choices = new_choices,
          refreshed = Sys.time()
        ))

        # keep selectizeInput in sync after refresh
        updateSelectizeInput(
          session,
          "selected_stock",
          choices = new_choices,
          selected = isolate(input$selected_stock)
        )

        # push today's date into the date-range max
        updateDateRangeInput(
          session,
          "date_range",
          end = Sys.Date(),
          max = Sys.Date()
        )
      }
    )
  })

  output$last_refresh_label <- renderText({
    md <- market_data_rv()
    paste("Data fetched:", format(md$refreshed, "%Y-%m-%d %H:%M"))
  })

  # 1. Reactive: filtered stock data
  stock_data <- reactive({
    req(input$selected_stock)
    md <- market_data_rv()

    validate(
      need(
        input$selected_stock %in% colnames(md$prices),
        "Selected stock data unavailable - try clicking Refresh Market Data."
      )
    )

    df <- md$prices %>%
      select(Date, close = all_of(input$selected_stock)) %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      drop_na()

    validate(need(
      nrow(df) > 10,
      "Fewer than 10 trading days in the selected date range. Please widen the window."
    ))

    df %>% mutate(log_ret = c(NA, diff(log(close))))
  })

  # 2. Reactive: CRR tree parameters
  tree_params <- reactive({
    df <- stock_data()
    log_rets <- na.omit(df$log_ret)
    S0 <- tail(df$close, 1)

    var_daily <- var(log_rets)
    std_daily <- sd(log_rets)
    sigma <- std_daily * sqrt(252)

    N <- input$time_steps
    T_mat <- input$t_mat
    dt <- T_mat / N
    u <- exp(sigma * sqrt(dt))
    d <- 1 / u

    r_f <- input$r_f # updated name <- was named `r`

    # guard against non-positive strike multiplier
    validate(need(
      isTRUE(input$strike_mult > 0),
      "Strike multiplier must be greater than zero."
    ))
    K <- input$strike_mult * S0

    q <- (exp(r_f * dt) - d) / (u - d)
    disc <- exp(-r_f * dt)

    # Fvalidate that q is in (0, 1); if not, the model is not arbitrage-free
    validate(need(
      isTRUE(q > 0) && isTRUE(q < 1),
      sprintf(
        "Risk-neutral probability q = %.4f lies outside (0, 1) \u2014 the model
is not arbitrage-free with the current parameters. Try lowering r, reducing T,
or increasing N.",
        q
      )
    ))

    list(
      S0 = S0,
      var_daily = var_daily,
      std_daily = std_daily,
      sigma = sigma,
      N = N,
      dt = dt,
      u = u,
      d = d,
      r_f = r_f,
      K = K,
      q = q,
      disc = disc
    )
  })

  # 3. Reactive: stock-price tree and option-value trees
  calc_trees <- reactive({
    p <- tree_params()
    N <- p$N

    # Stock price tree
    # S[j+1, i+1] = S0 * u^j * d^(i-j)   (j up-moves, i-j down-moves)
    S <- matrix(0, N + 1, N + 1)
    for (i in 0:N) {
      for (j in 0:i) {
        S[j + 1, i + 1] <- p$S0 * p$u^j * p$d^(i - j)
      }
    }

    # European Put: no early exercise - pure risk-neutral discounting
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

    # Bermudan Put as early exercise checked at every non-terminal node
    V_am <- matrix(0, N + 1, N + 1)
    early <- matrix(FALSE, N + 1, N + 1)
    for (j in 0:N) {
      V_am[j + 1, N + 1] <- max(p$K - S[j + 1, N + 1], 0)
    }
    for (i in (N - 1):0) {
      for (j in 0:i) {
        cont <- p$disc *
          (p$q * V_am[j + 2, i + 2] + (1 - p$q) * V_am[j + 1, i + 2])
        exer <- p$K - S[j + 1, i + 1]
        V_am[j + 1, i + 1] <- max(cont, exer)
        early[j + 1, i + 1] <- (exer > cont) & (exer > 0)
      }
    }

    list(S = S, V_eu = V_eu, V_am = V_am, early = early)
  })

  # reusable helper that emits a warning banner when N is large
  large_n_warning_ui <- function(n, threshold = 15) {
    if (isTRUE(n > threshold)) {
      tags$div(
        class = "alert alert-warning p-2 mb-2",
        style = "font-size: 0.85em;",
        icon("triangle-exclamation"),
        sprintf(
          " N = %d is large \u2014 node labels will overlap and the tree will be
            difficult to read. Consider N \u2264 %d for visual clarity.",
          n,
          threshold
        )
      )
    }
  }

  output$stock_tree_warning <- renderUI({
    large_n_warning_ui(input$time_steps)
  })
  output$euro_tree_warning <- renderUI({
    large_n_warning_ui(input$time_steps)
  })
  output$amer_tree_warning <- renderUI({
    large_n_warning_ui(input$time_steps)
  })

  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("app_data_", input$selected_stock, "_", Sys.Date(), ".csv")
    },
    content = function(file) write.csv(stock_data(), file, row.names = FALSE)
  )

  # --- Summary header ---
  output$summary_boxes <- renderUI({
    p <- tree_params()
    trees <- calc_trees()

    v0_eu <- trees$V_eu[1, 1]
    v0_am <- trees$V_am[1, 1]
    prem <- v0_am - v0_eu

    arrow_html <- ifelse(
      prem > 0.0001,
      "<span style='color:green; font-weight:bold;'>&#9650;</span>",
      ""
    )

    tagList(
      card(
        card_header(
          "Model Parameters & Volatility Metrics",
          class = "bg-primary text-white"
        ),
        card_body(HTML(sprintf(
          "<div style='font-size: 1.1em;'>
           <b>S0:</b> $%.2f &nbsp;|&nbsp; <b>Strike (K):</b> $%.2f &nbsp;|&nbsp;
           <b>Var:</b> %.6f &nbsp;|&nbsp; <b>SD:</b> %.6f &nbsp;|&nbsp;
           <b>Ann. Vol (\u03c3):</b> %.6f <br>
           <b>dt:</b> %.4f &nbsp;|&nbsp; <b>N:</b> %d &nbsp;|&nbsp;
           <b>u:</b> %.6f &nbsp;|&nbsp; <b>d:</b> %.6f &nbsp;|&nbsp;
           <b>q:</b> %.6f &nbsp;|&nbsp; <b>r:</b> %.1f%%
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
          p$r_f * 100
        )))
      ),
      layout_column_wrap(
        width = 1 / 3,
        value_box(
          title = "European Put (V0)",
          value = sprintf("$%.4f", v0_eu),
          theme = "success"
        ),
        value_box(
          title = "Bermudan/American Put (V0)",
          value = sprintf("$%.4f", v0_am),
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

  # --- Tab 1 ---
  output$log_returns_tbl <- renderDT({
    df <- stock_data() %>%
      arrange(desc(Date)) %>%
      mutate(
        Close = round(close, 2),
        `Daily log returns` = round(log_ret, 4)
      ) %>%
      select(Date, Close, `Daily log returns`)
    datatable(df, options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
  })

  # --- Tab 2 ---
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
        )
      }
    }
  })

  # --- Tab 3: European Put ---
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

    par(mar = c(0, 4, 3, 1))
    plot_tree(
      trees$V_eu,
      trees$S,
      sprintf(
        "European Put (K=%.2f, r=%.1f%%)  |  V0 = %.4f",
        p$K,
        p$r_f * 100,
        trees$V_eu[1, 1]
      ),
      N = p$N,
      log_scale = TRUE,
      show_xlab = FALSE,
      show_x_ticks = FALSE
    )

    par(mar = c(4, 4, 0.5, 1), mgp = c(2, 1, 0))
    plot_tree(trees$V_eu, trees$S, title = "", N = p$N, log_scale = FALSE)
  })

  # --- Tab 4: American/Bermudan Put ---
  output$amer_summary <- renderUI({
    p <- tree_params()
    trees <- calc_trees()
    prem <- trees$V_am[1, 1] - trees$V_eu[1, 1]
    HTML(sprintf(
      "<b>S0:</b> $%.2f | <b>Strike (K):</b> $%.2f | <b>Option Value (V0):</b> $%.4f | <b>Premium:</b> $%.4f",
      p$S0,
      p$K,
      trees$V_am[1, 1],
      prem
    ))
  })

  output$plot_amer_tree <- renderPlot({
    p <- tree_params()
    trees <- calc_trees()

    layout(matrix(1:2), heights = c(1.8, 3.2))
    par(oma = c(0, 0, 0, 0))

    par(mar = c(0, 4, 3, 1))
    plot_tree(
      trees$V_am,
      trees$S,
      sprintf(
        "Bermudan/American Put (K=%.2f, r=%.1f%%)  |  V0 = %.4f",
        p$K,
        p$r_f * 100,
        trees$V_am[1, 1]
      ),
      N = p$N,
      log_scale = TRUE,
      show_xlab = FALSE,
      show_x_ticks = FALSE,
      show_early = trees$early
    )

    par(mar = c(4, 4, 0.5, 1), mgp = c(2, 1, 0))
    plot_tree(
      trees$V_am,
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
