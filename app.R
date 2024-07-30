library(shiny)
library(shinydashboard)

# Helper functions
z_score = function(p) {
        qnorm(1 - p/2)
}

t_score = function(p, df) {
        qt(1 - p/2, df)
}

# UI
ui = dashboardPage(
        dashboardHeader(title = "Meastimate"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Within-Group", tabName = "within", icon = icon("chart-bar")),
                        menuItem("Between-Group", tabName = "between", icon = icon("chart-line")),
                        menuItem("About", tabName = "about", icon = icon("info-circle"))
                )
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "within",
                                tabBox(
                                        title = "Within-Group Circumstances",
                                        id = "within_tabs",
                                        width = 12,
                                        tabPanel("1. Standard Error",
                                                 numericInput("se_n", "Sample Size", value = 100),
                                                 numericInput("se_se", "Standard Error", value = 1),
                                                 actionButton("se_calc", "Calculate"),
                                                 verbatimTextOutput("se_result")
                                        ),
                                        tabPanel("2. Confidence Interval",
                                                 numericInput("ci_n", "Sample Size", value = 100),
                                                 numericInput("ci_lower", "Lower CI", value = 0),
                                                 numericInput("ci_upper", "Upper CI", value = 2),
                                                 numericInput("ci_conf", "Confidence Level (e.g., 0.95)", value = 0.95),
                                                 actionButton("ci_calc", "Calculate"),
                                                 verbatimTextOutput("ci_result")
                                        ),
                                        tabPanel("3. Descriptive Statistics",
                                                 selectInput("desc_type", "Available Data",
                                                             choices = c("Median, Min, Max", "Median, Q1, Q3", "Min, Q1, Median, Q3, Max")),
                                                 numericInput("desc_n", "Sample Size", value = 100),
                                                 numericInput("desc_min", "Minimum", value = 0),
                                                 numericInput("desc_q1", "Q1", value = 25),
                                                 numericInput("desc_median", "Median", value = 50),
                                                 numericInput("desc_q3", "Q3", value = 75),
                                                 numericInput("desc_max", "Maximum", value = 100),
                                                 actionButton("desc_calc", "Calculate"),
                                                 verbatimTextOutput("desc_result")
                                        ),
                                        tabPanel("4. Pooled SD",
                                                 numericInput("pool_n1", "Sample Size 1", value = 50),
                                                 numericInput("pool_sd1", "SD 1", value = 10),
                                                 numericInput("pool_n2", "Sample Size 2", value = 50),
                                                 numericInput("pool_sd2", "SD 2", value = 12),
                                                 numericInput("pool_mean1", "Mean 1", value = 100),
                                                 numericInput("pool_mean2", "Mean 2", value = 105),
                                                 actionButton("pool_calc", "Calculate"),
                                                 verbatimTextOutput("pool_result")
                                        ),
                                        tabPanel("5. SD for Change Score",
                                                 numericInput("change_n", "Sample Size", value = 100),
                                                 numericInput("change_sd_baseline", "SD Baseline", value = 10),
                                                 numericInput("change_sd_post", "SD Post", value = 12),
                                                 numericInput("change_corr", "Correlation", value = 0.5),
                                                 actionButton("change_calc", "Calculate"),
                                                 verbatimTextOutput("change_result")
                                        )
                                )
                        ),
                        tabItem(tabName = "between",
                                tabBox(
                                        title = "Between-Group Circumstances",
                                        id = "between_tabs",
                                        width = 12,
                                        tabPanel("6. SE of Difference",
                                                 numericInput("diff_n1", "Sample Size 1", value = 50),
                                                 numericInput("diff_n2", "Sample Size 2", value = 50),
                                                 numericInput("diff_se", "SE of Difference", value = 2),
                                                 actionButton("diff_calc", "Calculate"),
                                                 verbatimTextOutput("diff_result")
                                        ),
                                        tabPanel("7. Effect Estimate with CI",
                                                 numericInput("effect_n1", "Sample Size 1", value = 50),
                                                 numericInput("effect_n2", "Sample Size 2", value = 50),
                                                 numericInput("effect_est", "Effect Estimate", value = 5),
                                                 numericInput("effect_lower", "Lower CI", value = 2),
                                                 numericInput("effect_upper", "Upper CI", value = 8),
                                                 numericInput("effect_conf", "Confidence Level (e.g., 0.95)", value = 0.95),
                                                 actionButton("effect_calc", "Calculate"),
                                                 verbatimTextOutput("effect_result")
                                        ),
                                        tabPanel("8. Effect Estimate with Z-score",
                                                 numericInput("z_n1", "Sample Size 1", value = 50),
                                                 numericInput("z_n2", "Sample Size 2", value = 50),
                                                 numericInput("z_est", "Effect Estimate", value = 5),
                                                 numericInput("z_score", "Z-score", value = 1.96),
                                                 actionButton("z_calc", "Calculate"),
                                                 verbatimTextOutput("z_result")
                                        ),
                                        tabPanel("9. Effect Estimate with t-value",
                                                 numericInput("t_n1", "Sample Size 1", value = 50),
                                                 numericInput("t_n2", "Sample Size 2", value = 50),
                                                 numericInput("t_est", "Effect Estimate", value = 5),
                                                 numericInput("t_value", "t-value", value = 2),
                                                 actionButton("t_calc", "Calculate"),
                                                 verbatimTextOutput("t_result")
                                        ),
                                        tabPanel("10. Effect Estimate with p-value",
                                                 numericInput("p_n1", "Sample Size 1", value = 50),
                                                 numericInput("p_n2", "Sample Size 2", value = 50),
                                                 numericInput("p_est", "Effect Estimate", value = 5),
                                                 numericInput("p_value", "p-value", value = 0.05),
                                                 radioButtons("p_dist", "Distribution", choices = c("z", "t"), selected = "z"),
                                                 actionButton("p_calc", "Calculate"),
                                                 verbatimTextOutput("p_result")
                                        )
                                )
                        ),
                        # About tab
                        tabItem(tabName = "about",
                                fluidRow(
                                        box(
                                                title = "About the Creator",
                                                width = 12,
                                                p("Creator: Ahmad Sofi-Mahmudi"),
                                                br(),
                                                p("Social Media:"),
                                                tags$div(
                                                        tags$a("Blog", href = "https://choxos.com", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("GitHub", href = "https://github.com/choxos", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("Twitter", href = "https://twitter.com/ASofiMahmudi", target = "_blank"),
                                                        tags$span(" | "),
                                                        tags$a("LinkedIn", href = "https://www.linkedin.com/in/asofimahmudi/", target = "_blank")
                                                )
                                        )
                                )
                        )
                )
        )
)

# Server
server = function(input, output) {
        
        # 1. Standard Error
        observeEvent(input$se_calc, {
                sd = sqrt(input$se_n) * input$se_se
                output$se_result = renderPrint({
                        cat("Estimated SD:", round(sd, 4))
                })
        })
        
        # 2. Confidence Interval
        observeEvent(input$ci_calc, {
                if (input$ci_n >= 60) {
                        z = z_score((1 - input$ci_conf) / 2)
                        se = (input$ci_upper - input$ci_lower) / (2 * z)
                } else {
                        t = t_score((1 - input$ci_conf) / 2, input$ci_n - 1)
                        se = (input$ci_upper - input$ci_lower) / (2 * t)
                }
                sd = sqrt(input$ci_n) * se
                output$ci_result = renderPrint({
                        cat("Estimated SD:", round(sd, 4))
                })
        })
        
        # 3. Descriptive Statistics
        observeEvent(input$desc_calc, {
                n = input$desc_n
                if (input$desc_type == "Median, Min, Max") {
                        a = input$desc_min
                        m = input$desc_median
                        b = input$desc_max
                        est_mean = if (n < 25) (a + 2*m + b) / 4 else m
                        est_sd = if (n <= 15) sqrt(((b-a)^2 + (a-2*m+b)^2) / 12)
                        else if (n <= 70) (b-a) / 4
                        else (b-a) / 6
                } else if (input$desc_type == "Median, Q1, Q3") {
                        q1 = input$desc_q1
                        m = input$desc_median
                        q3 = input$desc_q3
                        est_mean = (0.7 + 0.39/n) * (q1+q3)/2 + (0.3 - 0.39/n) * m
                        est_sd = (q3-q1) / (2 * qnorm((0.75*n - 0.125) / (n + 0.25)))
                } else {
                        a = input$desc_min
                        q1 = input$desc_q1
                        m = input$desc_median
                        q3 = input$desc_q3
                        b = input$desc_max
                        w = 2.2 / (2.2 + n^0.75)
                        est_mean = w * (a+b)/2 + (0.7 - 0.72/n^0.55) * (q1+q3)/2 + (0.3 + 0.72/n^0.55 - w) * m
                        est_sd = (b-a)/(4*qnorm((n-0.375)/(n+0.25))) + (q3-q1)/(4*qnorm((0.75*n-0.125)/(n+0.25)))
                }
                output$desc_result = renderPrint({
                        cat("Estimated Mean:", round(est_mean, 4), "\n")
                        cat("Estimated SD:", round(est_sd, 4))
                })
        })
        
        # 4. Pooled SD
        observeEvent(input$pool_calc, {
                n1 = input$pool_n1
                n2 = input$pool_n2
                sd1 = input$pool_sd1
                sd2 = input$pool_sd2
                m1 = input$pool_mean1
                m2 = input$pool_mean2
                pooled_sd = sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2 + n1*n2/(n1+n2)*(m1^2+m2^2-2*m1*m2)) / (n1+n2-1))
                output$pool_result = renderPrint({
                        cat("Pooled SD:", round(pooled_sd, 4))
                })
        })
        
        # 5. SD for Change Score
        observeEvent(input$change_calc, {
                sd_change = sqrt(input$change_sd_baseline^2 + input$change_sd_post^2 - 
                                          2 * input$change_corr * input$change_sd_baseline * input$change_sd_post)
                output$change_result = renderPrint({
                        cat("SD for Change Score:", round(sd_change, 4))
                })
        })
        
        # 6. SE of Difference
        observeEvent(input$diff_calc, {
                sd_avg = input$diff_se / sqrt(1/input$diff_n1 + 1/input$diff_n2)
                output$diff_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        # 7. Effect Estimate with CI
        observeEvent(input$effect_calc, {
                if (input$effect_n1 + input$effect_n2 >= 60) {
                        z = z_score((1 - input$effect_conf) / 2)
                        se = (input$effect_upper - input$effect_lower) / (2 * z)
                } else {
                        t = t_score((1 - input$effect_conf) / 2, input$effect_n1 + input$effect_n2 - 2)
                        se = (input$effect_upper - input$effect_lower) / (2 * t)
                }
                sd_avg = se / sqrt(1/input$effect_n1 + 1/input$effect_n2)
                output$effect_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        # 8. Effect Estimate with Z-score
        observeEvent(input$z_calc, {
                se = abs(input$z_est / input$z_score)
                sd_avg = se / sqrt(1/input$z_n1 + 1/input$z_n2)
                output$z_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        # 9. Effect Estimate with t-value
        observeEvent(input$t_calc, {
                se = abs(input$t_est / input$t_value)
                sd_avg = se / sqrt(1/input$t_n1 + 1/input$t_n2)
                output$t_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
        
        # 10. Effect Estimate with p-value
        observeEvent(input$p_calc, {
                if (input$p_dist == "z") {
                        score = z_score(input$p_value)
                } else {
                        score = t_score(input$p_value, input$p_n1 + input$p_n2 - 2)
                }
                se = abs(input$p_est / score)
                sd_avg = se / sqrt(1/input$p_n1 + 1/input$p_n2)
                output$p_result = renderPrint({
                        cat("Average SD:", round(sd_avg, 4))
                })
        })
}

# Run the app
shinyApp(ui, server)