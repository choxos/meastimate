library(shiny)

ui <- fluidPage(
        titlePanel("meastimate: A Shinyapp for estimating the missing means and standard deviations of studies for meta-analysis"),
        sidebarLayout(
                sidebarPanel(
                        selectInput("circumstance", "Select Circumstance:",
                                    choices = c("Circumstance-1: only within-group standard error is available",
                                                "Circumstance-2: only within-group confidence interval is available",
                                                "Sub-circumstance 3.1: only minimum, median, and maximum are available",
                                                "Sub-circumstance 3.2: only median and quartiles are available",
                                                "Sub-circumstance 3.3: minimum, Q1, median, Q3, and maximum are available",
                                                "Sub-circumstance 3.4: only range is available but without minimum and maximum",
                                                "Sub-circumstance 3.5: only interquartile range is available but without Q1 and Q3",
                                                "Circumstance-4: pooled SD from two subgroups",
                                                "Circumstance 5: SD for the change score",
                                                "Circumstance 6: available data is SE of difference between two groups",
                                                "Circumstance 7: only effect estimates with corresponding CI are available",
                                                "Circumstance 8: only effect estimates with the z-score between two groups are available",
                                                "Circumstance 9: only effect estimates with the t-value between two groups are available",
                                                "Circumstance 10: only effect estimates with p value between two groups are available")),
                        uiOutput("inputUI")
                ),
                mainPanel(
                        textOutput("result")
                )
        ),
        tags$p("Developed by: Ahmad Sofi-Mahmudi",
               br(),
               tags$a("GitHub", href = "https://github.com/choxos", target = "_blank"),
               tags$a("Twitter", href = "https://twitter.com/ASofiMahmudi", target = "_blank"),
               tags$a("LinkedIn", href = "https://www.linkedin.com/in/asofimahmudi/", target = "_blank"),
               style="text-align: center;"
        ),
        p("made with <3 at McMaster University", style="text-align: center;"),
        tags$div(
                style = "text-align: center;",
                tags$img(src = "kurdistan.png", height = "5%", width = "5%")
        )
)

server <- function(input, output) {
        output$inputUI <- renderUI({
                switch(input$circumstance,
                       "Circumstance-1: only within-group standard error is available" = 
                               tagList(textInput("se", "Enter the standard error (SE):"),
                                       textInput("n", "Enter the sample size (n):"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       
                       "Circumstance-2: only within-group confidence interval is available" =
                               tagList(textInput("n", "Enter the sample size (n):"),
                                       textInput("lower", "Enter the lower limit:"),
                                       textInput("upper", "Enter the upper limit:"),
                                       numericInput("zvalue", "Enter z-value or t-value (default: 1.959964):", 1.959964, min = -4, max = 4),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Sub-circumstance 3.1: only minimum, median, and maximum are available" =
                               tagList(textInput("n", "Enter the sample size (n):"),
                                       textInput("median", "Enter the median value:"),
                                       textInput("min", "Enter the minimum value:"),
                                       textInput("max", "Enter the maximum value:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Sub-circumstance 3.2: only median and quartiles are available" =
                               tagList(textInput("n", "Enter the sample size (n):"),
                                       textInput("q1", "Enter the first quartile (Q1):"),
                                       textInput("median", "Enter the median value:"),
                                       textInput("q3", "Enter the third quartile (Q3):"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Sub-circumstance 3.3: minimum, Q1, median, Q3, and maximum are available" =
                               tagList(textInput("n", "Enter the sample size (n):"),
                                       textInput("q1", "Enter the first quartile (Q1):"),
                                       textInput("median", "Enter the median value:"),
                                       textInput("q3", "Enter the third quartile (Q3):"),
                                       textInput("min", "Enter the minimum value:"),
                                       textInput("max", "Enter the maximum value:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Sub-circumstance 3.4: only range is available but without minimum and maximum" =
                               tagList(textInput("n", "Enter the sample size (n):"),
                                       textInput("range", "Enter the range:"),
                                       textInput("median", "Enter the median:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Sub-circumstance 3.5: only interquartile range is available but without Q1 and Q3" =
                               tagList(textInput("n", "Enter the sample size (n):"),
                                       textInput("iqr", "Enter the interquartile range:"),
                                       textInput("median", "Enter the median:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance-4: pooled SD from two subgroups" =
                               tagList(textInput("n1", "Enter the sample size for the first group (n1):"),
                                       textInput("n2", "Enter the sample size for the second group (n2):"),
                                       textInput("mean1", "Enter the mean for the first group:"),
                                       textInput("mean2", "Enter the mean for the second group:"),
                                       textInput("sd1", "Enter the standard deviation for the first group (SD1):"),
                                       textInput("sd2", "Enter the standard deviation for the second group (SD2)"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance 5: SD for the change score" =
                               tagList(textInput("sdebase", "Enter the baseline SD for experimental group:"),
                                       textInput("sdepost", "Enter the post-intervention SD for experimental group"),
                                       textInput("sdechange", "Enter the change-score SD for experimental group:"),
                                       textInput("sdcbase", "Enter the baseline SD for control group:"),
                                       textInput("sdcpost", "Enter the post-intervention SD for control group"),
                                       textInput("sdcchange", "Enter the change-score SD for control group:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance 6: available data is SE of difference between two groups" =
                               tagList(textInput("n1", "Enter the sample size for the experimental group:"),
                                       textInput("n2", "Enter the sample size for the control group:"),
                                       textInput("se", "Enter the standard error (SE):"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance 7: only effect estimates with corresponding CI are available" =
                               tagList(textInput("n1", "Enter the sample size for the experimental group:"),
                                       textInput("n2", "Enter the sample size for the control group:"),
                                       textInput("lower", "Enter the lower limit for CI:"),
                                       textInput("upper", "Enter the upper limit for CI:"),
                                       numericInput("zvalue", "Enter z-value (default: 1.959964)", 1.959964, min = -4, max = 4),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance 8: only effect estimates with the z-score between two groups are available" =
                               tagList(textInput("n1", "Enter the sample size for the experimental group:"),
                                       textInput("n2", "Enter the sample size for the control group:"),
                                       textInput("effect", "Enter the effect estimate:"),
                                       textInput("zscore", "Enter the z-score:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance 9: only effect estimates with the t-value between two groups are available" =
                               tagList(textInput("n1", "Enter the sample size for the experimental group:"),
                                       textInput("n2", "Enter the sample size for the control group:"),
                                       textInput("effect", "Enter the effect estimate:"),
                                       textInput("tscore", "Enter the t-score:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                       "Circumstance 10: only effect estimates with p value between two groups are available" =
                               tagList(textInput("n1", "Enter the sample size for the experimental group:"),
                                       textInput("n2", "Enter the sample size for the control group:"),
                                       textInput("effect", "Enter the effect estimate:"),
                                       textInput("pvalue", "Enter the P-value:"),
                                       numericInput("rounding", "Select decimal places for rounding:", 3, min = 0, max = 10)),
                )
        })
        
        output$result <- renderText({
                if (input$circumstance == "Circumstance-1: only within-group standard error is available") {
                        if (!is.null(input$se) && !is.null(input$n) && !is.null(input$rounding)) {
                                se <- as.numeric(input$se)
                                n <- as.numeric(input$n)
                                rounding <- as.integer(input$rounding)
                                sd <- round(se * sqrt(n), digits = rounding)
                                paste("Standard deviation (SD) is:", sd)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance-2: only within-group confidence interval is available") {
                                if (!is.null(input$n) &&!is.null(input$lower) && !is.null(input$upper) && !is.null(input$zvalue)) {
                                        n <- as.numeric(input$n)
                                        lower <- as.numeric(input$lower)
                                        upper <- as.numeric(input$upper)
                                        zvalue <- as.numeric(input$zvalue)
                                        rounding <- as.integer(input$rounding)
                                        sd <- round(sqrt(n) * ((upper-lower)/(2*zvalue)), digits = rounding)
                                        paste("Standard deviation (SD) is:", sd)
                                } else {
                                        "Please input the required information."
                                }
                } else if (input$circumstance == "Sub-circumstance 3.1: only minimum, median, and maximum are available") {
                        if (!is.null(input$n) &&!is.null(input$min) && !is.null(input$median) && !is.null(input$max)) {
                                n <- as.numeric(input$n)
                                min <- as.numeric(input$min)
                                median <- as.numeric(input$median)
                                max <- as.numeric(input$max)
                                rounding <- as.integer(input$rounding)
                                mean = round(((4/(4+n^0.75)) * ((min+max)/2)) + (((n^0.75)/(4+n^0.75))*median), digits = rounding)
                                sd = round((max-min)/(2*pnorm((n-0.375)/(n+0.25), lower.tail = F)), digits = rounding)
                                paste("Mean is:", mean, "and standard deviation (SD) is:", sd)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Sub-circumstance 3.2: only median and quartiles are available") {
                        if (!is.null(input$n) &&!is.null(input$q1) && !is.null(input$median) && !is.null(input$q3)) {
                                n <- as.numeric(input$n)
                                q1 <- as.numeric(input$q1)
                                median <- as.numeric(input$median)
                                q3 <- as.numeric(input$q3)
                                rounding <- as.integer(input$rounding)
                                mean = round(((0.7+(0.39/n))*((q1+q3)/2)) + ((0.3-(0.39/n))*median), digits = rounding)
                                sd = round((q3-q1)/(2*pnorm((0.75*n-0.125)/(n+0.25), lower.tail = F)), digits = rounding)
                                paste("Mean is:", mean, "and standard deviation (SD) is:", sd)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Sub-circumstance 3.3: minimum, Q1, median, Q3, and maximum are available") {
                        if (!is.null(input$n) &&!is.null(input$q1) && !is.null(input$median) && !is.null(input$q3) && !is.null(input$min) && !is.null(input$max)) {
                                n <- as.numeric(input$n)
                                q1 <- as.numeric(input$q1)
                                median <- as.numeric(input$median)
                                q3 <- as.numeric(input$q3)
                                min <- as.numeric(input$min)
                                max <- as.numeric(input$max)
                                rounding <- as.integer(input$rounding)
                                mean = round(((2.2/(2.2+n^0.75))*((min+max)/2)) + ((0.7 - (0.72/n^0.55))*((q1+q3)/2)) + ((0.3 + (0.72/n^0.55) - (2.2/(2.2+n^0.75)))*median), digits = rounding)
                                sd = round(((max-min)/(4*pnorm((n-0.375)/(n+0.25), lower.tail = F))) + ((q3-q1)/(4*pnorm((0.75*n-0.125)/(n+0.25)))), digits = rounding)
                                paste("Mean is:", mean, "and standard deviation (SD) is:", sd)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Sub-circumstance 3.4: only range is available but without minimum and maximum") {
                        if (!is.null(input$n) &&!is.null(input$range)) {
                                n <- as.numeric(input$n)
                                range <- as.numeric(input$range)
                                median <- as.numeric(input$median)
                                rounding <- as.integer(input$rounding)
                                sd = round(range/(2*pnorm((n-0.375)/(n+0.25), lower.tail = F)), digits = rounding)
                                paste("Mean is equal to median (if data are normally distributed):", median, "and standard deviation (SD) is:", sd)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Sub-circumstance 3.5: only interquartile range is available but without Q1 and Q3") {
                        if (!is.null(input$n) &&!is.null(input$iqr)) {
                                n <- as.numeric(input$n)
                                iqr <- as.numeric(input$iqr)
                                median <- as.numeric(input$median)
                                rounding <- as.integer(input$rounding)
                                sd = round(iqr/(2*pnorm((0.75*n-0.125)/(n+0.25), lower.tail = F)), digits = rounding)
                                paste("Mean is equal to median (if data are normally distributed):", median, "and standard deviation (SD) is:", sd)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance-4: pooled SD from two subgroups") {
                        if (!is.null(input$n1) &&!is.null(input$n2) && !is.null(input$sd1) && !is.null(input$sd2)) {
                                n1 <- as.numeric(input$n1)
                                n2 <- as.numeric(input$n2)
                                mean1 <- as.numeric(input$mean1)
                                mean2 <- as.numeric(input$mean2)
                                sd1 <- as.numeric(input$sd1)
                                sd2 <- as.numeric(input$sd2)
                                rounding <- as.integer(input$rounding)
                                if(is.na(mean1)==F && is.na(mean2)==F){
                                        sd_pooled = round(sqrt((((n1-1)*sd1^2) + ((n2-1)*sd2^2) + (((n1*n2)/(n1+n2))*((mean1^2+mean2^2) - (2*mean1*mean2))))/(n1+n2-1)), digits = rounding)
                                } else {
                                        sd_pooled = round(sqrt((((n1-1)*sd1^2) + ((n2-1)*sd2^2))/(n1+n2-2)), digits = rounding)
                                }
                                paste("Pooled standard deviation (SD) is:", sd_pooled)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance 5: SD for the change score") {
                        if (!is.null(input$sdebase) &&!is.null(input$sdepost) && !is.null(input$sdechange) && !is.null(input$sdcbase) &&!is.null(input$sdcpost) && !is.null(input$sdcchange)) {
                                sdebase <- as.numeric(input$sdebase)
                                sdepost <- as.numeric(input$sdepost)
                                sdechange <- as.numeric(input$sdechange)
                                sdcbase <- as.numeric(input$sdcbase)
                                sdcpost <- as.numeric(input$sdcpost)
                                sdcchange <- as.numeric(input$sdcchange)
                                rounding <- as.integer(input$rounding)
                                corre = (sdebase^2+sdepost^2-sdechange^2)/(2*sdechange*sdepost)
                                corrc = (sdcbase^2+sdcpost^2-sdcchange^2)/(2*sdcchange*sdcpost)
                                corr_overall = (corre+corrc)/2
                                sde_change = round(sqrt(sdebase^2 + sdepost^2 - (2*corr_overall*sdebase*sdepost)), digits = rounding)
                                sdc_change = round(sqrt(sdcbase^2 + sdcpost^2 - (2*corr_overall*sdcbase*sdcpost)), digits = rounding)
                                paste("Change standard deviation (SD) for experimental group is:", sde_change, "and for control group is:", sdc_change)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance 6: available data is SE of difference between two groups") {
                        if (!is.null(input$n1) &&!is.null(input$n2) && !is.null(input$se)) {
                                n1 <- as.numeric(input$n1)
                                n2 <- as.numeric(input$n2)
                                se <- as.numeric(input$se)
                                rounding <- as.integer(input$rounding)
                                sd_average = round(se/sqrt(1/n1+1/n2), digits = rounding)
                                paste("The average standard deviation (SD) is:", sd_average)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance 7: only effect estimates with corresponding CI are available") {
                        if (!is.null(input$n1) &&!is.null(input$n2) && !is.null(input$lower) && !is.null(input$upper) && !is.null(input$zvalue)) {
                                n1 <- as.numeric(input$n1)
                                n2 <- as.numeric(input$n2)
                                lower <- as.numeric(input$lower)
                                upper <- as.numeric(input$upper)
                                zvalue <- as.integer(input$zvalue)
                                rounding <- as.integer(input$rounding)
                                se = sqrt(n) * ((upper-lower)/(2*zvalue))
                                sd_average = round(se/sqrt(1/n1+1/n2), digits = rounding)
                                paste("The average standard deviation (SD) is:", sd_average)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance 8: only effect estimates with the z-score between two groups are available") {
                        if (!is.null(input$n1) &&!is.null(input$n2) && !is.null(input$effect) && !is.null(input$zscore)) {
                                n1 <- as.numeric(input$n1)
                                n2 <- as.numeric(input$n2)
                                effect <- as.numeric(input$effect)
                                zscore <- as.numeric(input$zscore)
                                rounding <- as.integer(input$rounding)
                                se = effect/zscore
                                sd_average = round(se/sqrt(1/n1+1/n2), digits = rounding)
                                paste("The average standard deviation (SD) is:", sd_average)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance 9: only effect estimates with the t-value between two groups are available") {
                        if (!is.null(input$n1) &&!is.null(input$n2) && !is.null(input$effect) && !is.null(input$zscore)) {
                                n1 <- as.numeric(input$n1)
                                n2 <- as.numeric(input$n2)
                                effect <- as.numeric(input$effect)
                                tscore <- as.numeric(input$tscore)
                                rounding <- as.integer(input$rounding)
                                se = abs(effect/tscore)
                                sd_average = round(se/sqrt(1/n1+1/n2), digits = rounding)
                                paste("The average standard deviation (SD) is:", sd_average)
                        } else {
                                "Please input the required information."
                        }
                } else if (input$circumstance == "Circumstance 10: only effect estimates with p value between two groups are available") {
                        if (!is.null(input$n1) &&!is.null(input$n2) && !is.null(input$effect) && !is.null(input$pvalue)) {
                                n1 <- as.numeric(input$n1)
                                n2 <- as.numeric(input$n2)
                                effect <- as.numeric(input$effect)
                                pvalue <- as.numeric(input$pvalue)
                                rounding <- as.integer(input$rounding)
                                zscore = abs(qnorm(pvalue))
                                se = effect/zscore
                                sd_average = round(se/sqrt(1/n1+1/n2), digits = rounding)
                                paste("The average standard deviation (SD) is:", sd_average)
                        } else {
                                "Please input the required information."
                        }
                }
        })
}



shinyApp(ui = ui, server = server)
