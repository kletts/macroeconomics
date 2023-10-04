library(tidyverse)
library(tsibble)
library(tsDyn)
library(googleCloudStorageR)
library(shiny)

source("~/Bizscisolve/Rfunlib/R/varfan.R")

macrodata <- Rfunlib::read_asrds_gc("au-varmacro-4qchgs.rds")
macrovar <- c("CashRate", "UnempRate", "LabourEarn", "HousePriceGrowth")

#varmod2 <- tsDyn::lineVar(macrodata %>% dplyr::select(all_of(macrovar)), 
#                         lag=2, include="const")


rev_trans.4q <- function(x, start=c(1,1,1,1)) { 
        n <- sum(!is.na(x))
        xt <- matrix(c(rep(0, 4), x[!is.na(x)]/100), nrow=4) + 1
        xt <- c(t(apply(xt, 1, cumprod)) * start)[5:(n+4)]
        x[!is.na(x)] <- xt
        return(x)
}

fit_varmodel <- function(macrodata, macrovar, lags, start=NULL, end=NULL) { 
    logvar <- c("CashRate", "UnempRate")
    macrodata %>% 
        { if (!is.null(start)) filter(., period >= yearquarter(start)) } %>% 
        { if (!is.null(end)) filter(., period <= yearquarter(end)) } %>% 
        dplyr::select(all_of(macrovar)) %>% 
        mutate(across(any_of(logvar), log)) %>% 
        tsDyn::lineVar(., lag=lags, include="const")
    }

build_macroscenario <- function(varmod, macrodata, start="2015 Q1", n.ahead=20, ci=0.95) { 
    logvar <- c("CashRate", "UnempRate")
    yvar <- attr(varmod$fitted.values, "dimnames")[[2]]
    startq <- yearquarter(start) - (varmod$lag-1) 
    endq <- yearquarter(start) + n.ahead
    fdata <- macrodata %>% filter(period>=startq) %>% slice_head(n=varmod$lag) %>% 
        dplyr::select(all_of(yvar)) %>% 
        mutate(across(any_of(logvar), log))
    forecast <- predict_var(varmod, newdata=fdata, n.ahead=n.ahead, ci=ci)
    forecast %>% imap_dfr( ~mutate(.x, 
                                   period=yearquarter(start) + 1:n.ahead, 
                                   scen=.y)) %>% 
        mutate(across(any_of(logvar), exp)) %>% 
        pivot_longer(cols=all_of(yvar), names_to="ecovar", values_to="value") %>% 
        pivot_wider(id_cols=c(period, ecovar), names_from =scen, values_from=value) %>% 
        full_join(macrodata %>% 
                      filter(between(period, startq, endq)) %>% 
                      pivot_longer(cols=all_of(yvar), names_to="ecovar", values_to="actual") %>%  
                      dplyr::select(period, ecovar, actual), 
                  by=c("period", "ecovar")) %>% 
        arrange(ecovar, period) %>% 
        mutate(across(c(fcst, lower, upper), ~coalesce(.x, actual))) %>%  
        group_by(ecovar) %>% 
        mutate(across(c(fcst, lower, upper, actual), ~rev_trans.4q(.x), .names="{.col}_cum"))
    }

ggplot_sample <- function(macrodata, macrovar, start=NULL, end=NULL) { 
    if (is.null(start)) { start <- min(macrodata$period) }
    if (is.null(end)) { end <- max(macrodata$period) }
    macrodata%>% 
        pivot_longer(cols=-period, names_to="macrovar", values_to="value") %>% 
        filter(macrovar %in% !!macrovar) %>% 
        ggplot(aes(x=period, y=value, col=macrovar)) + 
        annotate(geom="rect", xmin=yearquarter(start), xmax=yearquarter(end), 
                 ymin=-Inf, ymax=Inf, fill="lightgray", alpha=0.7) +     
            geom_line(linewidth=1.1) + 
            facet_wrap(vars(macrovar), scales="free_y") + 
            theme_bw() + 
            scale_color_manual(values = c("#BDA14D", "#3EBCB6","#0169C4","#153460")) + 
        theme(legend.position="none")
            
    }

ggplot_vardata <- function(vardata, type=c("chg", "cum")) {  
    type <- match.arg(type)
    asat <- format(min(vardata$period), "Q%q %Y")
    yvar <- c("fcst", "lower", "upper", "actual")
    if (type=="cum") { yvar <- paste(yvar, "cum", sep="_") }
    vardata %>% 
        ggplot(aes(x=period)) + 
        facet_wrap(vars(ecovar), scales = "free_y") + 
        geom_ribbon(aes(ymin=.data[[yvar[2]]], ymax=.data[[yvar[3]]], fill="Forecast"), alpha=0.4) + 
        geom_line(aes(y=.data[[yvar[1]]], col="Forecast"), linewidth=1.1) + 
        geom_line(aes(y=.data[[yvar[4]]], col="Actual"), linewidth=1.1) + 
        scale_fill_manual(values=c("Forecast"="#dbc5e2")) + 
        scale_color_manual(values=c("Forecast"="#A56EB6", "Actual"="black")) + 
        guides(fill = "none") + 
        theme_bw() + 
        labs(x=NULL, y=NULL, col=NULL, title="VAR forecast and range", subtitle=asat, linetype=NULL) } 

library(shiny)
ui <- fluidPage(
    tags$head(tags$style(HTML("body { font-family: 'Georgia'; }"))),
    titlePanel("VAR Forecast Model"), 
    tabsetPanel(
    tabPanel(title="VAR",
        br(),
        sidebarLayout(
            sidebarPanel(
            sliderInput("nahead", "Forecast Horizon", min=1, max=40, value=20, ticks=FALSE),
            sliderInput("plags", "VAR Model lags", min=1, max=10, value=2, ticks=FALSE), 
            sliderInput("ci", "Forecast range", min=0.5, max=0.999, value=0.95, ticks=FALSE), 
            selectInput("start", label="Forecast Date", choices=macrodata$period)
            ),
        mainPanel( 
            h4("Forecast of Rates"),
            plotOutput("forecastchgs", height="700px"),
            h4("Forecast of Cumulative Growth"),
            plotOutput("forecastlevels", height="400px")
            )
        )),
    tabPanel(title="DATA", 
        h4("Model Fitting Sample"), 
        p("The initial VAR model fitting sample has been selected to cover the period after inflation 
          was stabilitised following the 1991 recession, up until the quarter before the COVID 19 panic in 
          2020. You can shorten or length the data sample as required."),
        fluidRow(
            column(3, selectInput("fitstart", label="VAR model data start", 
                                        choices=macrodata$period, selected=yearquarter("1994 Q1"))),
            column(3, selectInput("fitend", label="VAR model data end", 
                                        choices=macrodata$period, selected=yearquarter("2019 Q4")))
            ),
        plotOutput("fitsample", height="700px")
        )
    )
)

server <- function(input, output) { 
    vardata <- reactive({ 
        varmod <- fit_varmodel(macrodata, macrovar, 
                               lags=input$plags, 
                               start=input$fitstart, 
                               end=input$fitend) 
        vardata <- build_macroscenario(varmod, macrodata, 
                                       start=input$start, 
                                       n.ahead=input$nahead, ci=input$ci)
        })
    output$fitsample <- renderPlot({ 
        ggplot_sample(macrodata, macrovar, input$fitstart, input$fitend) })
    output$forecastchgs <- renderPlot({ 
        ggplot_vardata(vardata())
        })
    output$forecastlevels <- renderPlot({ 
        lvar <- c("HousePriceGrowth", "LabourEarn")
        vardata() %>% 
            filter(ecovar %in% lvar) %>% 
            ggplot_vardata(., type="cum")
        })
    } 
shinyApp(ui=ui, server=server)
