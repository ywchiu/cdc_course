
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(scales)
library(lubridate)

library(timetk)
library(tidymodels)
library(modeltime)
library(recipes)
library(DBI)

predictSeries <- function(data, Model_Type = c('ARIMA'), Predict_Length = 30){

    #  取得每日新增個案數量
    data$Daily_Case <- c(0, diff(data$Case_Number))

    # 切分成訓練與測試資料集:(測試資料集：最後30 天的資料)
    splits <- data %>%
        time_series_split(assess = "30 days", cumulative = TRUE)

    models <- c()
    # arima
    if ('ARIMA' %in% Model_Type){
    model_fit_arima <- arima_reg() %>%
        set_engine("auto_arima") %>%
        fit(Daily_Case ~ Date, training(splits))
    models <- c(models,model_fit_arima)
    }

    # prophet
    if ('PROPHET' %in% Model_Type){
    model_fit_prophet <- prophet_reg() %>%
        set_engine("prophet", yearly.seasonality = TRUE) %>%
        fit(Daily_Case ~ Date, training(splits))
    models <- c(models,model_fit_prophet)
    }

    # create features
    recipe_spec <- recipe(Daily_Case ~ Date, training(splits)) %>%
        step_timeseries_signature(Date) %>%
        step_rm(contains("am.pm"),
                contains("year"),
                contains("hour"),
                contains("minute"),
                contains("second"),
                contains("xts"),
                'Date_index.num') %>%
        step_fourier(Date, period = 365, K = 7) %>%
        step_dummy(all_nominal())

    df <- recipe_spec %>%
        prep() %>%
        juice()


    # random forest
    if ('RANDOMFOREST' %in% Model_Type){
    model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
        set_engine("randomForest")

    workflow_fit_rf <- workflow() %>%
        add_model(model_spec_rf) %>%
        add_recipe(recipe_spec %>% step_rm(Date)) %>%
        fit(training(splits))
    models <- c(models,workflow_fit_rf)
    }

    #  模型表

    model_table <- modeltime_table(
        models
    )

    # 評估測試資料集
    calibration_table <- model_table %>%
        modeltime_calibrate(testing(splits))

    # 產生三個月的預測
    ret <- calibration_table %>%
        modeltime_refit(data) %>%
        modeltime_forecast(h = sprintf("%d days", Predict_Length), actual_data = data)

    ret
}

predictSeries <- function(data, Model_Type = c('ARIMA'), Predict_Length = 30){

    #  取得每日新增個案數量
    data$Daily_Case <- c(0, diff(data$Case_Number))

    # 切分成訓練與測試資料集:(測試資料集：最後30 天的資料)
    splits <- data %>%
        time_series_split(assess = "30 days", cumulative = TRUE)

    # create features
    recipe_spec <- recipe(Daily_Case ~ Date, training(splits)) %>%
        step_timeseries_signature(Date) %>%
        step_rm(contains("am.pm"), contains("year"), contains("hour"), contains("minute"), contains("second"), contains("xts"), 'Date_index.num') %>%
        step_fourier(Date, period = 365, K = 7)  %>%
        step_dummy(all_nominal())

    df <-  recipe_spec %>% prep() %>% juice()

    model_table <- modeltime_table()
    # arima
    if ('ARIMA' %in% Model_Type){
        model_fit_arima <- arima_reg() %>%
            set_engine("auto_arima") %>%
            fit(Daily_Case ~ Date, training(splits))
        model_table <- modeltime_table(model_fit_arima)
        #add_modeltime_model(model_table,model_fit_arima)
    }

    # prophet
    if ('PROPHET' %in% Model_Type){
        model_fit_prophet <- prophet_reg() %>%
            set_engine("prophet", yearly.seasonality = TRUE) %>%
            fit(Daily_Case ~ Date, training(splits))
        model_table <- modeltime_table(model_fit_prophet)
        #add_modeltime_model(model_table,model_fit_prophet)

    }

    # create features
    recipe_spec <- recipe(Daily_Case ~ Date, training(splits)) %>%
        step_timeseries_signature(Date) %>%
        step_rm(contains("am.pm"),
                contains("year"),
                contains("hour"),
                contains("minute"),
                contains("second"),
                contains("xts"),
                'Date_index.num') %>%
        step_fourier(Date, period = 365, K = 7) %>%
        step_dummy(all_nominal())

    df <- recipe_spec %>%
        prep() %>%
        juice()


    # random forest
    if ('RANDOMFOREST' %in% Model_Type){
        model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
            set_engine("randomForest")

        workflow_fit_rf <- workflow() %>%
            add_model(model_spec_rf) %>%
            add_recipe(recipe_spec %>% step_rm(Date)) %>%
            fit(training(splits))
        #models$randomforest <-workflow_fit_rf
        model_table <- modeltime_table(workflow_fit_rf)
        #add_modeltime_model(model_table,workflow_fit_rf)
    }

    # Model Table
    #model_table <- modeltime_table(

    #)

    # 套用到測試資料集產生結果
    calibration_table <- model_table %>%
        modeltime_calibrate(testing(splits))

    # 產生三個月的預測
    ret <- calibration_table %>%
        modeltime_refit(data) %>%
        modeltime_forecast(h = sprintf("%d days", Predict_Length), actual_data = data)
    ret
}

read_sql_query <- function(db, sql){
    con <- dbConnect(RSQLite::SQLite(), dbname = db)
    df <- dbGetQuery(conn = con, statement = sql)
    dbDisconnect(con)
    df
}

shinyServer(function(input, output, session) {
    ## 讀取資料
    covid19 <- read_sql_query('demo.db',
            "SELECT * FROM covid19")

    ## 地圖呈現 (Leaflet)
    output$covidmap <- renderLeaflet({
        # SELECT * FROM covid19 WHERE Case = 'confirmed' AND Date = '2020-10-13'
        print(input$Case_Type)
        print(input$timeSlider)
        confirmed <- read_sql_query('demo.db',
                sprintf("SELECT * FROM covid19 WHERE `Case` = '%s' AND `Date` = '%s'", input$Case_Type, input$timeSlider))

        map <- leaflet(confirmed) %>%

            addTiles() %>%
            addCircleMarkers(
                lat = confirmed$Lat,
                lng = confirmed$Long,
                radius = log(confirmed$Case_Number),
                stroke = FALSE,
                fillOpacity = 0.5
            )
        map
    })

    # 計算確診人數
    output$confirmed_all <- renderValueBox({
        case_stat <- read_sql_query('demo.db',
                sprintf("SELECT sum(Case_Number)  as Case_Sum  FROM covid19 WHERE `Case` = '%s' AND `Date` = '%s'", 'confirmed', input$timeSlider))

        print(case_stat)
        valueBox(
            case_stat$Case_Sum,
            color="orange",
            "確診人數",
            icon = icon("procedures")
        )
    })

    # 計算死亡人數
    output$deaths_all <- renderValueBox({
        case_stat <- read_sql_query('demo.db',
                    sprintf("SELECT sum(Case_Number)  as Case_Sum FROM covid19 WHERE `Case` = '%s' AND `Date` = '%s'", 'deaths', input$timeSlider))

        valueBox(
            case_stat$Case_Sum,
            color="red",
            "死亡人數",
            icon = icon("skull-crossbones")
        )
    })

    # 計算康復人數
    output$recovered_all <- renderValueBox({
        case_stat <- read_sql_query('demo.db',
                                    sprintf("SELECT sum(Case_Number)  as Case_Sum FROM covid19 WHERE `Case` = '%s' AND `Date` = '%s'", 'recovered', input$timeSlider))

        valueBox(
            case_stat$Case_Sum,
            color='green',
            "康復人數",
            icon = icon("first-aid")
        )
    })

    # 確診排行
    output$case_ranking <-renderPlotly({
        # SELECT Country/Region , SUM(Case_Number) as Case_Sum covid19 FROM covid19 GROUP BY Country/Region ORDER BY Case_Sum DESC LIMIT 10
        case_stat <- read_sql_query('demo.db',
               sprintf("SELECT `Country/Region` , SUM(Case_Number) AS Case_Number FROM covid19 WHERE `Case` = '%s' AND `Date` = '%s' GROUP BY `Country/Region` ORDER BY Case_Number  DESC LIMIT 10", input$Case_Type, input$timeSlider))


        fig <- ggplot(case_stat)
        g <- fig +
            geom_bar(aes(x=reorder(`Country/Region`, Case_Number),y=Case_Number),stat="identity", fill= 'blue') +
            coord_flip() +
            ylab('Case Numbers') +
            xlab('Country') +
            scale_y_continuous(labels = scales::comma)
        ggplotly(g)
    })

    # 更新國家選項
    observeEvent(input$Case_Type, {
        updateSelectInput(session, "Country", choices = unique(covid19$`Country/Region`))
        updateSelectInput(session, "Country2", choices = unique(covid19$`Country/Region`))
    })

    output$table <- renderDataTable({
        data <- read_sql_query('demo.db',
                sprintf("SELECT Date, Case_Number FROM covid19 WHERE `Case` = '%s' AND `Country/Region` = '%s' ORDER BY Date DESC", input$Case_Type, input$Country))

        data
    })

    # 產生預測結果
    output$predictPlot <- renderPlotly({

        ## Input Data
        data <- covid19 %>%
            filter((`Case` == input$Case_Type) &(`Country/Region` == input$Country)) %>%
            group_by(Date) %>%
            summarize(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)

        ret <- predictSeries(data)

        ret %>%
            plot_modeltime_forecast(.interactive = TRUE,.title="預測結果", .y_lab = "New daily cases",.legend_show = TRUE)
    })

    # 產生預測結果
    output$predictPlot2 <- renderPlotly({

        ## Input Data
        data <- covid19 %>%
            filter((`Case` == input$Case_Type2) & (`Country/Region` == input$Country2)) %>%
            group_by(Date) %>%
            summarize(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)
        print(input$Model_Type)
        print(input$Predict_Length)
        if (!is.null(input$Model_Type)){
            ret <- predictSeries(data, Model_Type = input$Model_Type, Predict_Length = input$Predict_Length)

            ret %>%
                plot_modeltime_forecast(.interactive = TRUE,.title="預測結果", .y_lab = "New daily cases",.legend_show = TRUE)
        }
    })
    ## 個案類型折線圖
    output$distPlot <- renderPlotly({

        data <- read_sql_query('demo.db',
               sprintf("SELECT Date, Case_Number FROM covid19 WHERE `Case` = '%s' AND `Country/Region` = '%s'", input$Case_Type, input$Country))

        fig <- ggplot(data)

        g <- fig +
            aes(x = Date, y = Case_Number) +
            geom_line(color='steelblue') +
            geom_point(color="blue", size=0.3)

        ggplotly(g)
    })

    # 繪製新增數量 (Bar)
    output$dailyNewPlot <- renderPlotly({
        data <- covid19 %>%
            filter((`Case` == input$Case_Type) &(`Country/Region` == input$Country)) %>%
            group_by(Date) %>%
            summarize(Case_Number = sum(Case_Number)) %>%
            select(Date, Case_Number)

        data$Daily_Number <- c(0, diff(data$Case_Number))

        fig <- ggplot(data)
        g <- fig +
            aes(x = Date, y = Daily_Number) +
            geom_bar(stat="identity", fill = "#FF6666")

        ggplotly(g)
    })

})
