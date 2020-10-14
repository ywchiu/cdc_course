library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)

shinyServer(function(input, output, session) {
    covid <- read_csv('https://raw.githubusercontent.com/ywchiu/cdc_course/master/2020Exam/covid19.csv')

    output$accumPlot <- renderPlotly({
        # 題目一：請根據輸入個案類型與輸入國家產生累積個案折線趨勢圖(X 軸為時間，Y 為累積個案數量)
    })

    output$dailyNewPlot <- renderPlotly({
        # 題目二：請根據輸入個案類型與輸入國家產生每日新增個案長條圖(X 軸為時間，Y 為每日個案增加數量)
    })

    output$table <- renderDataTable({
        # 題目三：請根據輸入個案類型與輸入國家產生每日累積個案表格(至少包含欄位：日期、國家、累積個案數量)，表格必須根據日期排序(由新至舊)
    })

    observeEvent(input$Case_Type, {
        # 題目四：請產生所有國家組合，讓Shiny 介面選單上(輸入國家)可以選擇所有國家
    })

})
