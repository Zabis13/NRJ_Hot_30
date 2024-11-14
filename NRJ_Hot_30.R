library(shiny)
library(rvest)

# UI
ui <- fluidPage(
  titlePanel("Плей-лист Радио Energy"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("downloadBtn", "Скачать"),  # Кнопка для скачивания
      
      # Отображение времени выполнения
      uiOutput("timing_info")  # Новый блок для отображения времени выполнения
    ),
    
    mainPanel(
      uiOutput("result")  # Динамическое отображение результатов
    )
  )
)

# Server
server <- function(input, output) {
  
  # Реактивные переменные для хранения результата и времени выполнения
  result <- reactiveVal(NULL)
  timings <- reactiveVal(NULL)
  total_time <- reactiveVal(NULL)
  
  # Изначальное сообщение
  output$result <- renderUI({
    tags$p("Нажмите на кнопку для выполнения скрипта.")
  })
  
  # Обработчик нажатия кнопки
  observeEvent(input$downloadBtn, {
    # Запускаем общий таймер
    start_total_time <- Sys.time()
    
    # Этап 1: Загрузка HTML-контента страницы
    start_time <- Sys.time()
    url <- "https://www.energyfm.ru/hot-30"
    page <- read_html(url)
    end_time <- Sys.time()
    timing_download <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 1)
    
    # Этап 2: Нахождение div-контейнера
    start_time <- Sys.time()
    main_content <- page %>% html_node("div.hot-30-content__main")
    end_time <- Sys.time()
    timing_find_div <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 1)
    
    # Этап 3: Извлечение треков
    start_time <- Sys.time()
    songs <- main_content %>% 
      html_nodes("img.player__cover") %>%
      html_attr("alt") %>%
      gsub("Обложка трека '|'", "", .)
    end_time <- Sys.time()
    timing_extract_songs <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 1)
    
    # Этап 4: Сохранение результатов
    start_time <- Sys.time()
    result(songs[1:30])  # Сохраняем результат в реактивную переменную
    end_time <- Sys.time()
    timing_save_result <- round(as.numeric(difftime(end_time, start_time, units = "secs")) * 1000, 1)
    
    # Этап 5: Отрисовка результата на странице
    render_start <- Sys.time()  # Зафиксируем время начала отрисовки
    output$result <- renderUI({
      # Если нет результатов, показываем сообщение
      if (is.null(result()) || length(result()) == 0) {
        tags$p("Нажмите на кнопку для выполнения скрипта.")  # Текст до выполнения
      } else {
        # Если есть данные, выводим список песен
        lapply(seq_along(result()), function(i) {
          tags$p(paste(i, ".", result()[i]))
        })
      }
    })
    render_end <- Sys.time()  # Зафиксируем время окончания отрисовки
    timing_render <- round(as.numeric(difftime(render_end, render_start, units = "secs")) * 1000, 1)
    
    # Общее время выполнения
    end_total_time <- Sys.time()
    total_exec_time <- round(as.numeric(difftime(end_total_time, start_total_time, units = "secs")) * 1000, 1)
    total_exec_time_with_render <- total_exec_time + timing_render
    
    # Сохраняем тайминги
    timings(list(
      timing_download = timing_download,
      timing_find_div = timing_find_div,
      timing_extract_songs = timing_extract_songs,
      timing_save_result = timing_save_result,
      timing_render = timing_render
    ))
    total_time(total_exec_time_with_render)
  })
  
  # Вывод времени выполнения рядом с кнопкой
  output$timing_info <- renderUI({
    # Проверяем, есть ли результаты
    if (!is.null(total_time())) {
      tagList(
        tags$h4("Время выполнения (мс):"),
        tags$p(paste("Загрузка страницы:", timings()$timing_download, "мс")),
        tags$p(paste("Нахождение div-контейнера:", timings()$timing_find_div, "мс")),
        tags$p(paste("Извлечение треков:", timings()$timing_extract_songs, "мс")),
        tags$p(paste("Сохранение результатов:", timings()$timing_save_result, "мс")),
        tags$p(paste("Время отрисовки:", timings()$timing_render, "мс")),
        tags$p(tags$b(paste("Общее время выполнения:", total_time(), "мс")))
      )
    } else {
      tags$p("Время выполнения (мс)")
    }
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)
