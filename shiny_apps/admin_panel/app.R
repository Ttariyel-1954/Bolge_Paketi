# ============================================================================
# ADMIN PANEL - ONLAYN TEST SİSTEMİ + EXCEL YÜKLƏMƏ
# ============================================================================

library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(DT)
library(stringr)
library(readxl)

Sys.setenv(TZ = "Asia/Baku")

# ============================================================================
# PostgreSQL BAĞLANTISI
# ============================================================================
get_db_connection <- function() {
  tryCatch({
    DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = "onlayn_test_db",
      host = "localhost",
      port = 5432,
      user = "test_admin",
      password = "test12345",
      sslmode = "disable"
    )
  }, error = function(e) {
    stop("PostgreSQL bağlantı xətası: ", e$message)
  })
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        padding: 20px;
      }
      .main-container {
        background: white;
        border-radius: 15px;
        padding: 30px;
        box-shadow: 0 10px 40px rgba(0,0,0,0.3);
      }
      h1 {
        color: #667eea;
        text-align: center;
        margin-bottom: 30px;
        font-weight: bold;
      }
      h2 {
        color: #667eea;
        border-bottom: 3px solid #667eea;
        padding-bottom: 10px;
        margin-top: 30px;
      }
      .stat-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        text-align: center;
        margin: 10px 0;
        box-shadow: 0 5px 15px rgba(0,0,0,0.2);
      }
      .stat-box h3 {
        margin: 0;
        font-size: 2.5em;
        font-weight: bold;
      }
      .stat-box p {
        margin: 5px 0 0 0;
        font-size: 1.1em;
      }
      .selected-count {
        background: #28a745;
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-size: 1.3em;
        font-weight: bold;
        text-align: center;
        margin: 15px 0;
        box-shadow: 0 5px 15px rgba(40,167,69,0.3);
      }
      .btn-lg {
        font-size: 1.2em;
        padding: 12px;
      }
      .upload-box {
        border: 3px dashed #667eea;
        border-radius: 10px;
        padding: 30px;
        text-align: center;
        background: #f8f9ff;
        margin: 20px 0;
      }
    "))
  ),
  
  div(class = "main-container",
      h1("🎓 ADMİN PANELİ — TEST YARATMA SİSTEMİ"),
      
      tabsetPanel(
        # ================================================================
        # STATİSTİKA PANELİ
        # ================================================================
        tabPanel("📊 Statistika",
                 br(),
                 fluidRow(
                   column(4, div(class = "stat-box", 
                                 h3(textOutput("total_questions")), 
                                 p("Ümumi Sual"))),
                   column(4, div(class = "stat-box", 
                                 h3(textOutput("total_subjects")), 
                                 p("Fənn sayı"))),
                   column(4, div(class = "stat-box", 
                                 h3(textOutput("total_areas")), 
                                 p("Məzmun sahəsi sayı")))
                 ),
                 br(),
                 h3("📚 Fənn və məzmun sahəsinə görə sual sayı:"),
                 DTOutput("questions_stats"),
                 br(),
                 h3("📈 Çətinlik paylanması:"),
                 DTOutput("difficulty_stats")
        ),
        
        # ================================================================
        # YENİ: EXCEL YÜKLƏMƏ PANELİ
        # ================================================================
        tabPanel("📥 Excel-dən Yüklə",
                 br(),
                 h2("EXCEL FAYLINI YÜKLƏ"),
                 
                 div(class = "upload-box",
                     fileInput("excel_file", 
                               "📂 Excel faylını seçin (.xlsx):",
                               accept = c(".xlsx", ".xls"),
                               buttonLabel = "Fayl Seç",
                               placeholder = "Fayl seçilməyib")
                 ),
                 
                 hr(),
                 
                 h2("FİLTR SEÇİMLƏRİ (İxtiyari)"),
                 
                 fluidRow(
                   column(4, selectInput("excel_fenn_filter", 
                                         "🎯 Fənn:",
                                         choices = c("Hamısı" = ""))),
                   column(4, selectInput("excel_mezmun_filter",
                                         "📚 Məzmun Sahəsi:",
                                         choices = c("Hamısı" = ""))),
                   column(4, selectInput("excel_cetinlik_filter",
                                         "⭐ Çətinlik:",
                                         choices = c("Hamısı" = "",
                                                     "Asan (b < -1)" = "asan",
                                                     "Orta (-1 ≤ b ≤ 1)" = "orta",
                                                     "Çətin (b > 1)" = "cetin")))
                 ),
                 
                 hr(),
                 
                 h2("PREVIEW - YÜKLƏNƏCƏK SUALLAR"),
                 DTOutput("excel_preview"),
                 
                 br(),
                 
                 fluidRow(
                   column(6, 
                          selectInput("excel_mode",
                                      "📝 Yükləmə rejimi:",
                                      choices = c("Əlavə et (APPEND)" = "append",
                                                  "Əvəz et (REPLACE)" = "replace"),
                                      selected = "append")),
                   column(6, br(),
                          actionButton("excel_upload_btn",
                                       "📤 POSTGRESQL-Ə YÜKLƏ",
                                       class = "btn-success btn-lg btn-block",
                                       style = "margin-top: 5px;"))
                 ),
                 
                 br(),
                 
                 div(style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 5px solid #ffc107;",
                     tags$strong("⚠️ DİQQƏT:"),
                     tags$ul(
                       tags$li("REPLACE rejimi bazadakı BÜTÜN sualları siləcək!"),
                       tags$li("APPEND rejimi mövcud suallara əlavə edəcək."),
                       tags$li("Filtr seçsəniz, yalnız seçilmiş suallar yüklənəcək.")
                     ))
        ),
        
        # ================================================================
        # SUAL SEÇİMİ PANELİ (köhnə)
        # ================================================================
        tabPanel("✏️ Sual Seçimi (Manual)",
                 br(),
                 h2("ADDIM 1: FƏNN SEÇİN VƏ SUALLARI GÖRÜN"),
                 
                 fluidRow(
                   column(4, selectInput("select_subject", "Fənn seçin:", 
                                         choices = c())),
                   column(4, selectInput("filter_content_area", "📚 Məzmun sahəsi:", 
                                         choices = c())),
                   column(4, selectInput("filter_difficulty", "⭐Çətinlik:", 
                                         choices = c("Hamısı" = "", 
                                                     "Asan (b < -1)" = "asan", 
                                                     "Orta (-1 ≤ b ≤ 1)" = "orta", 
                                                     "Çətin (b > 1)" = "cetin")))
                 ),
                 
                 br(),
                 DTOutput("available_questions"),
                 hr(),
                 
                 h2("ADDIM 2: SUAL ID-LƏRİNİ DAXİL EDİN"),
                 p("💡 İpucu: Yuxarıdakı cədvəldən question_id sütununa baxıb ID-ləri yazın."),
                 fluidRow(
                   column(8, textAreaInput("question_ids_input",
                                           "Sual ID-lərini vergüllə ayırın:", 
                                           placeholder = "Məsələn: 1, 5, 10, 15, 20", 
                                           rows = 3, width = "100%")),
                   column(4, br(), actionButton("add_questions", "➕ ƏLAVƏ ET", 
                                                class = "btn-success btn-lg btn-block", 
                                                style = "margin-top: 10px;"))
                 ),
                 
                 hr(),
                 h2("ADDIM 3: SEÇİLMİŞ SUALLAR"),
                 
                 fluidRow(
                   column(6, textInput("test_adi_input", 
                                       "📝 Test Adı:",
                                       placeholder = "Məsələn: Fevral 2025",
                                       width = "100%")),
                   column(6, textInput("test_kodu_input",
                                       "🔢 Test Kodu:",
                                       placeholder = "Məsələn: TEST_FEB_2025",
                                       width = "100%"))
                 ),
                 
                 fluidRow(
                   column(6, dateInput("test_tarixi_input",
                                       "📅 Test Tarixi:",
                                       value = Sys.Date(),
                                       width = "100%")),
                   column(6, numericInput("test_muddeti_input",
                                          "⏱️ Müddət (dəqiqə):",
                                          value = 90,
                                          min = 30,
                                          max = 240,
                                          width = "100%"))
                 ),
                 br(),
                 
                 div(class = "selected-count", textOutput("selected_count_text")),
                 DTOutput("selected_questions_table"),
                 
                 br(),
                 fluidRow(
                   column(4, actionButton("clear_selection", "🗑️ TƏMİZLƏ", 
                                          class = "btn-warning btn-lg btn-block")),
                   column(4, actionButton("remove_selected_rows", "❌ SİL", 
                                          class = "btn-danger btn-lg btn-block")),
                   column(4, actionButton("export_selected", "📤 TEST YARAT", 
                                          class = "btn-primary btn-lg btn-block"))
                 )
        ),
        
        # ================================================================
        # MÖVCUD TESTLƏR (köhnə)
        # ================================================================
        tabPanel("📋 Mövcud Testlər",
                 br(),
                 h2("Yaradılmış Testlər"),
                 DTOutput("existing_tests_table"),
                 br(),
                 fluidRow(
                   column(6, actionButton("refresh_tests", "🔄 Yenilə", 
                                          class = "btn-info btn-lg btn-block")),
                   column(6, actionButton("delete_tests", "🗑️ Sil", 
                                          class = "btn-danger btn-lg btn-block"))
                 )
        )
      )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  selected_questions_rv <- reactiveVal(data.frame())
  tests_to_delete <- reactiveVal(NULL)
  excel_data_rv <- reactiveVal(NULL)
  
  # ========== YENİ: EXCEL YÜKLƏMƏ LOGİKASI ==========
  
  # Excel faylı oxu
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    tryCatch({
      # Excel-i oxu
      df <- read_excel(input$excel_file$datapath)
      
      # Sütun adlarını yoxla
      required_cols <- c("sual_id", "fenn", "mezmun_sahesi", "sual_metni",
                        "variant_A", "variant_B", "variant_C", "variant_D", "variant_E",
                        "dogru_cavab", "cetinlik", "ayirdetme", "texminetme", "fisher_info")
      
      if (!all(required_cols %in% colnames(df))) {
        showNotification("❌ Excel formatı səhvdir! Lazımi sütunlar yoxdur.", 
                        type = "error", duration = 10)
        return()
      }
      
      # Sütun adlarını düzəlt
      df <- df %>%
        rename(
          variant_a = variant_A,
          variant_b = variant_B,
          variant_c = variant_C,
          variant_d = variant_D,
          variant_e = variant_E
        ) %>%
        mutate(
          sekil_var = ifelse(is.na(sekil_var), FALSE, sekil_var),
          sekil_yolu = ifelse(is.na(sekil_yolu) | sekil_yolu == "", NA, sekil_yolu),
          muellif = ifelse(is.na(muellif), "", muellif),
          qeydler = ifelse(is.na(qeydler), "", qeydler)
        )
      
      excel_data_rv(df)
      
      # Fənn filterini yenilə
      fennler <- unique(df$fenn)
      updateSelectInput(session, "excel_fenn_filter", 
                       choices = c("Hamısı" = "", fennler))
      
      showNotification(paste0("✅ ", nrow(df), " sual oxundu!"), 
                      type = "message")
      
    }, error = function(e) {
      showNotification(paste0("❌ Xəta: ", e$message), 
                      type = "error", duration = 10)
    })
  })
  
  # Məzmun sahəsi filterini yenilə
  observe({
    req(excel_data_rv())
    req(input$excel_fenn_filter)
    
    if (nchar(input$excel_fenn_filter) > 0) {
      saheler <- excel_data_rv() %>%
        filter(fenn == input$excel_fenn_filter) %>%
        pull(mezmun_sahesi) %>%
        unique()
      
      updateSelectInput(session, "excel_mezmun_filter",
                       choices = c("Hamısı" = "", saheler))
    } else {
      saheler <- unique(excel_data_rv()$mezmun_sahesi)
      updateSelectInput(session, "excel_mezmun_filter",
                       choices = c("Hamısı" = "", saheler))
    }
  })
  
  # Preview cədvəli
  output$excel_preview <- DT::renderDT({
    req(excel_data_rv())
    
    df <- excel_data_rv()
    
    # Filterləri tətbiq et
    if (!is.null(input$excel_fenn_filter) && nchar(input$excel_fenn_filter) > 0) {
      df <- df %>% filter(fenn == input$excel_fenn_filter)
    }
    
    if (!is.null(input$excel_mezmun_filter) && nchar(input$excel_mezmun_filter) > 0) {
      df <- df %>% filter(mezmun_sahesi == input$excel_mezmun_filter)
    }
    
    if (!is.null(input$excel_cetinlik_filter) && nchar(input$excel_cetinlik_filter) > 0) {
      if (input$excel_cetinlik_filter == "asan") {
        df <- df %>% filter(cetinlik < -1)
      } else if (input$excel_cetinlik_filter == "orta") {
        df <- df %>% filter(cetinlik >= -1 & cetinlik <= 1)
      } else if (input$excel_cetinlik_filter == "cetin") {
        df <- df %>% filter(cetinlik > 1)
      }
    }
    
    # Göstər
    display_df <- df %>%
      select(sual_id, fenn, mezmun_sahesi, sual_metni, dogru_cavab, cetinlik, ayirdetme) %>%
      mutate(
        sual_metni = substr(sual_metni, 1, 60),
        cetinlik = round(cetinlik, 2),
        ayirdetme = round(ayirdetme, 2)
      )
    
    DT::datatable(
      display_df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      caption = paste0("TOPLAM: ", nrow(df), " sual")
    )
  })
  
  # PostgreSQL-ə yüklə
  observeEvent(input$excel_upload_btn, {
    req(excel_data_rv())
    
    df <- excel_data_rv()
    
    # Filterləri tətbiq et
    if (!is.null(input$excel_fenn_filter) && nchar(input$excel_fenn_filter) > 0) {
      df <- df %>% filter(fenn == input$excel_fenn_filter)
    }
    
    if (!is.null(input$excel_mezmun_filter) && nchar(input$excel_mezmun_filter) > 0) {
      df <- df %>% filter(mezmun_sahesi == input$excel_mezmun_filter)
    }
    
    if (!is.null(input$excel_cetinlik_filter) && nchar(input$excel_cetinlik_filter) > 0) {
      if (input$excel_cetinlik_filter == "asan") {
        df <- df %>% filter(cetinlik < -1)
      } else if (input$excel_cetinlik_filter == "orta") {
        df <- df %>% filter(cetinlik >= -1 & cetinlik <= 1)
      } else if (input$excel_cetinlik_filter == "cetin") {
        df <- df %>% filter(cetinlik > 1)
      }
    }
    
    if (nrow(df) == 0) {
      showNotification("❌ Yükləmək üçün sual yoxdur!", type = "error")
      return()
    }
    
    # Təsdiqləmə modal
    showModal(modalDialog(
      title = "⚠️ TƏSDİQLƏYİN",
      h4(paste0(nrow(df), " sual yüklənəcək")),
      h4(paste0("Rejim: ", ifelse(input$excel_mode == "append", "ƏLAVƏ ET", "ƏVƏZ ET"))),
      hr(),
      if (input$excel_mode == "replace") {
        tags$div(style = "background: #f8d7da; padding: 15px; border-radius: 5px; color: #721c24;",
                tags$strong("DİQQƏT: Bazadakı BÜTÜN suallar silinəcək!"))
      },
      footer = tagList(
        modalButton("❌ Xeyr"),
        actionButton("confirm_excel_upload", "✅ Bəli, Yüklə", class = "btn-success")
      )
    ))
  })
  
  # Təsdiqlənmiş yükləmə
  observeEvent(input$confirm_excel_upload, {
    req(excel_data_rv())
    
    df <- excel_data_rv()
    
    # Filterləri tətbiq et (eyni kod)
    if (!is.null(input$excel_fenn_filter) && nchar(input$excel_fenn_filter) > 0) {
      df <- df %>% filter(fenn == input$excel_fenn_filter)
    }
    if (!is.null(input$excel_mezmun_filter) && nchar(input$excel_mezmun_filter) > 0) {
      df <- df %>% filter(mezmun_sahesi == input$excel_mezmun_filter)
    }
    if (!is.null(input$excel_cetinlik_filter) && nchar(input$excel_cetinlik_filter) > 0) {
      if (input$excel_cetinlik_filter == "asan") {
        df <- df %>% filter(cetinlik < -1)
      } else if (input$excel_cetinlik_filter == "orta") {
        df <- df %>% filter(cetinlik >= -1 & cetinlik <= 1)
      } else if (input$excel_cetinlik_filter == "cetin") {
        df <- df %>% filter(cetinlik > 1)
      }
    }
    
    tryCatch({
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      # REPLACE rejimi
      if (input$excel_mode == "replace") {
        dbExecute(con, "DELETE FROM questions")
      }
      
      # Yüklə
      dbWriteTable(con, "questions", df, append = TRUE, row.names = FALSE)
      
      removeModal()
      showNotification(paste0("✅ ", nrow(df), " sual uğurla yükləndi!"), 
                      type = "message", duration = 5)
      
      # Excel məlumatını təmizlə
      excel_data_rv(NULL)
      updateSelectInput(session, "excel_fenn_filter", selected = "")
      updateSelectInput(session, "excel_mezmun_filter", selected = "")
      updateSelectInput(session, "excel_cetinlik_filter", selected = "")
      
    }, error = function(e) {
      removeModal()
      showNotification(paste0("❌ Xəta: ", e$message), 
                      type = "error", duration = 10)
    })
  })
  
  # ========== KÖHNƏ FUNKSIYALAR (dəyişməz) ==========
  
  update_available_tests <- function() {
    tryCatch({
      con <- get_db_connection()
      available_tests <- dbGetQuery(con, "
        SELECT test_id, test_kodu, test_adi, test_tarixi, test_muddeti,
               umumi_sual_sayi, riyaziyyat_sayi, azerbaycan_sayi, ingilis_sayi, status
        FROM tests
        WHERE status = 'hazırlanır' OR status = 'aktiv'
        ORDER BY test_id DESC;
      ")
      dbDisconnect(con)
      
      if (nrow(available_tests) > 0) {
        available_tests <- available_tests %>%
          filter(sapply(test_kodu, function(tk) {
            file.exists(file.path("../../data/tests", paste0(tk, ".rds")))
          }))
      }
      
      test_list <- list(
        last_updated = Sys.time(),
        total_tests = nrow(available_tests),
        tests = available_tests
      )
      
      saveRDS(test_list, file = "../../data/tests/available_tests.rds")
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  output$total_questions <- renderText({
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    format(dbGetQuery(con, "SELECT COUNT(*) AS n FROM questions")$n, big.mark = " ")
  })
  
  output$total_subjects <- renderText({
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    format(dbGetQuery(con, "SELECT COUNT(DISTINCT fenn) AS n FROM questions")$n, big.mark = " ")
  })
  
  output$total_areas <- renderText({
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    format(dbGetQuery(con, "SELECT COUNT(DISTINCT mezmun_sahesi) AS n FROM questions")$n, big.mark = " ")
  })
  
  output$questions_stats <- DT::renderDT({
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    data <- dbGetQuery(con, "
      SELECT fenn AS \"Fənn\", mezmun_sahesi AS \"Məzmun Sahəsi\",
             COUNT(*) AS \"Sual Sayı\"
      FROM questions
      GROUP BY fenn, mezmun_sahesi
      ORDER BY fenn, mezmun_sahesi;
    ")
    DT::datatable(data, options = list(pageLength = 20, dom = 't'), rownames = FALSE)
  })
  
  output$difficulty_stats <- DT::renderDT({
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    data <- dbGetQuery(con, "
      WITH categorized AS (
        SELECT 
          CASE 
            WHEN cetinlik < -1 THEN 'Asan (b < -1)'
            WHEN cetinlik >= -1 AND cetinlik <= 1 THEN 'Orta (-1 ≤ b ≤ 1)'
            ELSE 'Çətin (b > 1)'
          END AS seviyye,
          cetinlik
        FROM questions
      )
      SELECT seviyye AS \"Çətinlik\",
             COUNT(*) AS \"Say\",
             ROUND(AVG(cetinlik)::numeric, 2) AS \"Orta b\"
      FROM categorized
      GROUP BY seviyye
      ORDER BY CASE seviyye
        WHEN 'Asan (b < -1)' THEN 1
        WHEN 'Orta (-1 ≤ b ≤ 1)' THEN 2
        ELSE 3 END;
    ")
    DT::datatable(data, options = list(pageLength = 5, dom = 't'), rownames = FALSE)
  })
  
  observe({
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    subjects <- dbGetQuery(con, "SELECT DISTINCT fenn FROM questions ORDER BY fenn;")$fenn
    updateSelectInput(session, "select_subject", choices = subjects)
  })
  
  observe({
    req(input$select_subject)
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    areas <- dbGetQuery(con, "
      SELECT DISTINCT mezmun_sahesi FROM questions
      WHERE fenn = $1 ORDER BY mezmun_sahesi;
    ", params = list(input$select_subject))$mezmun_sahesi
    updateSelectInput(session, "filter_content_area", choices = c("Hamısı" = "", areas))
  })
  
  output$available_questions <- DT::renderDT({
    req(input$select_subject)
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    base_q <- "
      SELECT question_id, sual_id, fenn, mezmun_sahesi,
             LEFT(sual_metni, 100) AS sual_metni_qisa,
             dogru_cavab, ROUND(cetinlik::numeric, 2) AS cetinlik,
             ROUND(ayirdetme::numeric, 2) AS ayirdetme,
             CASE 
               WHEN cetinlik < -1 THEN 'Asan'
               WHEN cetinlik >= -1 AND cetinlik <= 1 THEN 'Orta'
               ELSE 'Çətin'
             END AS seviyye
      FROM questions WHERE fenn = $1
    "
    params <- list(input$select_subject)
    
    if (!is.null(input$filter_content_area) && nchar(input$filter_content_area)) {
      base_q <- paste0(base_q, " AND mezmun_sahesi = $2")
      params <- c(params, input$filter_content_area)
    }
    
    if (!is.null(input$filter_difficulty) && nchar(input$filter_difficulty)) {
      if (input$filter_difficulty == "asan") {
        base_q <- paste0(base_q, " AND cetinlik < -1")
      } else if (input$filter_difficulty == "orta") {
        base_q <- paste0(base_q, " AND cetinlik >= -1 AND cetinlik <= 1")
      } else if (input$filter_difficulty == "cetin") {
        base_q <- paste0(base_q, " AND cetinlik > 1")
      }
    }
    
    sql <- paste0(base_q, " ORDER BY question_id;")
    data <- dbGetQuery(con, sql, params = params)
    
    DT::datatable(data, 
                  colnames = c("ID", "Sual ID", "Fənn", "Məzmun", "Sual", 
                              "Düzgün", "Çətinlik", "Ayırdedmə", "Səviyyə"),
                  options = list(pageLength = 10, scrollX = TRUE), 
                  rownames = FALSE)
  })
  
  observeEvent(input$add_questions, {
    req(input$question_ids_input)
    
    ids <- gsub("\\s+", "", input$question_ids_input)
    ids <- as.integer(unlist(strsplit(ids, ",")))
    ids <- ids[!is.na(ids)]
    
    if (!length(ids)) {
      showNotification("❌ Düzgün ID daxil edin!", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    new_questions <- dbGetQuery(con, sprintf("
      SELECT question_id, sual_id, fenn, mezmun_sahesi, sual_metni, 
             variant_a, variant_b, variant_c, variant_d, variant_e,
             dogru_cavab, cetinlik, ayirdetme, texminetme, fisher_info,
             sekil_var, sekil_yolu, muellif, qeydler
      FROM questions WHERE question_id IN (%s)
      ORDER BY question_id;", paste(ids, collapse = ",")))
    
    if (!nrow(new_questions)) {
      showNotification("❌ Sual tapılmadı!", type = "error")
      return()
    }
    
    cur <- selected_questions_rv()
    new_questions <- new_questions[!(new_questions$question_id %in% cur$question_id), ]
    
    if (nrow(new_questions) == 0) {
      showNotification("⚠️ Bu suallar artıq əlavə edilib!", type = "warning")
      return()
    }
    
    selected_questions_rv(rbind(cur, new_questions))
    showNotification(paste0("✅ ", nrow(new_questions), " sual əlavə edildi!"), type = "message")
    updateTextAreaInput(session, "question_ids_input", value = "")
  })
  
  output$selected_count_text <- renderText({
    d <- selected_questions_rv()
    if (is.null(d) || !nrow(d)) return("❌ Hələ sual seçilməyib")
    
    stats <- d %>% group_by(fenn) %>% summarise(say = n()) %>%
      mutate(text = paste0(fenn, ": ", say))
    
    paste0("✅ TOPLAM ", nrow(d), " SUAL | ", paste(stats$text, collapse = " | "))
  })
  
  output$selected_questions_table <- DT::renderDT({
    d <- selected_questions_rv()
    if (is.null(d) || !nrow(d)) {
      return(data.frame(Mesaj = "Hələ sual seçilməyib"))
    }
    
    display_data <- d %>%
      select(question_id, sual_id, fenn, mezmun_sahesi, 
             sual_metni, dogru_cavab, cetinlik, ayirdetme) %>%
      mutate(sual_metni = substr(sual_metni, 1, 80),
             cetinlik = round(cetinlik, 2),
             ayirdetme = round(ayirdetme, 2))
    
    DT::datatable(display_data,
                  colnames = c("ID", "Sual ID", "Fənn", "Məzmun", "Sual", 
                              "Düzgün", "Çətinlik", "Ayırdedmə"),
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE, selection = 'multiple')
  })
  
  observeEvent(input$clear_selection, {
    selected_questions_rv(data.frame())
    showNotification("🗑️ Seçim təmizləndi", type = "message")
  })
  
  observeEvent(input$remove_selected_rows, {
    d <- selected_questions_rv()
    selected_rows <- input$selected_questions_table_rows_selected
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("⚠️ Əvvəlcə sual seçin!", type = "warning")
      return()
    }
    
    d <- d[-selected_rows, ]
    selected_questions_rv(d)
    showNotification(paste0("✅ ", length(selected_rows), " sual silindi"), type = "message")
  })
  
  observeEvent(input$export_selected, {
    d <- selected_questions_rv()
    if (is.null(d) || !nrow(d)) {
      showNotification("❌ Sual seçilməyib!", type = "error")
      return()
    }
    
    test_adi <- if (!is.null(input$test_adi_input) && nchar(input$test_adi_input)) {
      input$test_adi_input
    } else {
      paste0("Test_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    
    test_kodu <- if (!is.null(input$test_kodu_input) && nchar(input$test_kodu_input)) {
      input$test_kodu_input
    } else {
      paste0("TEST_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    
    test_tarixi <- if (!is.null(input$test_tarixi_input)) {
      as.character(input$test_tarixi_input)
    } else {
      as.character(Sys.Date())
    }
    
    test_muddeti <- if (!is.null(input$test_muddeti_input)) {
      as.integer(input$test_muddeti_input)
    } else {
      90L
    }
    
    export_data <- list(
      test_info = list(
        test_adi = test_adi,
        test_kodu = test_kodu,
        test_tarixi = test_tarixi,
        test_muddeti = test_muddeti,
        yaradilma_tarixi = Sys.time(),
        umumi_sual_sayi = nrow(d),
        fenn_paylanmasi = d %>% count(fenn)
      ),
      questions = d
    )
    
    if (!dir.exists("../../data/tests")) {
      dir.create("../../data/tests", recursive = TRUE)
    }
    
    export_path <- file.path("../../data/tests", paste0(test_kodu, ".rds"))
    saveRDS(export_data, file = export_path)
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    tryCatch({
      test_id <- dbGetQuery(con, "
        INSERT INTO tests (test_adi, test_kodu, test_tarixi, test_muddeti, 
                          umumi_sual_sayi, riyaziyyat_sayi, azerbaycan_sayi, 
                          ingilis_sayi, status)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, 'hazırlanır')
        RETURNING test_id;
      ", params = list(
        test_adi, test_kodu, test_tarixi, test_muddeti,
        nrow(d),
        sum(d$fenn == "Riyaziyyat"),
        sum(d$fenn == "Azərbaycan dili"),
        sum(d$fenn == "İngilis dili")
      ))$test_id
      
      for (i in 1:nrow(d)) {
        dbExecute(con, "
          INSERT INTO test_questions (test_id, question_id, sira_nomresi)
          VALUES ($1, $2, $3);
        ", params = list(test_id, d$question_id[i], i))
      }
      
      showModal(modalDialog(
        title = "✅ TEST YARADILDI!",
        h4(paste0("Test ID: ", test_id)),
        h4(paste0("Test Kodu: ", test_kodu)),
        h4(paste0("Sual sayı: ", nrow(d))),
        footer = modalButton("Bağla")
      ))
      
      update_available_tests()
      selected_questions_rv(data.frame())
      updateTextInput(session, "test_adi_input", value = "")
      updateTextInput(session, "test_kodu_input", value = "")
      
    }, error = function(e) {
      showNotification(paste0("❌ Xəta: ", e$message), type = "error")
    })
  })
  
  output$existing_tests_table <- DT::renderDT({
    input$refresh_tests
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    tests <- dbGetQuery(con, "
      SELECT test_id AS \"ID\", test_adi AS \"Ad\", test_kodu AS \"Kod\",
             test_tarixi AS \"Tarix\", umumi_sual_sayi AS \"Say\",
             status AS \"Status\"
      FROM tests ORDER BY test_id DESC;
    ")
    
    if (nrow(tests) == 0) {
      return(data.frame(Mesaj = "Test yoxdur"))
    }
    DT::datatable(tests, options = list(pageLength = 10), 
                 rownames = FALSE, selection = 'multiple')
  })
  
  observeEvent(input$refresh_tests, {
    showNotification("🔄 Yeniləndi", type = "message")
  })
  
  observeEvent(input$delete_tests, {
    selected_rows <- input$existing_tests_table_rows_selected
    
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("⚠️ Test seçin!", type = "warning")
      return()
    }
    
    con <- get_db_connection()
    tests <- dbGetQuery(con, "SELECT test_id, test_kodu FROM tests ORDER BY test_id DESC;")
    dbDisconnect(con)
    
    selected_tests <- tests[selected_rows, ]
    tests_to_delete(selected_tests)
    
    showModal(modalDialog(
      title = "⚠️ TƏSDİQLƏYİN",
      h4(paste0(nrow(selected_tests), " test silinəcək?")),
      footer = tagList(
        modalButton("❌ Xeyr"),
        actionButton("confirm_delete", "✅ Bəli", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    selected_tests <- tests_to_delete()
    if (is.null(selected_tests)) return()
    
    deleted_count <- 0
    
    for (i in 1:nrow(selected_tests)) {
      tryCatch({
        test_id <- selected_tests$test_id[i]
        test_kodu <- selected_tests$test_kodu[i]
        
        con <- get_db_connection()
        dbExecute(con, "DELETE FROM test_questions WHERE test_id = $1", 
                 params = list(test_id))
        dbExecute(con, "DELETE FROM tests WHERE test_id = $1", 
                 params = list(test_id))
        dbDisconnect(con)
        
        rds_path <- file.path("../../data/tests", paste0(test_kodu, ".rds"))
        if (file.exists(rds_path)) file.remove(rds_path)
        
        deleted_count <- deleted_count + 1
      }, error = function(e) {})
    }
    
    removeModal()
    tests_to_delete(NULL)
    
    if (deleted_count > 0) {
      update_available_tests()
      showNotification(paste0("✅ ", deleted_count, " test silindi!"), type = "message")
    }
  })
}

shinyApp(ui = ui, server = server)
