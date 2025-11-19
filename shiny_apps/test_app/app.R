# ============================================================================
# ŞAGİRD TEST TƏTBİQİ - BÖLGƏLƏR ÜÇÜN (YALNIZ SQLite)
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)

Sys.setenv(TZ = "Asia/Baku")

# ============================================================================
# VERİLƏNLƏR BAZASI BAĞLANTISI (YALNIZ SQLite)
# ============================================================================

get_sqlite_connection <- function() {
  sqlite_path <- "database/sqlite/test_results_local.db"
  dbConnect(SQLite(), sqlite_path)
}

# ============================================================================
# LOKAL BAZANIN İNİSİALİZASİYASI
# ============================================================================

init_local_db <- function() {
  con <- get_sqlite_connection()
  on.exit(dbDisconnect(con))
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS test_results (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id TEXT,
      test_session TEXT,
      school TEXT,
      grade TEXT,
      teacher TEXT,
      question_id INTEGER,
      question_text TEXT,
      subject TEXT,
      content_area TEXT,
      selected_answer TEXT,
      correct_answer TEXT,
      is_correct INTEGER,
      points INTEGER,
      difficulty REAL,
      a_param REAL,
      b_param REAL,
      c_param REAL,
      fisher_info REAL,
      exam_time_baku TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      synced_to_server INTEGER DEFAULT 0
    );
  ")
}

init_local_db()

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$span("🎓 TESTLƏŞMƏ SİSTEMİ", 
                      style = "font-size: 26px; font-weight: bold;"),
    titleWidth = 500
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    
    tags$script(HTML("
      document.addEventListener('change', function(e) {
        if (e.target.matches('input[type=radio]')) {
          var name = e.target.name;
          var value = e.target.value;
          Shiny.setInputValue(name, value, {priority: 'event'});
        }
      });
      
      // TİMER FUNKSİYALARI
      var timerInterval;
      var remainingSeconds;
      var warningShown = false;
      
      function startTimer(minutes) {
        remainingSeconds = minutes * 60;
        updateTimerDisplay();
        
        timerInterval = setInterval(function() {
          remainingSeconds--;
          updateTimerDisplay();
          
          // 5 dəqiqə qaldı xəbərdarlığı
          if (remainingSeconds === 300 && !warningShown) {
            alert('⚠️ DİQQƏT: 5 dəqiqə qalıb!');
            warningShown = true;
            $('#timer').addClass('timer-warning');
          }
          
          // Vaxt bitdi
          if (remainingSeconds <= 0) {
            clearInterval(timerInterval);
            alert('⏰ VAXT BİTDİ! Test avtomatik olaraq bitir.');
            $('#finish_btn').click();
          }
        }, 1000);
      }
      
      function updateTimerDisplay() {
        var minutes = Math.floor(remainingSeconds / 60);
        var seconds = remainingSeconds % 60;
        var display = minutes + ':' + (seconds < 10 ? '0' : '') + seconds;
        $('#timer').text('⏱️ ' + display);
        
        // Son 5 dəqiqə qırmızı rəng
        if (remainingSeconds <= 300) {
          $('#timer').addClass('timer-warning');
        }
      }
      
      function stopTimer() {
        if (timerInterval) {
          clearInterval(timerInterval);
          warningShown = false;
        }
      }
      
      // Shiny-dən mesaj qəbul et
      Shiny.addCustomMessageHandler('stopTimer', function(message) {
        stopTimer();
      });
    ")),
    
    tags$head(tags$style(HTML("
      body, .content-wrapper { 
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        font-size: 22px;
      }
      .main-box {
        background: white;
        padding: 50px;
        border-radius: 25px;
        box-shadow: 0 15px 40px rgba(0,0,0,0.3);
        margin: 30px auto;
        max-width: 1500px;
        position: relative;
      }
      
      /* TİMER */
      .timer-box {
        position: fixed;
        top: 20px;
        right: 20px;
        background: linear-gradient(135deg, #f39c12 0%, #e67e22 100%);
        color: white;
        padding: 20px 30px;
        border-radius: 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.4);
        z-index: 1000;
        font-size: 32px;
        font-weight: bold;
        text-align: center;
        min-width: 220px;
      }
      .timer-warning {
        background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
        animation: pulse 1s infinite;
      }
      @keyframes pulse {
        0%, 100% { transform: scale(1); box-shadow: 0 10px 30px rgba(0,0,0,0.4); }
        50% { transform: scale(1.05); box-shadow: 0 15px 40px rgba(231,76,60,0.6); }
      }
      
      /* TEST MƏLUMAT BARI */
      .test-info-bar {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
        padding: 20px 30px;
        border-radius: 12px;
        margin-bottom: 30px;
        display: flex;
        justify-content: space-around;
        align-items: center;
        box-shadow: 0 5px 15px rgba(52, 152, 219, 0.4);
      }
      .test-info-item {
        text-align: center;
        flex: 1;
      }
      .test-info-item .label {
        font-size: 16px;
        opacity: 0.95;
        margin-bottom: 8px;
        font-weight: 500;
      }
      .test-info-item .value {
        font-size: 24px;
        font-weight: bold;
      }
      
      .subject-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 25px;
        border-radius: 12px;
        margin: 40px 0 20px 0;
        font-size: 26px;
        font-weight: bold;
        text-align: center;
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.5);
      }
      .question-item {
        background: linear-gradient(135deg, #f9f9f9 0%, #ececec 100%);
        padding: 35px;
        margin: 25px 0;
        border-radius: 15px;
        border-left: 6px solid #3c8dbc;
        transition: all 0.3s;
      }
      .question-item:hover {
        box-shadow: 0 8px 20px rgba(0,0,0,0.1);
        transform: translateX(5px);
      }
      .question-text {
        font-size: 24px;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 20px;
        line-height: 1.5;
      }
      .radio-card {
        background: white;
        border: 3px solid #ddd;
        border-radius: 12px;
        padding: 18px;
        margin: 10px 0;
        transition: 0.3s;
        cursor: pointer;
      }
      .radio-card:hover {
        border-color: #3c8dbc;
        background: #f0f8ff;
      }
      input[type='radio'] {
        width: 24px; 
        height: 24px; 
        margin-right: 15px;
        accent-color: #3c8dbc;
      }
      .btn-finish {
        background: linear-gradient(135deg, #00a65a 0%, #008d4c 100%);
        color: white; 
        font-size: 26px; 
        padding: 20px 50px;
        border: none; 
        border-radius: 15px; 
        font-weight: bold;
        width: 100%; 
        margin-top: 40px;
        box-shadow: 0 10px 30px rgba(0,166,90,0.5);
      }
      .btn-finish:hover {
        transform: translateY(-3px);
        box-shadow: 0 15px 40px rgba(0,166,90,0.6);
      }
      .user-info-box {
        background: linear-gradient(135deg, #ecf0f1 0%, #d5dbdb 100%);
        padding: 35px;
        border-radius: 15px;
        margin-bottom: 30px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }
      .test-select-box {
        background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
        padding: 30px;
        border-radius: 15px;
        margin-bottom: 30px;
        border: 3px solid #f39c12;
      }
      .info-note {
        background: #d1ecf1;
        border-left: 5px solid #0c5460;
        padding: 20px;
        border-radius: 8px;
        margin: 20px 0;
      }
    "))),
    
    uiOutput("main_content")
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    test_loaded = FALSE,
    test_data = NULL,
    questions = NULL,
    test_finished = FALSE,
    results = NULL
  )
  
  # ========== ƏSAS MƏZMUN ==========
  
  output$main_content <- renderUI({
    if (rv$test_finished) {
      render_results()
    } else if (!rv$test_loaded) {
      render_test_selection()
    } else {
      render_test_form()
    }
  })
  
  # ========== TEST SEÇİMİ EKRANI ==========
  
  render_test_selection <- function() {
    # available_tests.rds-dən oxu
    available_tests_path <- "data/tests/available_tests.rds"
    
    if (file.exists(available_tests_path)) {
      test_list <- readRDS(available_tests_path)
      test_files <- paste0(test_list$tests$test_kodu, ".rds")
    } else {
      test_files <- list.files("data/tests/", 
                               pattern = "^TEST_.*\\.rds$", 
                               full.names = FALSE)
    }
    
    tags$div(
      class = "main-box",
      tags$h1("📚 TEST SEÇİMİ", 
              style = "text-align:center; color:#3c8dbc; margin-bottom:40px;"),
      
      tags$div(
        class = "test-select-box",
        tags$h3("⚠️ MƏLUMAT", style = "color:#f39c12; margin-bottom:20px;"),
        tags$p("Test seçin və məlumatlarınızı daxil edərək teste başlayın.", 
               style = "font-size:20px;")
      ),
      
      tags$div(
        class = "info-note",
        tags$strong("ℹ️ QEYD: "),
        "Nəticələr lokal bazaya yazılacaq. Test bitdikdən sonra administrator nəticələri toplayacaq."
      ),
      
      if (length(test_files) == 0) {
        tags$div(
          class = "alert alert-danger",
          style = "font-size:22px; padding:25px;",
          tags$h3("❌ Heç bir test tapılmadı!"),
          tags$p("Testlər əlavə edilməlidir.")
        )
      } else {
        tagList(
          selectInput("test_file_select", 
                      "📋 TEST SEÇİN:", 
                      choices = c("Seçin..." = "", test_files),
                      width = "100%"),
          
          br(),
          
          tags$div(
            class = "user-info-box",
            tags$h3("👤 İSTİFADƏÇİ MƏLUMATLARI", 
                    style = "margin-bottom:25px; color:#3c8dbc;"),
            fluidRow(
              column(6, textInput("user_id", "İSTİFADƏÇİ ID:", 
                                  value = paste0("S", sample(10000:99999, 1)))),
              column(6, textInput("student_name", "AD VƏ SOYAD:", 
                                  placeholder = "Məsələn: Əli Məmmədov"))
            ),
            fluidRow(
              column(4, textInput("school", "📘 MƏKTƏB:", 
                                  placeholder = "Məktəb adı")),
              column(4, selectInput("grade", "🏫 SİNİF:", 
                                    choices = c("7","8","9","10","11"))),
              column(4, textInput("teacher", "👩‍🏫 MÜƏLLİM:",
                                  placeholder = "Müəllim adı"))
            )
          ),
          
          br(),
          
          actionButton("load_test_btn", 
                       "🚀 TESTİ YÜKLƏ VƏ BAŞLA", 
                       class = "btn-finish")
        )
      }
    )
  }
  
  # ========== TEST YÜKLƏ ==========
  
  observeEvent(input$load_test_btn, {
    
    req(input$test_file_select)
    req(input$user_id)
    req(input$student_name)
    
    if (input$test_file_select == "") {
      showNotification("❌ Test seçin!", type = "error")
      return()
    }
    
    if (nchar(trimws(input$student_name)) == 0) {
      showNotification("Ad və soyadınızı daxil edin!", type = "error")
      return()
    }
    
    test_path <- file.path("../../data/tests", input$test_file_select)
    
    tryCatch({
      rv$test_data <- readRDS(test_path)
      rv$questions <- rv$test_data$questions
      rv$test_loaded <- TRUE
      
      showNotification("✅ Test yükləndi! Uğurlar!", type = "message")
      
    }, error = function(e) {
      showNotification(paste0("❌ Test yüklənə bilmədi: ", e$message), 
                       type = "error")
    })
  })
  
  # ========== TEST FORMU ==========
  
  render_test_form <- function() {
    req(rv$questions)
    
    # Test məlumatlarını al
    test_muddeti <- if (!is.null(rv$test_data$test_info$test_muddeti)) {
      rv$test_data$test_info$test_muddeti
    } else {
      90
    }
    
    test_tarixi <- if (!is.null(rv$test_data$test_info$test_tarixi)) {
      format(as.Date(rv$test_data$test_info$test_tarixi), "%d.%m.%Y")
    } else {
      format(Sys.Date(), "%d.%m.%Y")
    }
    
    # Sualları fənnə görə qrupla
    questions_ui <- lapply(seq_len(nrow(rv$questions)), function(i) {
      q <- rv$questions[i, ]
      
      tagList(
        # Fənn başlığı (yalnız fənn dəyişəndə)
        if (i == 1 || rv$questions$fenn[i] != rv$questions$fenn[i - 1]) {
          tags$div(class = "subject-header", toupper(q$fenn))
        },
        
        # Sual
        tags$div(
          class = "question-item",
          tags$div(class = "question-text", 
                   paste0("SUAL ", i, ": ", q$sual_metni)),
          
          # Şəkil (varsa)
          if (!is.na(q$sekil_var) && q$sekil_var == TRUE && 
              !is.na(q$sekil_yolu) && q$sekil_yolu != "") {
            tags$img(src = q$sekil_yolu, 
                     width = "320px",
                     style = "margin:20px 0; border-radius:10px; box-shadow:0 5px 15px rgba(0,0,0,0.2);")
          },
          
          # Cavab variantları
          div(
            class = "radio-card-group",
            lapply(c("A","B","C","D","E"), function(opt) {
              variant_col <- paste0("variant_", tolower(opt))
              label_text <- q[[variant_col]]
              
              tags$label(
                class = "radio-card",
                tags$input(type = "radio", 
                           name = paste0("q_", i), 
                           value = opt),
                paste(opt, ")", label_text)
              )
            })
          )
        )
      )
    })
    
    # Əsas form
    tags$div(
      class = "main-box",
      
      # TİMER (fixed position, sağ yuxarıda)
      tags$div(id = "timer", class = "timer-box", "⏱️ --:--"),
      
      # TEST BAŞLIĞI
      tags$h2(paste0("📝 ", rv$test_data$test_info$test_adi), 
              style = "text-align:center; color:#3c8dbc; margin-bottom:25px; font-size:36px;"),
      
      # TEST MƏLUMAT BARI
      tags$div(
        class = "test-info-bar",
        tags$div(
          class = "test-info-item",
          tags$div(class = "label", "📅 Test Tarixi"),
          tags$div(class = "value", test_tarixi)
        ),
        tags$div(
          class = "test-info-item",
          tags$div(class = "label", "⏱️ Müddət"),
          tags$div(class = "value", paste0(test_muddeti, " dəqiqə"))
        ),
        tags$div(
          class = "test-info-item",
          tags$div(class = "label", "📊 Sual Sayı"),
          tags$div(class = "value", nrow(rv$questions))
        )
      ),
      
      # İSTİFADƏÇİ MƏLUMATLARI
      tags$div(
        style = "background:#d4edda; padding:20px; border-radius:10px; margin-bottom:30px;",
        tags$p(paste0("👤 ", input$student_name, " | ",
                      "🏫 ", input$grade, " sinif | ",
                      "📘 ", input$school),
               style = "font-size:20px; margin:0; color:#155724;")
      ),
      
      tags$hr(style = "border-top:3px solid #3c8dbc; margin:40px 0;"),
      
      # SUALLAR
      questions_ui,
      
      # BİTİR DÜYMƏSI
      actionButton("finish_btn", 
                   "TESTİ BİTİR VƏ NƏTİCƏLƏRİ GÖR", 
                   class = "btn-finish"),
      
      # TİMERİ BAŞLAT (səhifə yükləndikdə)
      tags$script(HTML(paste0("startTimer(", test_muddeti, ");")))
    )
  }
  
  # ========== TESTİ BİTİR VƏ NƏTİCƏLƏRİ YAZ ==========
  
  observeEvent(input$finish_btn, {
    req(rv$questions)
    
    # Timeri dayandır
    session$sendCustomMessage(type = "stopTimer", message = list())
    
    # Cavabları topla
    answers <- lapply(seq_len(nrow(rv$questions)), function(i) {
      q <- rv$questions[i, ]
      sel_raw <- input[[paste0("q_", i)]]
      
      selected_clean <- toupper(trimws(ifelse(is.null(sel_raw), "", as.character(sel_raw))))
      correct_clean <- toupper(trimws(as.character(q$dogru_cavab)))
      
      is_correct <- ifelse(
        nzchar(selected_clean) && 
          selected_clean %in% c("A","B","C","D","E") &&
          selected_clean == correct_clean, 
        1L, 0L
      )
      
      data.frame(
        question_id = as.integer(q$question_id),
        fenn = as.character(q$fenn),
        selected_answer = selected_clean,
        correct_answer = correct_clean,
        is_correct = is_correct,
        stringsAsFactors = FALSE
      )
    })
    
    rv$results <- bind_rows(answers)
    
    # ========== YALNIZ LOKAL BAZAYA YAZ ==========
    
    test_session <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    tryCatch({
      con_local <- get_sqlite_connection()
      on.exit(dbDisconnect(con_local))
      
      sql <- "
        INSERT INTO test_results (
          user_id, test_session, school, grade, teacher,
          question_id, question_text, subject, content_area,
          selected_answer, correct_answer, is_correct, points,
          difficulty, a_param, b_param, c_param,
          fisher_info, exam_time_baku, synced_to_server
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
      "
      
      for (i in seq_len(nrow(rv$results))) {
        r <- rv$results[i, ]
        q <- rv$questions[i, ]
        
        fisher_info <- ifelse(!is.na(q$ayirdetme),
                              round(q$ayirdetme^2 * 0.25, 3), NA)
        
        params <- list(
          as.character(input$user_id),
          as.character(test_session),
          as.character(input$school),
          as.character(input$grade),
          as.character(input$teacher),
          as.integer(q$question_id),
          as.character(q$sual_metni),
          as.character(q$fenn),
          as.character(q$mezmun_sahesi),
          as.character(r$selected_answer),
          as.character(r$correct_answer),
          as.integer(r$is_correct),
          as.integer(r$is_correct),
          as.numeric(q$cetinlik),
          as.numeric(q$ayirdetme),
          as.numeric(q$cetinlik),
          as.numeric(q$texminetme),
          as.numeric(fisher_info),
          as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Asia/Baku")),
          0L  # synced_to_server = 0 (hələ göndərilməyib)
        )
        
        dbExecute(con_local, sql, params = params)
      }
      
      showNotification("Nəticələr uğurla qeyd edildi!", type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste0("Xəta: ", e$message), type = "error", duration = 10)
    })
    
    rv$test_finished <- TRUE
  })
  
  # ========== NƏTİCƏLƏR EKRANI ==========
  
  render_results <- function() {
    req(rv$results)
    
    total <- nrow(rv$results)
    correct <- sum(rv$results$is_correct, na.rm = TRUE)
    incorrect <- total - correct
    percentage <- ifelse(total > 0, round(100 * correct / total, 1), 0)
    
    tags$div(
      class = "main-box",
      tags$h2("📊 TEST NƏTİCƏLƏRİ", 
              style = "text-align:center; color:#3c8dbc; font-size:40px; margin-bottom:40px;"),
      
      tags$div(
        class = "info-note",
        tags$strong("ℹ️ QEYD: "),
        "Nəticələriniz lokal bazaya yazıldı. Administrator məlumatları toplayacaq."
      ),
      
      tags$div(
        style = "background:#d4edda; padding:20px; border-radius:10px; margin: 20px 0;",
        tags$h3(paste0("👤 ", input$student_name), 
                style = "margin:0; color:#155724;")
      ),
      
      fluidRow(
        column(4, tags$div(
          style = "background:#00a65a; color:white; padding:25px; text-align:center; border-radius:15px; box-shadow:0 5px 15px rgba(0,166,90,0.3);",
          tags$h3("✅ DOĞRU", style = "margin:0 0 15px 0;"), 
          tags$h1(correct, style = "margin:0; font-size:4em;")
        )),
        column(4, tags$div(
          style = "background:#dd4b39; color:white; padding:25px; text-align:center; border-radius:15px; box-shadow:0 5px 15px rgba(221,75,57,0.3);",
          tags$h3("❌ SƏHV", style = "margin:0 0 15px 0;"), 
          tags$h1(incorrect, style = "margin:0; font-size:4em;")
        )),
        column(4, tags$div(
          style = "background:#f39c12; color:white; padding:25px; text-align:center; border-radius:15px; box-shadow:0 5px 15px rgba(243,156,18,0.3);",
          tags$h3("📊 FAİZ", style = "margin:0 0 15px 0;"), 
          tags$h1(paste0(percentage, "%"), style = "margin:0; font-size:4em;")
        ))
      ),
      
      br(), br(),
      
      fluidRow(
        column(6, 
               tags$div(
                 style = "background:#f8f9fa; padding:25px; border-radius:15px;",
                 tags$h3("📈 QRAFİK", style = "color:#3c8dbc; margin-bottom:20px;"), 
                 plotOutput("result_plot", height = "420px")
               )
        ),
        column(6, 
               tags$div(
                 style = "background:#f8f9fa; padding:25px; border-radius:15px;",
                 tags$h3("📊 CƏDVƏL", style = "color:#3c8dbc; margin-bottom:20px;"), 
                 tableOutput("result_table")
               )
        )
      ),
      
      br(), br(),
      
      actionButton("new_test_btn", 
                   "🔄 YENİ TEST", 
                   class = "btn-finish",
                   style = "background: linear-gradient(135deg, #3c8dbc 0%, #2c6ba0 100%);")
    )
  }
  
  # Qrafik
  output$result_plot <- renderPlot({
    req(rv$results)
    
    by_subject <- rv$results %>%
      group_by(fenn) %>%
      summarise(
        Dogru = sum(is_correct), 
        Sehv = n() - sum(is_correct),
        .groups = "drop"
      )
    
    plot_data <- pivot_longer(by_subject, 
                              cols = c("Dogru", "Sehv"), 
                              names_to = "Tip", 
                              values_to = "Say")
    
    ggplot(plot_data, aes(x = fenn, y = Say, fill = Tip)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = Say), 
                position = position_dodge(0.9), 
                vjust = -0.3, 
                size = 6) +
      scale_fill_manual(values = c("Dogru" = "#00a65a", "Sehv" = "#dd4b39")) +
      labs(x = "", y = "Sualların Sayı", fill = "") +
      theme_minimal(base_size = 18) +
      theme(
        legend.position = "top",
        axis.text = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16)
      )
  })
  
  # Cədvəl
  output$result_table <- renderTable({
    req(rv$results)
    
    rv$results %>%
      group_by(fenn) %>%
      summarise(
        Toplam = n(), 
        Dogru = sum(is_correct), 
        Faiz = paste0(round(100 * sum(is_correct) / n(), 1), "%"),
        .groups = "drop"
      ) %>%
      rename(`Fənn` = fenn)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Yenidən başla
  observeEvent(input$new_test_btn, {
    rv$test_loaded <- FALSE
    rv$test_data <- NULL
    rv$questions <- NULL
    rv$test_finished <- FALSE
    rv$results <- NULL
  })
}

# ============================================================================
# TƏTBQ BAŞLAT
# ============================================================================

shinyApp(ui = ui, server = server)