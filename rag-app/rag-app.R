# ===========================
#   RAG Copilot - Shiny App
# ===========================

library(shiny)
library(shinyWidgets)
library(jsonlite)
library(RAGFlowChainR)
library(chatLLM)
library(markdown)


default_openai_models <- c(
  "gpt-4.1",
  "gpt-4.1-mini",
  "gpt-4o",
  "gpt-4o-mini",
  "gpt-3.5 turbo"
)

ui <- fluidPage(
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;800&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      body { background-color: #191C20 !important; color: #fff; font-family: 'Inter', sans-serif; }
      .sidebar-custom {
        background: #6c767d;
        min-height: 100vh;
        padding: 38px 22px 0 22px;
        position: fixed;
        left: 0; top: 0;
        width: 320px;
        color: #fff;
        font-family: 'Inter', sans-serif;
        border-right: 2px solid #23242a;
        z-index: 100;
      }
      .sidebar-label {
        font-weight: 600; font-size: 15px; margin-bottom: 18px; margin-top: 20px;
      }
      .sidebar-custom input[type='password'],
      .sidebar-custom select,
      .sidebar-custom .selectize-input {
        background: #6c767d !important;
        color: #fff !important;
        border-radius: 12px !important;
        border: 1.5px solid #393d40 !important;
        font-size: 17px !important;
        font-family: 'Inter', sans-serif;
        box-shadow: none !important;
        margin-bottom: 18px;
        padding: 10px 12px !important;
      }
      .sidebar-custom .selectize-input,
      .sidebar-custom .selectize-input.full {
        background: #6c767d !important;
        color: #fff !important;
        border: 1.5px solid #393d40 !important;
        border-radius: 12px !important;
        box-shadow: none !important;
      }
      .sidebar-custom .selectize-dropdown-content {
        background: #6c767d !important;
        color: #fff !important;
        border-radius: 12px !important;
        border: 1.5px solid #393d40 !important;
      }
      .sidebar-custom input[type='password']:focus,
      .sidebar-custom .selectize-input.focus {
        outline: 2px solid #a5d6ff !important;
        box-shadow: none !important;
      }
      .main-panel-custom {
        margin-left: 320px;
        padding: 0 0 0 0;
        min-height: 100vh;
        background: transparent;
      }
      /* Sticky title + log bar */
      .main-sticky-header {
        position: sticky;
        top: 0;
        z-index: 15;
        background: #191C20f2;
        padding-top: 10px;
        padding-bottom: 8px;
        margin-bottom: 0;
        box-shadow: 0 2px 18px 0 rgba(10,15,20,0.04);
      }
      .main-title {
        font-size: 44px;
        font-weight: bold;
        margin-top: 0;
        margin-bottom: 8px;
        padding-left: 0;
        margin-left: 0;
      }
      .main-title-container {
        width: 100%;
        max-width: 900px;
        min-width: 340px;
        margin: 0 auto;
      }
      .log-bar-container {
        width: 100%;
        max-width: 900px;
        min-width: 340px;
        margin: 0 auto 0 auto;
      }
      .chat-outer-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        width: 100%;
        min-height: 80vh;
      }
      .chat-inner-container {
        width: 100%;
        max-width: 900px;
        min-width: 340px;
        margin: 0 auto;
        height: calc(100vh - 125px - 90px); /* header + input height */
        min-height: 320px;
        padding-bottom: 20px;
        overflow-y: auto;
        scrollbar-width: none; /* Firefox */
        -ms-overflow-style: none; /* IE 10+ */
      }
      .chat-inner-container::-webkit-scrollbar {
        display: none; /* Chrome, Safari, Opera */
        width: 0 !important;
        background: transparent !important;
      }
      .dropdown-menu {
        width: 100% !important;
        min-width: 100% !important;
        border-radius: 13px !important;
        background: #222529 !important;
        color: #fff !important;
        font-size: 14px !important; /* Smaller font */
      }
      .dropdown-toggle {
        width: 100% !important;
        border-radius: 14px !important;
        background: #191C20 !important;
        border: 1px solid #444;
        color: #fff !important;
        text-align: left;
        font-size: 13px !important; /* Smaller font */
        padding: 14px 16px;
      }
      .debugbox { background: #222529; border-radius: 16px; padding: 18px; margin-bottom: 18px; font-size: 15px; }
      .json-pre { background: #191C20; color: #a5d6ff; padding: 12px; border-radius: 8px; font-size: 13px; }
      .chatbox { margin-bottom: 20px; }
      .chat-msg-user, .chat-msg-ai {
        margin: 10px 0;
        padding: 16px 20px;
        border-radius: 18px;
        max-width: 98%;
        min-width: 220px;
        font-size: 15px;
        word-break: break-word;
        background: #222529;
        display: flex;
        align-items: flex-start;  /* <-- Change this line */
      }
      .chat-msg-user { background: #272a2f; align-self: flex-end; }
      .chat-msg-ai {
        background: #191C20;
        align-self: flex-start;
      }
      .ai-icon {
        background: #ffe082;
        color: #191C20;
        border-radius: 8px;
        width: 28px;
        height: 28px;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-right: 10px;
        font-size: 16px;      /* Smaller icon */
        flex-shrink: 0;
        flex-grow: 0;
        margin-top: 2px;      /* Fine-tune vertical alignment */
      }
      .user-icon {
        background: #90caf9;
        color: #191C20;
        border-radius: 8px;
        padding: 2px 6px;     /* Smaller padding */
        margin-right: 10px;
        margin-left: 0;
        display: inline-block;
        font-size: 15px;      /* Smaller icon */
        margin-top: 2px;      /* Fine-tune vertical alignment */
      }
      .ai-icon, .user-icon {
        margin-top: 2px; /* or tweak as you like */
      }
      .chat-input-float-container {
        position: fixed;
        left: 320px;
        bottom: 28px;
        width: calc(100vw - 320px);
        display: flex;
        justify-content: center;
        z-index: 999;
        pointer-events: none;
        background: transparent;
      }
      .chat-input-container {
        position: relative;
        width: 100%;
        max-width: 900px;
        min-width: 220px;
        margin: 0 auto;
        pointer-events: auto;
      }
      #user_input {
        width: 100%;
        padding: 10px 40px 10px 18px;    /* less padding = thinner input */
        background: #2c2d33;
        color: #bbb;
        border: none;
        border-radius: 18px;
        font-size: 15px;
        box-shadow: none;
        outline: none;
        min-height: 28px;
        max-height: 46px;                /* max-height so it doesn't expand much */
        height: 38px !important;         /* fixed height for consistency */
        resize: none !important;
      }

      #user_input::placeholder {
        color: #888;
        opacity: 0.95;
        font-size: 15px;
      }
      #user_input:focus {
        outline: 2px solid #6ba4ff;
      }
      textarea#user_input {
        resize: none !important;
        overflow-y: hidden !important;
        min-height: 44px;
        max-height: 180px;
        box-sizing: border-box;
      }
      .send-arrow-btn {
        position: absolute;
        right: 14px;
        top: 50%;
        transform: translateY(-70%);
        background: none;
        border: none;
        color: #8d9096;
        font-size: 22px;
        cursor: pointer;
        padding: 0;
        z-index: 10;
      }
      .send-arrow-btn:active {
        color: #fff;
      }
      .log-download-btn {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        color: #a5d6ff !important;
        font-size: 19px !important;
        padding: 0 8px 0 0 !important;
        float: right;
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }
      .log-download-btn .btn {
        background: none !important;
        border: none !important;
        box-shadow: none !important;
        color: #a5d6ff !important;
        font-size: 19px !important;
        padding: 0 !important;
        margin: 0 !important;
      }
      .json-log-scrollbox {
        max-height: 230px;
        overflow-y: auto;
        margin-top: 8px;
        border-radius: 12px;
      }
      .json-log-scrollbox::-webkit-scrollbar {
        width: 7px;
        background: #23242a;
        border-radius: 8px;
      }
      .json-log-scrollbox::-webkit-scrollbar-thumb {
        background: #444;
        border-radius: 7px;
      }
      /* Spinner styles */
      .spinner-container {
        display: flex;
        align-items: center;
        margin: 10px 0 10px 0;
        color: #a5d6ff;
        font-size: 16px;
        font-family: 'Inter', sans-serif;
      }
      .spinner-dot {
        width: 18px;
        height: 18px;
        border: 3px solid #a5d6ff;
        border-top: 3px solid #222529;
        border-radius: 50%;
        margin-right: 14px;
        animation: spin 0.8s linear infinite;
      }
      @keyframes spin {
        0% { transform: rotate(0deg);}
        100% { transform: rotate(360deg);}
      }
      #toast {
        visibility: hidden;
        min-width: 260px;
        color: #fff;
        text-align: left;
        border-radius: 14px;
        padding: 18px 30px;
        position: fixed;
        z-index: 9999;
        right: 36px;
        top: 36px;
        font-size: 18px;
        font-family: 'Inter', sans-serif;
        box-shadow: 0 6px 24px rgba(0,0,0,0.18);
        opacity: 0;
        transition: opacity 0.5s, visibility 0.5s, background 0.3s;
        background: #222;
      }
      #toast.success {
        background-color: #4BB543 !important;
      }
      #toast.error {
        background-color: #ff5252 !important;
      }
      #toast.show {
        visibility: visible;
        opacity: 1;
      }
      @media (max-width: 1100px) {
        .main-panel-custom { padding: 0; }
        .main-title-container, .log-bar-container, .chat-inner-container, .chat-input-container { max-width: 98vw !important; }
        .chat-inner-container { height: calc(100vh - 125px - 90px); }
      }
      @media (max-width: 800px) {
        .main-panel-custom, .main-title-container, .log-bar-container, .chat-inner-container, .chat-input-container {
          width: 98vw !important;
          min-width: 0;
          margin-left: 0;
          padding-left: 0;
        }
        .chat-input-float-container {
          left: 0 !important;
          width: 100vw !important;
          margin-left: 0 !important;
        }
      }
    "))
  ),

  uiOutput("sidebar"),

  tags$div(class = "main-panel-custom",
           div(class = "main-sticky-header",
               div(class = "main-title-container",
                   div(class = "main-title", "Your RAG Copilot")
               ),
               div(class = "log-bar-container",
                   shinyWidgets::dropdownButton(
                     label = "Show Conversation Log",
                     circle = FALSE, status = "default", icon = icon("angle-down"), width = "100%",
                     div(style="display: flex; align-items: center; justify-content: space-between;",
                         tags$strong("Conversation log for this session:"),
                         downloadButton("download_chatlog", NULL, class = "log-download-btn", style="margin-left:auto; margin-bottom: 8px;", icon = icon("download"))
                     ),
                     div(
                       class = "json-log-scrollbox",
                       uiOutput("json_chat_history")
                     )
                   )
               )
           ),
           uiOutput("conditional_chat_panel")
  ),

  div(id = "toast"),

  tags$script(HTML('
  $(document).on("input", "#user_input", function() {
    this.style.height = "auto";
    this.style.height = (this.scrollHeight) + "px";
  });
  $(document).on("keydown", "#user_input", function(e) {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      setTimeout(function() {
        $("#send").click();
      }, 60);
      return false;
    }
  });
  Shiny.addCustomMessageHandler("resetInputHeight", function(message) {
    var input = document.getElementById("user_input");
    if (input) input.style.height = "auto";
  });
  Shiny.addCustomMessageHandler("scrollToBottom", function(message) {
    document.getElementById("bottom-anchor").scrollIntoView({ behavior: "smooth" });
  });
  Shiny.addCustomMessageHandler("showToast", function(opts) {
    var toast = document.getElementById("toast");
    toast.textContent = opts.message;
    toast.className = opts.type + " show";
    setTimeout(function(){
      toast.className = toast.className.replace("show", "");
    }, 3400);
  });
'))
)

server <- function(input, output, session) {
  # Reactive values for API key, models, LLM and RAG chain
  rv <- reactiveValues(
    api_key_valid = FALSE,
    api_key_error = NULL,
    openai_models = default_openai_models,
    LLM = NULL,
    rag_chain = NULL
  )

  # ----- SIDEBAR UI -----
  output$sidebar <- renderUI({
    tags$div(class="sidebar-custom",
             tags$div(class="sidebar-label", "Enter your OpenAI API Key"),
             div(class = "sidebar-input", passwordInput("api_key", NULL, placeholder = "API Key", width = "100%")),
             tags$div(class="sidebar-label", "Choose OpenAI Models"),
             div(class = "sidebar-dropdown",
                 selectInput("openai_model", NULL, choices = rv$openai_models, width = "100%",
                             selected = rv$openai_models[1]
                 )
             ),
             tags$hr(style="border: 1px solid #393d40;"),
             tags$p("Enter your OpenAI API Key and Choose one of the OpenAI models.",
                    style="color:#fff; opacity:0.75; font-size:15px;"),
             if (!is.null(rv$api_key_error))
               tags$div(style="color:#ffb4b4; font-size:16px; margin-top:16px;",
                        rv$api_key_error)
    )
  })

  # ----- MAIN PANEL UI -----
  output$conditional_chat_panel <- renderUI({
    if (!rv$api_key_valid) {
      tags$div(
        style="margin-top:64px;color:#bbb;font-size:20px;text-align:center;",
        "Please enter your OpenAI API Key to proceed."
      )
    } else {
      tagList(
        div(class = "chat-outer-container",
            div(class = "chat-inner-container",
                uiOutput("chat_history"),
                uiOutput("thinking_spinner"),
                tags$div(id = "bottom-anchor")
            )
        ),
        div(class = "chat-input-float-container",
            div(class = "chat-input-container",
                textAreaInput("user_input", NULL,
                              placeholder = "Enter your Marketing question here:",
                              width = "100%", rows = 1),
                actionButton("send", label = NULL,
                             icon = icon("angle-right"),
                             class = "send-arrow-btn")
            )
        )
      )
    }
  })

  # ----- VALIDATE API KEY -----
  observeEvent(input$api_key, {
    if (!is.null(input$api_key) && nzchar(input$api_key)) {
      tryCatch({
        test_llm <- call_llm(
          provider   = "openai",
          api_key    = input$api_key,
          max_tokens = 60,
          verbose    = FALSE
        )
        test_llm("Hello")
        rv$api_key_valid <- TRUE
        rv$api_key_error <- NULL
        session$sendCustomMessage("showToast",
                                  list(message = "API Key is valid! Connected to OpenAI.",
                                       type    = "success"))
      }, error = function(e) {
        rv$api_key_valid <- FALSE
        rv$api_key_error <- paste("Invalid API Key:", e$message)
        session$sendCustomMessage("showToast",
                                  list(message = "Invalid API Key. Please check and try again.",
                                       type    = "error"))
      })
    } else {
      rv$api_key_valid <- FALSE
      rv$api_key_error <- NULL
    }
  }, ignoreInit = TRUE)

  # ----- INITIALISE LLM & RAG CHAIN WHEN KEY & MODEL ARE SET -----
  observe({
    req(rv$api_key_valid, input$openai_model, input$api_key)
    Sys.setenv(OPENAI_API_KEY = input$api_key)
    rv$LLM <- call_llm(
      provider    = "openai",
      model       = input$openai_model,
      max_tokens  = 4000,
      api_key     = input$api_key
    )
    rv$rag_chain <- create_rag_chain(
      llm                         = rv$LLM,
      vector_database_directory   = "rag-app/data/my_vectorstore.duckdb",
      method                      = "DuckDB",
      embedding_function          = NULL,
      use_web_search              = FALSE
    )
  })

  # ----- CHAT HISTORY STORAGE -----
  chat_history <- reactiveVal(
    list(list(role = "ai", content = "How can I help you?"))
  )

  # ----- RENDER CHAT HISTORY AS HTML -----
  output$chat_history <- renderUI({
    msgs <- chat_history()
    ui_list <- lapply(msgs, function(msg) {
      content <- msg$content
      if (msg$role == "ai") {
        # Markdown → HTML transformations
        content <- gsub("^### (.*)$", "<h3>\\1</h3>", content, perl = TRUE)
        content <- gsub("\\*\\*(.*?)\\*\\*", "<b>\\1</b>", content, perl = TRUE)
        content <- gsub("__(.*?)__", "<b>\\1</b>", content, perl = TRUE)
        content <- gsub("\\*(.*?)\\*", "<i>\\1</i>", content, perl = TRUE)
        content <- gsub("_(.*?)_", "<i>\\1</i>", content, perl = TRUE)
        content <- gsub("^[ \t]+|[ \t]+$", "", content, perl = TRUE)
        content <- gsub("\n{2,}", "<br><br>", content)
        content <- gsub("\n", "<br>", content)
        rendered <- HTML(content)
      } else {
        rendered <- content
      }

      div(
        class = ifelse(msg$role == "ai", "chat-msg-ai", "chat-msg-user"),
        span(class = ifelse(msg$role == "ai", "ai-icon", "user-icon"),
             icon(ifelse(msg$role == "ai", "robot", "user"))),
        rendered
      )
    })
    tagList(ui_list)
  })

  # ----- RENDER RAW JSON LOG -----
  output$json_chat_history <- renderUI({
    tags$pre(class = "json-pre",
             toJSON(chat_history(), pretty = TRUE, auto_unbox = TRUE))
  })

  # ----- DOWNLOAD HANDLER FOR LOG -----
  output$download_chatlog <- downloadHandler(
    filename = function() {
      paste0("rag_copilot_chatlog_", Sys.Date(), ".json")
    },
    content = function(file) {
      write(
        jsonlite::toJSON(chat_history(), pretty = TRUE, auto_unbox = TRUE),
        file = file
      )
    }
  )

  # ----- AUTO‑SCROLL AFTER ANY CHAT HISTORY UPDATE -----
  observeEvent(chat_history(), {
    # after Shiny flushes new HTML, scroll to bottom
    session$onFlushed(function() {
      session$sendCustomMessage("scrollToBottom", list())
    }, once = TRUE)
  })

  # ----- SEND BUTTON HANDLER -----
  observeEvent(input$send, {
    req(trimws(input$user_input) != "")

    # 1) append the user message
    msgs <- chat_history()
    msgs <- append(msgs, list(list(role = "user", content = input$user_input)))
    chat_history(msgs)

    # 2) clear input & reset its height
    updateTextAreaInput(session, "user_input", value = "")
    session$sendCustomMessage("resetInputHeight", list())

    # 3) show “Thinking…” spinner
    output$thinking_spinner <- renderUI({
      div(class = "spinner-container",
          div(class = "spinner-dot"),
          "Thinking..."
      )
    })

    # 4) invoke your RAG chain asynchronously
    shiny::withProgress(message = "Thinking...", value = 0, {
      Sys.sleep(0.25)  # let the UI render first
      rag_resp <- tryCatch({
        rv$rag_chain$invoke(input$user_input)
      }, error = function(e) {
        list(answer = paste("Error:", e$message),
             input = input$user_input,
             chat_history = msgs)
      })

      # 5) append the AI’s reply
      msgs <- append(msgs, list(list(role = "ai", content = rag_resp$answer)))
      chat_history(msgs)

      # 6) hide spinner
      output$thinking_spinner <- renderUI({ NULL })
    })
  })
}

shinyApp(ui, server)
