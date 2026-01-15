
# Hub - My Workstack WorkstackR
library(dplyr)
library(lubridate)
library(shinycustomloader)
library(timevis)
library(rhandsontable)
library(DT)
library(shinyWidgets)
library(shinyalert)
library(shiny)
library(shinydashboard)

# [ Update These Values]
my_dir_style <- "C:/Users/NM2.P6JL0LLI/OneDrive - NHS/Desktop/R/"
my_dir <- "C:/Users/NM2.P6JL0LLI/OneDrive - NHS/Desktop/R/data/"

### Dependent Files 
# readRDS(paste0(my_dir, "hub_main.rds"))
# readRDS(paste0(my_dir, "hub_links.rds"))

# Set the IP address and port - Default Load
options(shiny.host = "127.0.0.1")
options(shiny.port = 8010)

# Not In
"%ni%" <- Negate("%in%")

# Visible Default Cols (DT minus 1)
my_dt_view <- c(0,5,6,7,8)

# status
my_status <- c("Open", "Park", "Closed")

# Loader Function
fun_spin <- function(x){ 
  withLoader(x, type="html", loader="dnaspin")
}

# Define UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "WorkstackR"),
  dashboardSidebar(
    fluidRow(align = "center",
             actionButton("bt_sh_new_request", "New Ticket", icon = icon("retweet"), style="color: #fff; background-color: #46505a; border-color: #51a5059")
    ),
    textInput("search", "Search Links", ""),
    hr(),
    uiOutput("link_buttons")
  ),
  dashboardBody(
    tags$head(includeCSS(paste0(my_dir_style,'www/style_helpr.css'))),
    tabsetPanel(id = "tabs", header = uiOutput("ui_options1"),
                tabPanel("Tasks", value = 1,
                         fluidRow(
                           box(title = "Tasks - By Review Date", width = 12,height = "auto", solidHeader = F,status = "primary",  fun_spin(DT::dataTableOutput("dt01"))
                           )
                         )
                ),
                tabPanel("Gantt Chart", icon = icon("plus"), value = 2,
                         fluidRow(
                           box(title = "Project Planner", width = 12,height = "auto", solidHeader = F,status = "primary",  
                               timevisOutput("ui_timvis_1"),
                               actionButton("default_tvs1", "Default", style="color: #fff; background-color: #46505a; border-color: #51a5059"),
                               actionButton("today_tvs1", "Today",  style="color: #fff; background-color: #46505a; border-color: #51a5059"),
                               actionButton("bt_show_tvs1", "Show All Items",  style="color: #fff; background-color: #46505a; border-color: #51a5059"),
                               hr(),
                               tableOutput("dt_timvis_1")
                           )
                         )
                ),
                tabPanel("Links", value = 3,
                         fluidRow(
                           box(title = "Add & Update Links - Sidbar (Right Click for more options)", width = 8,height = "auto", solidHeader = F,status = "primary",  
                               rHandsontableOutput("links_table"),
                               br(),
                               actionButton("save_links_bt", "Save Changes")
                           )
                         )
                )
    )
  )
) # End

# Define server logic
server <- function(input, output, session) {
  
  rectval_hub <- reactiveValues()
  rectval_hub$refresh <- F #[ Refresh ]
  click1_item <- reactiveVal() # Reactive Val - DT Row    
  
  # Refresh Function #[ Refresh ]
  fun_refresh <- function(){
    if(rectval_hub$refresh){
      rectval_hub$refresh <- FALSE
    } else {
      rectval_hub$refresh <- TRUE
    }
  }
  
  # Main Data Set
  df_main <- reactive({ 
    if(rectval_hub$refresh) {
      readRDS(paste0(my_dir, "hub_main.rds")) %>% 
        filter(Status %in% input$opt_101) %>% 
        arrange(ReviewDate)
    } else {
      readRDS(paste0(my_dir, "hub_main.rds")) %>% 
        filter(Status %in% input$opt_101) %>% 
        arrange(ReviewDate)
    }
  })
  
  # Main Data Set
  df_links <- reactive({ 
    if(rectval_hub$refresh) {
      readRDS(paste0(my_dir, "hub_links.rds"))
    } else {
      readRDS(paste0(my_dir, "hub_links.rds"))
    }
  })
  
  
  #=============================================================================================================================
  
  # Links Lookups
  links_values <- reactiveValues(data = readRDS(paste0(my_dir, "hub_links.rds")))
  
  # Links Table
  output$links_table <- renderRHandsontable({
    rhandsontable(links_values$data)
  })
  
  # Save the edited data
  observeEvent(input$save_links_bt, {
    print(links_values$data)
    saveRDS(links_values$data, paste0(my_dir, "hub_links.rds"))
    shinyalert("Updated", "", type = "success", timer = 1000, animation = "slide-from-bottom")
  })
  
  # Observe 
  observe({
    if (!is.null(input$links_table)) {
      links_values$data <- hot_to_r(input$links_table)
    }
  })
  
  #=============================================================================================================================
  
  # Render action buttons based on search input
  output$link_buttons <- renderUI({
    filtered_links <- df_links()[grep(input$search, df_links()$Hyperlink_Name, ignore.case = TRUE), ]
    
    if(nrow(filtered_links)>0) {
      tagList(
        lapply(1:nrow(filtered_links), function(i) {
          actionButton(
            inputId = paste0("link_", i),
            label = filtered_links$Hyperlink_Name[i],
            onclick = sprintf("window.open('%s', '_blank')", filtered_links$Url[i]),
            icon = icon("link"), style="color: #fff; background-color: #46505a; border-color: #46AD3E")
        })
      )
    } else {
      helpText(align = "center", "No Results")
    }
  })
  
  
  #=============================================================================================================================
  
  # UI Buttons
  output$ui_options1 <- renderUI({
    req(input$tabs %in%  c("1", "2"))
    
    splitLayout(
      prettyCheckboxGroup(inputId = "opt_101",label = "Quick Status", choices = my_status, selected = my_status[1:2], inline = TRUE, icon = icon("check"), bigger = TRUE, status = "success",animation = "jelly")
      # prettyCheckboxGroup(inputId = "in_filt3",label = "Quick Priority", choices = c("A", "B", "C"), selected = c("A", "B", "C"), inline = TRUE, icon = icon("check"), bigger = TRUE, status = "success",animation = "jelly")
    )
  })
  
  #=============================================================================================================================
  
  # [ DT ] :: Assigened Workstack
  output$dt01 <- DT::renderDataTable({
    datatable({
      df_main() %>%
        rename(Title = WorkTitle, Task = TasktTitle, Details = TaskDetails) %>% 
        mutate("Update Duration" = paste0(round(difftime(Sys.Date(), DateCreate, units = c("days"), tz = "Europe/London"))," DAYS"))
    },
    selection = "single",
    extensions = c('Buttons'),
    escape=FALSE, # Icon Visible
    options = list(pageLength = -1,
                   dom='fBtirpl', 
                   scrollX = T, 
                   fixedColumns = list(leftColumns = 1),
                   lengthMenu = list(c(15, 30, 50, 100, -1), c('15','30', '50', '100', 'All')),
                   buttons = list(list(extend = 'colvis', text='Selected Columns'),
                                  list(extend = 'colvisGroup', text = "Show all", show = ":hidden"),
                                  list(extend = 'colvisGroup', text = "Show none", hide = ":visible"),
                                  c('copy', 'csv', 'excel')),
                   columnDefs = list(list(visible=FALSE, targets=c(0,5,6,7,8)))
    ),
    rownames= FALSE, style = "bootstrap") %>%
      formatDate("Modified", method = 'toLocaleString', params = list('fr-FR')) %>%
      formatDate(c("ReviewDate","DateCreate"), method = 'toLocaleDateString', params = list('fr-FR')) %>%
      formatStyle("ReviewDate", backgroundColor = styleInterval(Sys.Date()-1, c("#F9C095","#9EDB6F"))) %>% 
      formatStyle("Title", "Status", color=styleEqual(my_status, c("green", "orange", "red")))
  })
  
  
  # Popup DT
  observeEvent(input$dt01_rows_selected, {
    selected_row <- input$dt01_rows_selected
    
    df <- df_main() %>% slice(input$dt01_rows_selected)
    click1_item(df) # Update Filter Val
    
    showModal(modalDialog(
      title = "Update Task",
      easyClose = TRUE,
      fluidPage(
        fluidRow(
          column(12, textInputIcon(inputId = "up_1", label = "Work Title:", value = df$WorkTitle, placeholder = "Brief Title...", icon = icon("book"), width = "90%"))
        ),
        fluidRow(
          column(6, textInput("up_2", "Task Title:", value = df$TasktTitle)),
          column(6, airDatepickerInput(inputId='up_3',label = uiOutput("ui_text_01"), value = df$ReviewDate, firstDay = 1, dateFormat = "dd-MM-yyyy", autoClose = TRUE))
        ),
        fluidRow(
          column(12, textAreaInput("up_4", "Task Details:", value = df$TaskDetails, rows = 5, width = "100%"))
        ),
        fluidRow(
          column(4, selectInput("up_5", "Status", choices = my_status, selected = df$Status))
          # column(4, sliderInput("up_9", "Completed:", min = 0, max = 100, value = 0, post = "%", step = 10, width = "100%"))
        ),
        fluidRow(
          column(6,
                 textInputIcon(inputId = "up_7", label = "Next Step", value = df$NextStep, icon = icon("forward"), width = "100%")
          ),
          column(6,
                 textInputIcon(inputId = "up_8", label = "Blocker", value = df$NextStep, icon = icon("ban"), width = "100%")
          )
        ),
        hr(),
        fluidRow(
          column(6,
                 textInputIcon(inputId = "up_6", label = "Primary Contact (Teams Link)", value = df$PrimaryContact, placeholder = "Email Address", icon = icon("envelope"), width = "100%")
          ),
          column(4,
                 div(style = "margin-top: 25px;", 
                     actionButton("in_p1_teams", "Open Teams", icon = icon("comments"), style="color: #fff; background-color: #46505a; border-color: #51a5059")
                 )
          )
        )
      ),
      footer = tagList(
        actionButton("go_update_task", "Update Task", icon = icon("plus")),
        actionButton("go_close_task", "Close Task", icon = icon("equals")),
        modalButton("cancel", icon = icon("times"))
      )
    ))
  })
  
  
  # Update Task - Go 
  observeEvent(input$go_update_task, {
    
    df0 <- readRDS(paste0(my_dir, "hub_main.rds"))
    df1 <- df0 %>% filter(Id %ni% click1_item()$Id)
    df2 <- click1_item()
    
    df2$WorkTitle <- input$up_1
    df2$TasktTitle <- input$up_2
    df2$ReviewDate <- input$up_3
    df2$TaskDetails <- input$up_4
    df2$Status <- input$up_5
    df2$Modified <- Sys.time()
    df2$PrimaryContact <- input$up_6
    df2$NextStep <- input$up_7
    df2$Blocker <- input$up_8
    
    df3 <- rbind(df1,df2) %>% arrange(desc(Id))
    
    # Save
    if(nrow(df3)>0){
      saveRDS(df3, paste0(my_dir, "hub_main.rds"))
    }
    
    fun_refresh() # [ Refresh ]
    removeModal()
    shinyalert("Updated", "", type = "success", timer = 1000, animation = "slide-from-bottom")
  })
  # Close Task - Go 
  observeEvent(input$go_close_task, {
    
    df1 <- df_main() %>% filter(Id %ni% click1_item()$Id)
    df2 <- click1_item()
    
    df2$Status = "Closed"
    
    df3 <- rbind(df1,df2) %>% arrange(desc(Id))
    
    # Save
    if(nrow(df3)>0){
      saveRDS(df3, paste0(my_dir, "hub_main.rds"))
    }
    
    fun_refresh() # [ Refresh ]
    removeModal()
    shinyalert("Closed", "", type = "success", timer = 1000, animation = "slide-from-bottom")
  })
  
  
  #=============================================================================================================================
  
  # New Requests
  observeEvent(input$bt_sh_new_request, {
    showModal(modalDialog(
      title = "Add New Task",
      easyClose = FALSE,
      fluidPage(
        fluidRow(
          column(12, textInputIcon(inputId = "in_1", label = "Work Title:", value = "", placeholder = "Brief Title...", icon = icon("book"), width = "90%"))
        ),
        fluidRow(
          column(6, textInput("in_2", "Task Title:", "")),
          column(6, airDatepickerInput(inputId='in_3',label = "Due Date", value = Sys.Date()+1, firstDay = 1, dateFormat = "dd-MM-yyyy", autoClose = TRUE))
        ),
        fluidRow(
          column(12, textAreaInput("in_4", "Task Details:", "", rows = 5, width = "100%"))
        ),
        fluidRow(
          column(4, selectInput("in_5", "Status:", choices = my_status))
        ),
        fluidRow(
          column(6, textInputIcon(inputId = "in_6", label = "Primary Contact (Teams Link)", value = "", placeholder = "Email Address", icon = icon("envelope"), width = "100%")
          )
        )
      ),
      footer = tagList(
        actionButton("go_add_task", "Add Task", icon = icon("plus")),
        modalButton("cancel", icon = icon("times"))
      )
    ))
  })
  
  # Add New Task
  observeEvent(input$go_add_task,{
    
    df1 <- readRDS(paste0(my_dir, "hub_main.rds"))
    
    # Create an empty data frame with specified columns
    df2 <- data.frame(
      Id = max(df1$Id)+1,
      WorkTitle = input$in_1,
      TasktTitle = input$in_2,
      ReviewDate = input$in_3,
      TaskDetails = input$in_4,
      Status = input$in_5,
      DateCreate = Sys.Date(),
      Modified = Sys.time(),
      PrimaryContact = input$in_6,
      NextStep = NA,
      Blocker = NA
    )
    
    df3 <- rbind(df1,df2) %>% arrange(desc(Id))
    
    # Save
    if(nrow(df3)>0){
      saveRDS(df3, paste0(my_dir, "hub_main.rds"))
    }
    
    fun_refresh() # [ Refresh ]
    print("New Task Added")
    removeModal()
    shinyalert("", "", type = "success", timer = 1000, animation = "slide-from-bottom")
  })
  
  #=============================================================================================================================
  
  # Timevis Function
  output$ui_timvis_1 <- renderTimevis({  
    # Time Vis Set Defaults
    my_config <- list(start = Sys.Date()-7, end = Sys.Date()+7)
    
    df_main() %>% 
      mutate(style = if_else(Status == my_status[1], paste0("background-color: ","#3dd413","; color: #000000;"),
                             if_else(Status == my_status[2], paste0("background-color: ","#db630d","; color: #000000;"),
                                     if_else(Status == my_status[3], paste0("background-color: ","#e02424","; color: #000000;"),
                                             paste0("background-color: ","#4091cf","; color: #000000;"))))) %>% 
      mutate(start = date(DateCreate), end = ReviewDate, id = Id, content = WorkTitle) %>% 
      timevis(options = my_config)
  })
  
  observeEvent(input$bt_show_tvs1, { fitWindow("ui_timvis_1") })
  observeEvent(input$default_tvs1, { setWindow("ui_timvis_1", Sys.Date()-14, Sys.Date()+14) })
  observeEvent(input$today_tvs1, { setWindow("ui_timvis_1", Sys.Date()-1, Sys.Date()+2) })
  
  # Table Out
  output$dt_timvis_1 <- renderTable({
    df_main() %>% 
      select(Title = WorkTitle, Task = TasktTitle, NextStep, Blocker, Status)
  })
  
  
  #=============================================================================================================================
  
  # Teams Chat - Primary
  observeEvent(input$in_p1_teams, {
    p1 <- click1_item()$PrimaryContact
    href <- paste0("https://teams.microsoft.com/l/chat/0/0?users=", p1)
    browseURL(href)
  })
  
  # [ ]============================================================================================================================[ ]
  
  # Text Colours
  output$ui_text_01 <- renderText({
    if(input$up_3 < Sys.Date()){
      return(paste("<span style=\"color:red\">Due Date</span>"))
    }else{
      return(paste("<span style=\"color:black\">Due Date</span>"))
    }
  })
  
  #=============================================================================================================================
  
}

# Run the application
shinyApp(ui = ui, server = server)
