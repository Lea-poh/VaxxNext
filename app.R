
library(shiny)
library(DT)


# setwd("~/Documents/Privat/apps/Vaxxnext/")

# source("functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel( div(column(width = 4, tags$img(src = "VaxxNext_logo.jpg",height = '80', width ='80')), 
                  column(width = 6, h2("Vaxx Next?"))),
              windowTitle="MyPage"
  ),
  # tags$img(src = "VaxxNext_logo.jpg"),
  # titlePanel("Vaxx Next?"),
  fluidRow(
    column(width = 12,
           # textOutput("text_Test"),
           # tableOutput("df_Test"),
           h3("Summary"),
           uiOutput("User_Info"),
           br(),
           DT::dataTableOutput("summaryTable"),
           br(),
           br(),
           br(),
           conditionalPanel(condition = "typeof input.summaryTable_rows_selected  !== 'undefined' && input.summaryTable_rows_selected.length > 0",
                uiOutput("header_selected_vaccine"),
                br(),
                # tags$head(tags$style("#header_selected_vaccine{color: black; font-size: 25px; font-style: bold}")),
                DT::dataTableOutput("selected_vaccine_table"),
                # conditionalPanel(condition = "typeof input.selected_vaccine_table_rows_selected  !== 'undefined' && input.selected_vaccine_table_rows_selected.length > 0",
                #   div(style="display: inline-block;vertical-align:middle; width: 300px;",actionButton("change_date","Correct selected date")),
                #   div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("delete","delete selected date"))
                # ),
                br(),
                div(style="display: inline-block;vertical-align:middle; width: 300px;",dateInput("new_vaccination_date","Date of new vaccination")),
                div(style="display: inline-block;vertical-align:middle; width: 150px;",actionButton("Vaccinate","Vaccinate"))
           )

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  orange_status_after_n_days <- 200
  data_path <- ""
  
  output$User_Info <- renderUI({
    if(file.exists(paste0(data_path,"User_Info.Rds"))){
      Info <- readRDS(paste0(data_path,"User_Info.Rds"))
      a <- paste0("<span font-size:100%> Name: ",Info$Name , "<br> Date of birth: ",Info$date_of_bith,"</span>")
      HTML(a)
    }else{
      tagList(
        textInput("Name", "Please enter your name."),
        dateInput("date_of_birth","Please enter your date of birth.")
      )
    }
  })
  
  vaccination_timetable <- read.csv(paste0(data_path,"Vaccination_timetable_2.csv"))
  
  dt <- read.csv(paste0(data_path,"VN.csv"), na.strings = "")
  dt <- apply(dt,2,function(x){
    out <-  na.omit(x)
    out <- as.Date(out,format = "%Y-%m-%d")
    return(out)
    })

  # if(file.exists(paste0(data_path,"my_Vaccinations.Rds"))){
  #   dt <- readRDS(paste0(data_path,"my_Vaccinations.Rds"))
  # }else{
  #   dt <- vector("list", length(colnames(vaccination_timetable)[-1]))
  #   names(dt) <- colnames(vaccination_timetable)[-1]
  # }
  vaccination_data <- reactiveValues(vaccinations=dt)
  
  
  vaccinations <- reactive({
    vaccination_data[["vaccinations"]]
  })
  
  output$text_Test <- renderText({
    status()
  })
  
  output$df_Test <- renderTable({
    status()
  })
  
  vaccine_names <- reactive({names(vaccinations())})
  
  last_vaccination <- reactive({
    unlist(lapply(vaccinations(),function(x){
      out <- tail(x,1)
      if(length(out) == 0) return(NA)
      else return(out)
    }))
  })
  
  next_vaccination <- reactive({
    unlist(last_vaccination()) + 356.25
  })
  
  status <- reactive({
    status <- rep("ok",length = length(last_vaccination()))
    status[(next_vaccination() - orange_status_after_n_days) < Sys.Date()] <- "soon"
    status[next_vaccination() < Sys.Date()] <- "overdue"
    status
  })
  
  out_table <- reactive({
    out_table <- data.frame(vaccine_names(), last_vaccination(),next_vaccination(),status())
    colnames(out_table) <- c("Name","Last Vaccination","Next Vaccination","Status")
    out_table <- out_table[order(out_table$'Next Vaccination'),]
    out_table$'Last Vaccination' <- as.character(as.Date(out_table$'Last Vaccination', origin = '1970-01-01'))
    out_table$'Next Vaccination' <- as.character(as.Date(out_table$'Next Vaccination', origin = '1970-01-01'))
    out_table
  })
  
  output$summaryTable <- DT::renderDataTable({
    datatable(out_table(), selection = 'single', rownames = FALSE )%>%formatStyle("Status",
            backgroundColor=styleEqual(c("ok","soon","overdue"), c("green","orange","red")))
    # datatable(out_table)
    
  })
  
  selected_vaccine <- reactive({
    out_table()$Name[input$summaryTable_rows_selected]
  })
  
  output$header_selected_vaccine <- renderUI({
    status_table <- data.frame(status = c("ok","soon","overdue"), color = c("green","orange","red"))
      current_vaccine_status <- out_table()$Status[input$summaryTable_rows_selected]
      status_color <- status_table$color[status_table$status == current_vaccine_status]
      a <- paste0("<span style=color:",status_color,";font-size:200%>", selected_vaccine(), "</span>")
      HTML(a)
  })
  
  current_vaccine <- reactive({
    if(!is.null(input$summaryTable_rows_selected)){
      if(length(vaccinations()[[selected_vaccine()]]) != 0){
        current_vaccine <- as.Date(vaccinations()[[selected_vaccine()]], origin = '1970-01-01')
        current_vaccine <- data.frame(n = 1:length(current_vaccine), Date = as.character(current_vaccine))
        current_vaccine <- current_vaccine[order(nrow(current_vaccine):1),]
      }else{
        # current_vaccine <- as.data.frame(matrix(1:4,nrow=2, ncol = 2))
        # colnames(current_vaccine) <- c("n","Date")
        current_vaccine <- NULL
      } 
    }
  })

  output$selected_vaccine_table <- DT::renderDataTable(
    current_vaccine(), options = list(pageLength = 5),
    selection = 'single', rownames = FALSE
  )
  
  observeEvent(input$Vaccinate, {
    dt<-reactive({
      req(input$new_vaccination_date)
      dt <- vaccination_data[["vaccinations"]]
      dt[[selected_vaccine()]] <- c(unlist(dt[selected_vaccine()]),input$new_vaccination_date)
      return(dt)
    })
    saveRDS(dt(),paste0(data_path,"my_Vaccinations.Rds"))
    vaccination_data[["vaccinations"]] <- dt()
  })
  
  observeEvent(input$date_of_birth, {
    if(!is.null(input$date_of_birth) & !is.null(input$Name)){
    Info <- list(Name = input$Name ,date_of_bith = input$date_of_birth)
    saveRDS(Info,paste0(data_path,"User_Info.Rds"))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
