library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(DT)

# 创建一个datatable

options(shiny.sanitize.errors = FALSE)
header <- dashboardHeader(title = "snp Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sideBar",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem(
      "Genomic Variation", tabName = "tab2",
      menuItem("Variations by Region", tabName = "tab2_1", icon = icon("calendar-plus")),
      menuItem(
        "Variations in Gene", tabName = "tab2_2", icon = icon("keyboard")
      ),
      menuItem(
        "Variation by ID",
        tabName = "tab2_3",
        icon = icon("search")
      )
    ),
    menuItem(
      "Transcription factor",
      tabName = "tab3"
    )
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML("
      .my-column {
        margin-top: 25px;
      }
    "))),
  tabItems(
    tabItem(
      tabName = "home",
      #includeMarkdown("/srv/shiny-server/snpDB/About.md")
    ),
    tabItem(
      tabName = "tab2_1",
      fluidPage(
        titlePanel("Search for Variations by Region"),
        br(),
        fluidRow(
          column(
            width = 3,
            selectInput(
              "Chromsome",
              "Chromsome",
              choices = NULL,
              selected = NULL
            )
          ),
          column(
            width = 3,
            textInput("SP","Start position:"),
            
          ),
          column(
            width = 3,
            textInput("EP","End position:"),
            
          ),
          column(
            width = 3,
            div(actionButton(
              "submit_btn2_1",
              "Submit",
              class = "btn-primary"
            ),class="my-column")
          )
        ),
        br(),
        dataTableOutput("results2_1"),
        downloadButton("download2_1", "Download")
        
      )
    ),
 
    tabItem(
      tabName = "tab2_2",
      fluidPage(
        titlePanel("Search for Variations in Gene"),
        br(),
        fluidRow(
          column(
            width = 2,
            selectInput(
              "GSN",
              "Gene ID/Symbol/Name",
              choices = NULL,
              selected = NULL
            )
          ),
          column(

            width = 2,
            selectInput(
              "EffectMSU",
              "EffectMSU",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
              
            )
          ),
          column(
            width = 2,
            selectInput(
              "EffectRAP",
              "EffectRAP",
              choices = NULL,
              selected = NULL,
              multiple = TRUE
              
            )
          ),
          column(
            width = 2,
            textInput("Upstream", "Upstream (kb,  must <= 10 kb)"),
          ),
          column(
            width = 2,
            textInput("Downstream", "Downstream (kb,  must <= 10 kb)"),
            
          ),
          column(
            width = 2,
            div(actionButton(
              "submit_btn2_2",
              "Submit",
              class = "btn-primary"
            ),class="my-column")
          )
        ),
        br(),
        dataTableOutput("results2_2"),
        downloadButton("download2_2", "Download")
      )
    ),
    tabItem(
      
      tabName = "tab2_3",
      fluidPage(

        titlePanel("Search for Variation information by Variation ID:"),
        br(),
        fluidRow(
          column(
            width = 4,
            selectInput(
              "filter_col", 
              "Select columns to filter",
              choices = NULL
              ),
          ),
          column(
            width = 4,
            textInput("filter_val", "Enter the value to filter"),
            
          ),
      
          column(
            width = 4,
            div(actionButton(
              "submit_btn2_3",
              "Submit",
              class = "btn-primary"
            ),class="my-column")
          )
        ),
        br(),
        dataTableOutput("results2_3"),
        downloadButton("download2_3", "Download")
        
      )
      
    ),
    #Trans Factor
    tabItem(
      tabName = "tab3",
      fluidPage(
        titlePanel("Search for Regulation information by TFs:"),
        br(),
        fluidRow(
          column(
            width = 2,
            textInput(
              "tFs",
              "TFs :",
              value = "AT1G"
            )
          ),
          column(
            width = 2,
            textInput(
              "tFname",
              "TFname:",
              value = "SMB1"
            )
          ),
          column(
            width = 2,
            textInput(
              "annotation",
              "Annotation:",
              value = "Promoter"
            )
          ),
          column(
            width = 2,
            textInput("targetId", "TargetId:",value = "AT3G26125"),
          ),
          column(
            width = 2,
            textInput("symbol", "Symbol:",value = "ARF"),
            
          ),
          column(
            width = 2,
            div(actionButton(
              "submit_btn3",
              "Submit",
              class = "btn-primary"
            ),class="my-column")
          )
        ),
        br(),
        dataTableOutput("results3"),
        downloadButton("download3", "Download")
      )
    )

   )
  )
    
    



ui <- fluidPage(
  dashboardPage(
    header,
    sidebar,
    body
  )
)

# Define df outside server function
#Variation information file
df <- read_excel("E:\\RWork\\RiceDataBase\\var_example.xlsx")
#df2_2 <- read_excel("/srv/shiny-server/snpDB/genes_pos.xlsx")
#df <- read_excel("/srv/shiny-server/snpDB/var_example.xlsx")
#Strand file
df2_2<-read_excel("E:\\RWork\\RiceDataBase\\genes_pos.xlsx")
#Transcription and regulation file
#df3<-read_excel("E:\\RWork\\RiceDataBase\\data\\sub_smb_anno.xlsx")
df3<-read_excel("E:/RWork/RiceDataBase/data/sub_smb_anno.xlsx")


# 定义 server
server <- function(input, output) {


  #tab2_1
  {
    #更新selectInput
    observe({
      updateSelectInput(
        inputId = "Chromsome",
        choices = unique(df$CHROM),
      )
    })

    
    # 存储筛选结果
    filtered_data2_1 <- reactiveVal()
    
    # 当按下“提交”按钮时，筛选表格并更新结果
    observeEvent(eventExpr = input$submit_btn2_1, {
      
      #filter筛选结果出错需检查updateselectInput函数是否设置了selected选项
      filtered_data2_1(df %>%
                         filter(.data[['CHROM']]==input$Chromsome) %>%

                         filter(.data[['POS']] >= as.numeric(input$SP)) %>%
                         filter(.data[['POS']] <= as.numeric(input$EP)))
    })
    
    # 显示结果
    output$results2_1 <- renderDataTable({
      filtered_data2_1()
    },options = list(scrollX = TRUE, scrollY = "500px"))
    output$download2_1 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data2_1(), file)
      }
    )
    
  }
  
  #tab2_2
  {
    
    #print(colnames(df2_2))
    observe({
      updateSelectInput(
        inputId = "GSN",
        choices = unique(df2_2$GeneID),
        selected = df2_2[2][1]
      )
    }) 
    
    observe({
      updateSelectInput(
        inputId = "EffectMSU",
        choices = unique(df$EffectMSU),
      )
    })
    
    observe({
      updateSelectInput(
        inputId = "EffectRAP",
        choices = unique(df$EffectRAP),
      )
    })
    
    # 存储筛选结果
    filtered_data2_2 <- reactiveVal()
    
    observeEvent(eventExpr = input$submit_btn2_2, {
      row_num <- which(df2_2$GeneID == input$GSN)
      
      if(df2_2[row_num,"Strand"]==1)
      {
        left<-df2_2[row_num,"Start"] - as.numeric(input$Upstream)*1000
        right<-df2_2[row_num,"End"] + as.numeric(input$Downstream)*1000
        left<-unlist(left)
        left<-as.numeric(left)
        right<-unlist(right)
        right<-as.numeric(right)
        
        if ((is.null(input$EffectMSU) || input$EffectMSU[1] == "")
            &&(is.null(input$EffectRAP) || input$EffectRAP[1] == ""))
        {
          filtered_data2_2(df %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
        }
        
        else if (is.null(input$EffectMSU) || input$EffectMSU[1] == "")
        {
          filtered_data2_2(df %>%
                             filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
        }
        else if (is.null(input$EffectRAP) || input$EffectRAP[1] == "")
        {
          filtered_data2_2(df %>%
                             filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
          
        }
        else {
          filtered_data2_2(df %>%
                             filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                             filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
        }
        
        
      } else if(df2_2[row_num,"Strand"]==0) {
        left<-df2_2[row_num,"Start"] - as.numeric(input$Downstream)*1000
        right<-df2_2[row_num,"End"] + as.numeric(input$Upstream)*1000
        
        left<-unlist(left)
        left<-as.numeric(left)
        right<-unlist(right)
        right<-as.numeric(right)
        if ((is.null(input$EffectMSU) || input$EffectMSU[1] == "")
            &&(is.null(input$EffectRAP) || input$EffectRAP[1] == "")
        )
        {

          filtered_data2_2(df %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
        }
        else if (is.null(input$EffectMSU) || input$EffectMSU[1] == "")
        {
          filtered_data2_2(df %>%
                             filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
        }
        else if (is.null(input$EffectRAP) || input$EffectRAP[1] == "")
        {
          filtered_data2_2(df %>%
                             filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
          
        }else
        {
          filtered_data2_2(df %>%
                             filter(.data[['EffectMSU']]==input$EffectMSU) %>%
                             filter(.data[['EffectRAP']]==input$EffectRAP) %>%
                             filter(.data[['POS']] >= left) %>%
                             filter(.data[['POS']] <= right))
        }
        
        # filtered_data2_2(df %>%
        #                    #filter(CHROM==input$Chromsome) %>%
        #                    filter(.data[['POS']] >= left) %>%
        #                    filter(.data[['POS']] <= right))
        
        
      }
      
    })
    
    # 显示结果
    output$results2_2 <- renderDataTable({
      filtered_data2_2()
    },options = list(scrollX = TRUE, scrollY = "500px"))
    
    output$download2_2 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data2_2(), file)
      }
    )
    
  }
  
  
  #tab2_3
  {
    
    observe({
      updateSelectInput(
        inputId = "filter_col",
        choices = names(df),
        selected = names(df)[1]
      )
    })
    
    # 存储筛选结果
    filtered_data2_3 <- reactiveVal()
    
    # 当按下“提交”按钮时，筛选表格并更新结果
    observeEvent(eventExpr = input$submit_btn2_3, {
      
      filtered_data2_3(df %>% filter(.data[[input$filter_col]] == input$filter_val))
    })
    
    
    # 显示结果
    output$results2_3 <- renderDataTable({
      filtered_data2_3()
    },options = list(scrollX = TRUE, scrollY = "500px"))
    
    output$download2_3 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data2_3(), file)
      }
    )
    
  }
  #tab3
  {
    # 存储筛选结果
    filtered_data3 <- reactiveVal()
    # 当按下“提交”按钮时，筛选表格并更新结果
    observeEvent(eventExpr = input$submit_btn3, {
      TFs_data<-df3[grep(input$tFs, df3[['TFs']], ignore.case = TRUE),]
      TFname_data<-TFs_data[grep(input$tFname, TFs_data[['TFname']], ignore.case = TRUE),]
      Annotation_data<-TFname_data[grep(input$annotation, TFname_data[['Annotation']], ignore.case = TRUE),]
      TargetId_data<-Annotation_data[grep(input$targetId, Annotation_data[['TargetId']], ignore.case = TRUE),]
      Symbol_data<-TargetId_data[grep(input$symbol, TargetId_data[['Symbol']], ignore.case = TRUE),]
      
      filtered_data3(Symbol_data)
    })
    
    
    # 显示结果
    output$results3 <- renderDataTable({
      filtered_data3()
      
    },options = list(scrollX = TRUE, scrollY = "500px"))
    
    output$download3 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data2(), file)
      }
    )
  }
}
# 运行应用
shinyApp(ui, server)

