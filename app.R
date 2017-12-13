library(shiny)
library(dplyr)
library(tibble)
library(tidytext)
library(data.table)
library(tidyr)
library(tokenizers)
library(sentimentr)
library(magrittr)



# Display long-form question,, average wordcount, # of responses each upon upload

# Define UI for data upload app ----
ui <- fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  # App title ----
  titlePanel("Survey Response Analyzer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
 
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Action button ----
      actionButton("do", "Get Top Positive Responses")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(id = "tabs",
       tabPanel("Overview", value = "panel1",
                uiOutput("overview")
                ),
       tabPanel("Question Summary", value = "panel2",

                 # Output: Data file ----
                uiOutput("questions")
              )
      )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
    output$overview <- renderUI(
        tagList(
          tags$br(),
          tags$br(),
          tags$h4("What This App Does (and Why):"),
          tags$p("This app will takes a plain CSV file of customer feedback responses, detects and extracts all the long-form questions in the survey, then displays the top 10 most positive-rated responses per question in a separate tab."),
          tags$p("This app is designed to help marketers and copywriters quickly zero-in on the most useful customer feedback responses to use for testimonials and sales copywriting later."),
          tags$br(),
          tags$h4("Important Note:"), 
          tags$p("Currently the sentiment analysis of these survey responses isn't great yet - there output of top 'positive' responses is still pretty mixed. In future iterations I'd like to incorporate a more sophisticated, machine-learning-based algorithm to more accurately detect positive and negative reponses."),
          tags$br(),
          tags$h4("Getting Started:"), 
          tags$p(
            tags$strong("Step 1:"),
            "Download the demo file by clicking the button below."
          ),
          tags$p(
            tags$strong("Step 2:"),
            "Upload the CSV file in the left sidebar (upper left corner)."
          ),
          tags$p(
            tags$strong("Step 3:"),
            "Click the button 'Get Top Positive Responses' at the bottom left corner."),
          tags$br(),
          tags$p("In the tab called 'Question Summary', the app will list all the questions that resulted in long-form responses (i.e. more than 5 words), and the average word count and the average sentiment rating for each."),
          tags$p("The app will also create separate tabs for each long-form question and display the top 10 responses with the highest sentiment rating (i.e. the most positive responses, in theory)."),
          tags$br(),
          tags$a(href = 'data.csv', class = "btn", icon("download"), 'Download Demo File')
        )
      )
  
    output$contents <- renderTable({
      req(input$file1)
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      raw_voc <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote,
                     na.strings = c("", " ", "None", "n/a")
                     )
      
      
      if(input$disp == "head") {
        return(head(raw_voc))
      }
      else {
        return(raw_voc)
      }
  
    })
  
  # User clicks 'Get Positive Responses' ----
  
  observeEvent(input$do, {

    raw_voc <- read.csv(input$file1$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote, 
                        na.strings = c("", " ", "None", "n/a")
                        )
    # start of code that processes the data
    
    ### COPY PASTE START -------    
    
    # ORGANIZE DATA
    
    # Create new df with question numbers as index
    Question <- data.frame(Question = names(raw_voc))
    setDT(Question, keep.rownames = TRUE)[]
    Question <- mutate(Question, Question_Index = as.numeric(rn)) %>%
      select(Question_Index:Question)
    
    # Turn rownames into element ids
    setDT(raw_voc, keep.rownames = TRUE)[]
    raw_voc <- rename(raw_voc, "element_id" = rn)
    
    # Tidy dataset into 2-column table of Questions and Answers  
    voc <- raw_voc %>%
      gather(key = Question, value = Answer, -element_id)
    
    voc <- merge(Question, voc)
    
    # remove periods from questions, add question marks at end
    voc$Question <- gsub("\\.", " ", voc$Question)
    voc$Question <- paste(voc$Question, "?", sep="")
    voc$Question <- gsub(" \\?", "?", voc$Question)
    
    
    # Tokenize each Answer into words
    
    voc$Words <- tokenize_words(voc$Answer)
    
    # Count words and create column of word count
    
    voc$WordCount <- sapply(voc$Words, length)
    
    
    # For each question, determine the mean and sd. Drop any questions with a word count mean of < 4 AND sd < 4 
    
    by_question <- voc %>% 
      group_by(Question_Index) %>%
      summarize(Question_Count = n(), 
                WC_Mean = mean(WordCount),
                WC_StatDev = sd(WordCount)
      )
    
    by_question <- by_question %>%
      mutate(TextForm = ifelse((WC_Mean > 3 & WC_StatDev > 3), "Long", "Short"))
    
    voc <- merge(voc, by_question)
    
    long_questions <- voc %>%
      filter(TextForm == "Long") %>%
      select("element_id", "Question", "Question_Index", "Answer", "WordCount")
    
    long_questions <- as.tibble(long_questions)
    
    
    # We now have a subset of long-form survey responses to mine for insights. Time to clean & transform the data. 
    
    # split long_questions into one df per question:
    question_dfs <- split(long_questions, long_questions$Question_Index)
    
    # Run sentiment analysis and pull up the top 10 most positive sentences for each question:
    
    getSent <- function(x) {
      sent <- sentiment(get_sentences(x$Answer))
      sorted_x <- merge(x, sent, by="element_id") %>%
        select(-sentence_id) %>%
        arrange(desc(sentiment))
    }
    
    senti <- lapply(question_dfs, getSent)
    
    ### COPY PASTE END -------
    
    # Make Sure to Not overwrite any app code when pasting back into App.r
    
    
    # START OF: Create question summary data set ----
    total_df <- rbindlist(senti)
    by_question <- total_df %>%
      group_by(Question) %>%
      summarize(
        "Number of Responses" = n(),
        "Avg Word Count" = round(mean(WordCount), 0),
        "Avg Sentiment" = mean(sentiment)
      )
    
    by_question$TabID <- paste("Q", rownames(by_question), sep="")
    
    
    ntabs <- length(by_question$TabID)
    
    
    updateTabsetPanel(session, "tabs",
                      selected = "panel2")
     
    output$questions <- renderUI({
      output$answer1 <- renderDataTable(by_question)
      tagList(
        tags$h3("Long-Form Questions in this Survey"),
        tags$div(
          dataTableOutput("answer1")
        )
      )
    })
    
    # END OF: Create question summary data set ----
    
    # START OF: Create top10 data sets -----
    
    
   
    # Dynamically append a tab
    lapply(1:length(senti), 
           function(i) {
             insertTab(inputId = "tabs",
                tabPanel(by_question$TabID[i], 
                         value=paste("panel", 2+i, sep=""), 
                         renderUI(
                           tagList(
                             tags$h3(unique(senti[[i]]$Question)),
                             tags$div(
                               renderDataTable(head(select(senti[[i]], c("Top 10 Positive Answers" = Answer, "Sentiment" = sentiment)),10))
                             )
                           )
                         ) 
                        ),
                target = paste("panel", 1+i, sep=""),
                position = "after"
      )}
    
    )
  })
}


# END OF: Create top10 data sets -----


# Run the app ----
shinyApp(ui, server)