setwd(dirname(parent.frame(2)$ofile))

library(shiny)
library(dplyr)
library(tibble)
library(tidytext)
library(data.table)
library(tidyr)
library(tokenizers)
library(sentimentr)
library(magrittr)


raw_voc <- read.csv("www/data.csv")
      

# ORGANIZE DATA

# Create new column with question numbers
Question <- data.frame(Question = names(raw_voc))
setDT(Question, keep.rownames = TRUE)[]
Question <- mutate(Question, Question_Index = paste("Q", rn, sep="")) %>%
  select(Question_Index:Question)

# Tidy dataset into 2-column table of Questions and Answers  
voc <- raw_voc %>%
  gather(key = Question, value = Answer)

voc <- merge(Question, voc)

# remove periods from questions
voc$Question <- gsub("\\.", " ", voc$Question)
voc$Question <- paste(voc$Question, "?", sep="")
voc$Question <- gsub(" \\?", "?", voc$Question)

# find and remove answers that match dates and numbers








# Tokenize each Answer into words

voc$Words <- tokenize_words(voc$Answer)

# Count words and create column of word count

voc$WordCount <- sapply(voc$Words, length)
    
# For each question count # of Answers with word count > 4

voc <- voc %>% mutate(TextForm = ifelse(WordCount > 4, "Long", "Short"))

# For each question, calculate % of answers with word count > 4
# Filter only questions with 30+% answers word count > 4
# Save 1 dataframe of >4 word counts (long form answers), 1 dataframe of 4 or les word counts (other kinds of questions)

by_question <- voc %>% 
  group_by(Question_Index) %>%
  summarize(Question_Count = n())

by_long <- voc %>% 
  group_by(Question_Index, TextForm) %>%
  summarize(TextForm_Count = n())

voc <- merge(voc, by_question)

long_questions <- merge(by_question, by_long) %>%
  filter(TextForm == "Long") %>%
  mutate(Percent_Long = TextForm_Count / Question_Count) %>%
  filter(Percent_Long > 0.3) 

long_questions <-  merge(long_questions, voc) %>%
  select("Question", "Question_Index", "Answer", "WordCount")

long_questions <- as.tibble(long_questions)
    
# We now have a subset of long-form survey responses to mine for insights. Time to clean & transform the data. 

# split long_questions into one df per question:
question_dfs <- split(long_questions, long_questions$Question_Index)

# Run sentiment analysis and pull up the top 10 most positive sentences for each question:

getSent <- function(x) {
  sent <- sentiment(get_sentences(x$Answer))
  x <- rename(x, element_id = Number)
  sorted_x <- merge(x, sent, by="element_id") %>%
    select(-sentence_id) %>%
    arrange(desc(sentiment))
}
    
senti <- lapply(question_dfs, getSent)


# START OF: Create question summary data set ----
total_df <- rbindlist(senti)
by_question <- total_df %>%
  group_by(Question) %>%
  summarize(
    "Number of Responses" = n(),
    "Avg Word Count" = round(mean(WordCount), 0),
    "Avg Sentiment" = mean(sentiment)
  )


# Make Sure to Not overwrite any app code when pasting back into App.r
