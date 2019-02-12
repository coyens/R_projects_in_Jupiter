
# Loading necessary packages
# .... YOUR CODE FOR TASK 1 ....
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
# Loading the data
responses <- read_csv(file = "datasets/kagglesurvey.csv")

# Printing the first 10 rows
head(responses, 10)

library("testthat")
library('IRkernel.testthat')

run_tests({
    test_that("Read in data correctly.", {
        expect_is(responses, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/kagglesurvey.csv" into responses')
    })
    
    test_that("Read in data correctly.", {
        responses_test <- read_csv('datasets/kagglesurvey.csv')
        expect_equivalent(responses, responses_test, 
            info = 'responses should contain the data in "datasets/kagglesurvey.csv"')
    })
    
})

# Printing the first respondents' tools and languages
# .... YOUR CODE FOR TASK 2 ....



# Creating a new data frame called tools
tools <- responses[, 2]

# Adding a new column to tools which splits the WorkToolsSelect column at the commas and unnests the new column
tools <-  tools %>% 
    mutate(work_tools = strsplit(as.character(WorkToolsSelect), ",")) %>% 
    unnest(work_tools)

# Viewing the first 6 rows of tools
# .... YOUR CODE FOR TASK 2 ....
head(tools,6)

run_tests({
    test_that("Tools and Languages were Split and Unnested", {
        expect_true(nrow(tools) == 47409, 
            info = 'Make sure that you split the tools at the commas and unnested them')
    })
    
    test_that("Tools and Languages were Unnested", {
        expect_is(tools$work_tools, "character", 
            info = 'The work_tools column should be of class "character". Make sure that you unnested the results of strsplit()')
    })
    
})

tool_count <- tools 

tool_count <- tool_count %>%
    group_by(work_tools) %>%
    summarise(number = n()) %>%
    arrange(desc(number))

head(tool_count, 6)

run_tests({
    test_that("Tools were Grouped and Summarised", {
        expect_true(nrow(tool_count) == 50, 
            info = 'Make sure that you grouped by tools and then summarised')
    })
    
    test_that("Values were sorted correctly", {
        expect_true(tool_count[1, 2] == 6073, 
            info = 'Do not forget to sort your tool counts from largest to smallest')
    })
    
})

# Creating a bar chart of the work_tools column. 
# Arranging the bars so that the tallest are on the far right
ggplot(tool_count,aes(x = reorder(work_tools,  -number), number)) + 
    geom_bar( stat="identity") +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust= 1))

run_tests({
   test_that("Plot is a bar chart",{
      p <- last_plot()
      q <- p$layers[[1]]
      expect_is(q$geom, "GeomBar", 
                info = "You should plot a bar chart with ggplot")
    })
})


# Creating a new data frame called debate_tools
debate_tools <- responses

# Creating a new column called language preference, based on the conditions specified in the Instructions
debate_tools <- debate_tools  %>% 
   mutate(language_preference = case_when(
   grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "R",
   grepl("Python", WorkToolsSelect) & ! grepl("R", WorkToolsSelect)~ "Python",
   grepl("Python", WorkToolsSelect) & grepl("R", WorkToolsSelect)~ "both",
   ! grepl("Python", "R", WorkToolsSelect)~ "neither"))

# Printing the first 6 rows
# .... YOUR CODE FOR TASK 5 ....
head(debate_tools, 6)

run_tests({
    test_that("New column was created", {
        expect_is(debate_tools$language_preference, "character", 
            info = 'The language_preference column should be of class "character". Make sure that you filled this new column correctly')
    })
    
})

# Creating a new data frame
debate_plot <- debate_tools

# Grouping by language preference and calculate number of responses
debate_plot <- debate_plot  %>% 
   group_by(language_preference)  %>% 
   summarise( number_of_language = n()) %>%

# Removing the row for users of "neither"
    filter(language_preference != "neither")
head(debate_plot)
# Creating a bar chart
# .... YOUR CODE FOR TASK 6 ....
ggplot(debate_plot, aes(language_preference, number_of_language)) +
    geom_bar(stat="identity")

run_tests({
   test_that("Plot is a bar chart",{
      p <- last_plot()
      q <- p$layers[[1]]
      expect_is(q$geom, "GeomBar",
               info = "You should plot a bar chart with ggplot")
    })
})

# Creating a new data frame
recommendations <- debate_tools

# Grouping by language_preference and then LanguageRecommendationSelect
recommendations <- recommendations  %>% 
    group_by(language_preference, LanguageRecommendationSelect)  %>% 
    summarise(number_of_recomendation = n()) %>%
    filter(!is.na(LanguageRecommendationSelect)) %>%
    arrange(desc(number_of_recomendation)) %>%
    mutate(LP = row_number()) %>%
    filter(LP < 5)
# Removing empty responses and include the top recommendations
# .... YOUR CODE FOR TASK 7 ....
head(recommendations,6)

run_tests({
    test_that("Tools have been summarised", {
        expect_true(nrow(recommendations) == 16, 
            info = 'Make sure that you are only keeping the top 4 responses for each language used')
    })
    
})

# Creating a faceted bar plot
ggplot(recommendations, aes(LanguageRecommendationSelect, number_of_recomendation)) +
    geom_bar(stat="identity") +
    facet_wrap(~ language_preference)
    

run_tests({
   test_that("Plot is a bar chart",{
      p <- last_plot()
      q <- p$layers[[1]]
      expect_is(q$geom, "GeomBar")
    })
})

# Would R users find this statement TRUE or FALSE?
R_is_number_one = TRUE

run_tests({
    test_that("The question has been answered", {
        expect_true(R_is_number_one, 
            info = 'Try again! Should R_is_number_one be set to TRUE or FALSE?')
    })
    
})
