
# Loading in required libraries
library(tidyverse)

# Reading in the Nobel Prize data
nobel <- read_csv("datasets/nobel.csv")

# Taking a look at the first couple of winners
head(nobel,2)

library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("Test that tidyverse is loaded", {
    expect_true( "package:tidyverse" %in% search(), 
        info = "The tidyverse package should be loaded using library().")
    })
    
    test_that("Read in data correctly.", {
        expect_is(nobel, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/nobel.csv" into nobel.')
    })
    
    test_that("Read in data correctly.", {
        nobel_temp <- read_csv('datasets/nobel.csv')
        expect_equivalent(nobel, nobel_temp, 
            info = 'nobel should contain the data in "datasets/nobel.csv".')
    })
})

# Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
nobel %>% 
    count()

# Counting the number of prizes won by male and female recipients.
nobel %>%
    group_by(sex) %>%
    count(var = "sex")

# Counting the number of prizes won by different nationalities.
nobel %>%
    group_by(birth_country) %>%
    count(var = "birth_country") %>%
    arrange(desc(n)) %>%
    head(20)

last_value <- .Last.value

correct_value <- nobel %>%
    group_by(birth_country) %>%
    count()  %>% 
    arrange(desc(n))  %>% 
    head(20)

run_tests({
    test_that("That the by country count is correct", {
    expect_equivalent(last_value$birth_country[1], last_value$birth_country[1], 
        info = "The countries should be arranged by n, arrange(desc(n)), and the top country should be United States of America")
    })
})

# Calculating the proportion of USA born winners per decade
prop_usa_winners <- nobel %>% 
    mutate(birth_country, usa_born_winner = (birth_country == "United States of America")) %>%

# Display the proportions of USA born winners per decade
# .... YOUR CODE FOR TASK 3 ....
   mutate(year, decade = floor(year/10) *10) %>%
   group_by(decade) %>%
   summarize(proportion = mean(usa_born_winner, na.rm = TRUE))


correct_prop_usa_winners <- nobel %>% 
    mutate(usa_born_winner = birth_country == "United States of America",
           decade = floor(year / 10) * 10 ) %>% 
    group_by(decade) %>%
    summarize(proportion = mean(usa_born_winner, na.rm = TRUE))

run_tests({
    test_that("prop_usa_winners is calculated correctly", {
    expect_true("proportion" %in% names(prop_usa_winners) &
                all(prop_usa_winners$proportion %in% correct_prop_usa_winners$proportion), 
        info = "The column proportion in prop_usa_winners should contain the proportion of usa_born_winner by decade.")
    })
})

# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plotting USA born winners
# .... YOUR CODE FOR TASK 4 ....
ggplot(prop_usa_winners, aes(x = decade, y = proportion)) +
    geom_line() +
    geom_point()

p <- last_plot()

correct_p <- ggplot(prop_usa_winners, aes(decade, proportion)) +
    geom_line() + geom_point() +
    scale_y_continuous(labels = scales::percent, limits = 0:1, expand = c(0,0))

run_tests({
    test_that("The plot uses the correct dataset", {
    expect_equivalent(p$data, correct_p$data, 
        info = "The plot should show the data in prop_usa_winners.")
    })
    
    test_that("correct columns are plotted", {
        mappings <- str_replace(as.character(p$mapping), "~", "")
        expect_true(all(c("decade", "proportion") %in% mappings), 
            info = "You should plot decade on the x-axis, prop_female on the y-axis, and category should be mapped to color.")
    })
})

# Calculating the proportion of female laureates per decade
prop_female_winners <- nobel %>%
    mutate(sex, female_winner =(sex == "Female")) %>%
    mutate(year, decade = floor(year /10)*10) %>%
    group_by(decade, category) %>%
    summarize(proportion = mean(female_winner, na.rm = TRUE))

ggplot(prop_female_winners, aes(x = decade, y = proportion, col = category)) +
    geom_line() +
    geom_point()
# Plotting the proportion of female laureates per decade
# .... YOUR CODE FOR TASK 5 ....

p <- last_plot()

correct_prop_female_winners <- nobel %>%
    mutate(female_winner = sex == "Female",
           decade = floor(year / 10) * 10)  %>% 
    group_by(decade, category)  %>% 
    summarize(proportion = mean(female_winner, na.rm = TRUE))

run_tests({
    test_that("prop_female_winners$prop_female is correct", {
    expect_true(all(prop_female_winners$proportion %in% correct_prop_female_winners$proportion), 
        info = "prop_female_winners$prop_female need to have the proportion of female winners per decade.")
    })
    
    test_that("correct columns are plotted", {
    mappings <- str_replace(as.character(p$mapping), "~", "")
    expect_true(all(c("decade", "proportion", "category") %in% mappings), 
        info = "You should plot decade on the x-axis, prop_female on the y-axis, and category should be mapped to color.")
    })
})

# Picking out the first woman to win a Nobel Prize
nobel %>%
    filter(sex == "Female") %>%
    arrange(year) %>%
    head(1)

last_value <- .Last.value

correct_last_value <- nobel %>%
    filter(sex == "Female")  %>% 
    top_n(1, desc(year))

run_tests({
    test_that("Marie Curie is picked out.", {
    expect_equivalent(last_value$full_name, correct_last_value$full_name,
        info = "You should pick out the first woman to win a Nobel Prize. Hint: Her first name was Marie.")
    })
})

# Selecting the laureates that have received 2 or more prizes.
nobel %>%
    group_by(full_name) %>%
    count(var = "full_name") %>%
    filter(n > 1)

last_value <- .Last.value

correct_last_value <- nobel %>%
    group_by(full_name)  %>% 
    count()  %>% 
    filter(n >= 2)

run_tests({
    test_that("The right winners were selected", {
    expect_equivalent(sort(last_value$full_name), sort(correct_last_value$full_name),
        info = "You should filter away everybody that didn't win the prize at least twice.")
    })
})

# Loading the lubridate package
# .... YOUR CODE FOR TASK 8 ....
library(lubridate)
birth <- as.Date(nobel$birth_date)
# Calculating the age of Nobel Prize winners
nobel_age <- nobel %>%
    mutate(age = year - year(birth)) 

ggplot(nobel_age, aes(x = age, y = year) ) +
    geom_point() +
    geom_smooth()


# Plotting the age of Nobel Prize winners
# .... YOUR CODE FOR TASK 8 ....

p <- last_plot()

correct_nobel_age <- nobel %>%
    mutate(age = year - year(birth_date))

run_tests({
    test_that("nobel_age$age is correct", {
    expect_true(all(nobel_age$age %in% correct_nobel_age$age), 
        info = "nobel_age$age need to have the age of the winner when they received the prize.")
    })
    
    test_that("correct columns are plotted", {
    mappings <- str_replace(as.character(p$mapping), "~", "")
    expect_true(all(c("year", "age") %in% mappings), 
        info = "You should plot year on the x-axis and age on the y-axis.")
    })
})

# Same plot as above, but faceted by the category of the Nobel Prize
# .... YOUR CODE FOR TASK 9 ....
ggplot(nobel_age, aes(x = age, y = year)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(~category)


facet_class <- class(last_plot()$facet)

run_tests({
    test_that("The plot is faceted", {
    expect_true(! "FacetNull" %in% facet_class, 
        info = "The plot needs to be faceted by category. Try using facet_wrap().")
    })
})

# The oldest winner of a Nobel Prize as of 2016
nobel_age %>% 
    top_n(1, age)
    

# The youngest winner of a Nobel Prize as of 2016

    
nobel_age %>%
    arrange(age) %>%
    head(1)

last_value <- .Last.value
correct_last_value <- nobel_age %>% top_n(1, desc(age))

run_tests({
    test_that("The youngest winner is correct", {
    expect_equivalent(last_value$full_name, correct_last_value$full_name,
        info = "The last row you extract in the code cell should be the youngest Nobel Prize winner.")
    })
})

# The name of the youngest winner of the Nobel Prize as of 2016
youngest_winner <- "Malala Yousafzai"

run_tests({
    test_that("youngest_winner is correct", {
    expect_true(any(str_detect(tolower(youngest_winner), c("malala", "yousafzai"))), 
        info = "youngest_winner should be a string. Try writing only the first / given name.")
    })
})
