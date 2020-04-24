
library(tidyverse)

# imagine tidy survey data with a subscale of an instrument with 4 variables c1-c4
test_data <- tibble::tribble(~c1, ~c2, ~c3, ~c4, 
                             1, 2, 2, 4, 
                             3, 4, 3, 4, 
                             NA_integer_, 4, 4, 3,
                             NA_integer_, NA_integer_, NA_integer_, 3)

# now you want to calculate rowMeans() but you have to use (na.rm = T) cause you 
# loose to many answers if you dont do this - how can you assure, that the results
# are plausible and valid anyway?
# solution: check how many of the items per person are missing - then you can 
# define a threshhold. this is a solution used in a tidyverse pipe: 

# vectorized function for na_counts()
na_count <- function(x){ sum(is.na(x)) }

# vectorized function for na_pct()
na_pct <- function(x){ sum(is.na(x))/length(x) }

# counting rowMeans() but then checking how many of the variables are actually 
# missing per person - this lets you exclude e.g. person with > 50% missings (in that case row 4)
test_data %>% 
  mutate(scale1_mean = rowMeans(select(., c1:c4), na.rm = T),
         scale1_na_count = pmap_dbl(list(c1, c2, c3, c4), lift_vd(na_count)), #the purrr-way
         scale1_na_prop = pmap_dbl(list(c1, c2, c3, c4), lift_vd(na_pct)))  #the purrr-way
