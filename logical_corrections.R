library(tidyverse)
set.seed(12345)

# an excerpt of the original dataframe
# df_orig <- tibble::tibble(ARCHIV = c(17, 233, 442), 
#                        ABTEIL = c(123010, 309011, 145013),
#                        E30A = c(0, 0, 0),
#                        E30B = c(1, 0, 0),
#                        E30C = c(0, 1, 0),
#                        E30TEXT = c(NA_character_, "cleaning", "janitor"))

# # an excerpt of the original dataframe
df_orig <- tibble::tibble(ARCHIV = c(17, 233, 442),
                          ABTEIL = c(123010, 309011, 145013),
                          E30A = c(2, 2, 2),
                          E30B = c(1, 2, 2),
                          E30C = c(2, 1, 2),
                          E30TEXT = c(NA_character_, "cleaning", "janitor"))


# apparently, based on the information given, they should have
# crossed with cleaning E30A and with janitor E30B and E30C
df_orig

# this captures the text datafile which will manually be added
df_text <- tibble::tibble(ARCHIV = c(17, 233, 442), 
                         ABTEIL = c(123010, 309011, 145013),
                         E30TEXT = c(NA_character_, "cleaning", "janitor"), 
                         E30corr = c(NA_character_, "E30A", "E30B, E30C")) 
df_text

# using left_join()
# by ARCHIVE, ABTEIL and E30TEXT
# E30corr was added, still E30A-E30C hold the "old version"
df <- tidylog::left_join(df_orig, df_text, by = c("ARCHIV", "ABTEIL", "E30TEXT"))

df

# what to do?
# 1. make copies of all original variables E30A-E30Ccorr with the suffix "orig"
df %>% 
  mutate_at(.vars = vars(E30A, E30B, E30C, E30corr), 
            .funs = list(orig = ~.))

# 2. Split E30corr to a nested list with vector of column names inside the datframe
df %>% 
  mutate_at(.vars = vars(E30A, E30B, E30C, E30corr), 
            .funs = list(orig = ~.)) %>% 
  mutate(E30corr = strsplit(E30corr, split = ", ", fixed = TRUE))

# 3. Use pivot_longer and mutate/map to check if the variable name is mentioned in the 
# E30corr-splitted text variable, if so, change the value to 1
df %>% 
  mutate_at(.vars = vars(E30A, E30B, E30C, E30corr), 
            .funs = list(orig = ~.)) %>% 
  mutate(E30corr = strsplit(E30corr, split = ", ", fixed = TRUE)) %>% 
  pivot_longer(E30A:E30C)

# 4. Use pivot_wider to re-format the dataframe in the convenient version
df %>% 
  mutate_at(.vars = vars(E30A, E30B, E30C, E30corr), 
            .funs = list(orig = ~.)) %>% 
  mutate(E30corr = strsplit(E30corr, split = ", ", fixed = TRUE)) %>% 
  pivot_longer(E30A:E30C) %>% 
  mutate(value = as.integer(value | map2_lgl(name, E30corr, ~any(.x %in% .y)))) %>% 
  pivot_wider(names_from = name) %>% 
  select(-(4:8), everything())

##############################################################
##############################################################
##############################################################
# 4. Use pivot_wider to re-format the dataframe in the convenient version
df %>% 
  mutate_at(.vars = vars(E30A, E30B, E30C, E30corr), 
            .funs = list(orig = ~.)) %>% 
  mutate(E30corr = strsplit(E30corr, split = ", ", fixed = TRUE)) %>% 
  pivot_longer(E30A:E30C) %>% 
  mutate(value = case_when(is.na(E30corr_orig) ~ value, 
                           str_detect(E30corr_orig, name) ~ 1, 
                           TRUE ~ value)) %>%
  pivot_wider(names_from = name) %>% 
  select(-(4:8), everything())








#> # A tibble: 3 x 5
#>    E30A  E30B  E30C E30TEXT  E30corr  
#>   <dbl> <dbl> <dbl> <chr>    <list>   
#> 1     0     1     0 <NA>     <chr [1]>
#> 2     0     0     0 cleaning <chr [1]>
#> 3     0     0     0 janitor  <chr [2]>

# for (i in 1:nrow(df)) {
#   cols <- df$E30corr[[i]]
#   cols <- cols[!is.na(cols)]
#   df[i,cols] <- rep_along(cols,1L)
# }
# 
# df
#> # A tibble: 3 x 5
#>    E30A  E30B  E30C E30TEXT  E30corr  
#>   <dbl> <dbl> <dbl> <chr>    <list>   
#> 1     0     1     0 <NA>     <chr [1]>
#> 2     1     0     0 cleaning <chr [1]>
#> 3     0     1     1 janitor  <chr [2]>


df %>% 
  pivot_longer(1:3) %>% 
  mutate(value = as.integer(value | map2_lgl(name, E30corr, ~any(.x %in% .y)))) %>% 
  pivot_wider(names_from = name) %>% 
  select(-(1:2), everything())



tibble(strings = c('a', 'a, b', 'c')) %>% 
  mutate(split = strsplit(strings, split = ", ", TRUE))

