# Background: three tidy datasets for the timepoints 0, 1 and 2, where the datasets use 
# different variable names
# Additionally: one dataframe containing matching variable names to programmatically adapt the 
# names to combine the datasets
# very elegant solution as shown here provided by R4DS - scottandme!

library(tidyverse)
set.seed(1234)

# sample dfs
data_0 <- tibble::tibble("X1" = sample(c(1, 2, 3, 4, NA_real_), 8, T),
                         "X2" = sample(c(100, 200, 300, 400, NA_real_), 8, T),
                         "X3" = sample(c("m", "f"), 8, T), 
                         "tp" = "0") # timepoint
data_1 <- tibble::tibble("X1" = sample(c(100, 200, 300, 400, NA_real_), 8, T),
                         "X2" = sample(c("m", "f"), 8, T),
                         "X3" = sample(c(1, 2, 3, 4, NA_real_), 8, T), 
                         "tp" = "1")
data_2 <- tibble::tibble("X3" = sample(c(1, 2, 3, 4, NA_real_), 8, T),
                         "X4" = sample(c("m", "f"), 8, T), 
                         "tp" = "2")

# create df with matching variable names
var_names <- tibble::tibble(real_name = c("var1", "var2_salary", "var3_gender"),
                            name_0 = c("X1", "X2", "X3"),
                            name_1 = c("X3", "X1", "X2"),
                            name_2 = c("X3", NA_character_, "X4"))


var_names %>%
  rename(to = 1) %>%
  pivot_longer(-to, names_to = "source", values_to = "from") %>%
  select(source, from, to) -> var_names


data_list <- list(data_0 = data_0, data_1 = data_1, data_2 = data_2)

rename_cols <- function(.data, from, to) {
  dplyr::rename(.data, !!!rlang::syms(discard(set_names(from, to), is.na(from))))
}

table_cols <- function(.data, cols, ...) {
  purrr::map(cols, ~table(.data[[.x]]), ...)
}

remapped <-
  enframe(data_list, name = "source", value = "data") %>%
  mutate(mapping = map(source, ~filter(var_names, source == .))) %>%
  hoist(mapping, from = "from", to = "to") %>%
  mutate(output       = pmap(list(data, from, to), rename_cols),
         data_table   = map2(data, from, table_cols, useNA = "always"), # error occurs here
         output_table = map2(output, to, table_cols, useNA = "always"), # this line works fine
         validate     = map2_lgl(data_table, output_table, identical))













# prepare the renaming -vectors
name_0 <- pull(var_names, name_0)
name_1 <- pull(var_names, name_1)
name_2 <- pull(var_names, name_2)
real_names <- pull(var_names, real_name)

# merge the dataframes and create a combined df
map2_dfr(list(data_0, data_1, data_2), list(name_0, name_1, name_2), function(tdf, names) {
  real_names <- real_names[!is.na(names)]
  names <- keep(names, function(v) !is.na(v))
  tdf %>%
    rename(!!!rlang::syms(set_names(names, real_names)))
}) -> comb_data

######################################################
# NEW QUESTION HERE: How to run automatic checks with a selection of variables?
######################################################

# I can run manual checks (to see if the merging worked fine) for each variable in the dataframe
# manually with this code, but I was wondering how to do this
#  programmatically?
# (The final idea is to randomly sample 5% of the original dataframe and run the checks, which equals ~50 variables)

# # check T0
# table(data_0$X1, useNA = "a")
# table(comb_data$var1[comb_data$tp == "0"], useNA = "a")
# 
# 
# # check T1
# table(data_1$X3, useNA = "a")
# table(comb_data$var1[comb_data$tp == "1"], useNA = "a")
# 
# # check T2
# table(data_2$X3, useNA = "a")
# table(comb_data$var1[comb_data$tp == "2"], useNA = "a")
# 
# # I though about taking a sample from var_names
# # I take 100% here, cause its a small df
# var_names %>% 
#   sample_frac(1)

# and then writing a function to run trhough each line - but I failed miserably for
# now and my head is all wrapped up - do you know of any elegant solutions?



# Solution
library(tidyverse)
set.seed(1234)

# sample dfs
data_0 <- tibble::tibble("X1" = sample(c(1, 2, 3, 4, NA_real_), 8, T),
                         "X2" = sample(c(100, 200, 300, 400, NA_real_), 8, T),
                         "X3" = sample(c("m", "f"), 8, T), 
                         "tp" = "0") # timepoint
data_1 <- tibble::tibble("X1" = sample(c(100, 200, 300, 400, NA_real_), 8, T),
                         "X2" = sample(c("m", "f"), 8, T),
                         "X3" = sample(c(1, 2, 3, 4, NA_real_), 8, T), 
                         "tp" = "1")
data_2 <- tibble::tibble("X3" = sample(c(1, 2, 3, 4, NA_real_), 8, T),
                         "X4" = sample(c("m", "f"), 8, T), 
                         "tp" = "2")

# # create df with matching variable names
# var_names <- tibble::tibble(real_name = c("var1", "var2_salary", "var3_gender"),
#                             name_0 = c("X1", "X2", "X3"),
#                             name_1 = c("X3", "X1", "X2"),
#                             name_2 = c("X3", NA_character_, "X4"))

# Solution


# create df with matching variable names
var_names2 <-
  tibble::tibble(to = c("var1", "var2_salary", "var3_gender"),
                 data_0    = c("X1", "X2", "X3"),
                 data_1    = c("X3", "X1", "X2"),
                 data_2    = c("X3", NA_character_, "X4")) %>%
  pivot_longer(-to, names_to = "source", values_to = "from") %>% 
  select(source, from, to) %>% 
  drop_na()

# var_names %>%
#   rename(to = 1) %>%
#   pivot_longer(-to, names_to = "source", values_to = "from") %>%
#   select(source, from, to) %>% drop_na() -> var_names


data_list <- list(data_0 = data_0, data_1 = data_1, data_2 = data_2)

rename_cols <- function(.data, from, to) {
  dplyr::rename(.data, !!!rlang::syms(discard(set_names(from, to), is.na(from))))
}

table_cols <- function(.data, cols, ...) {
  purrr::map(cols, ~table(.data[[.x]]), ...)
}

remapped <-
  enframe(data_list, name = "source", value = "data") %>%
  mutate(mapping = map(source, ~filter(var_names, source == .))) %>%
  hoist(mapping, from = "from", to = "to") %>%
  mutate(output       = pmap(list(data, from, to), rename_cols),
         data_table   = map2(data, from, table_cols, useNA = "always"), #this line fails
         output_table = map2(output, to, table_cols, useNA = "always"), # this is ok
         validate     = map2_lgl(data_table, output_table, identical))

# 
# # remapped %>% select(output) %>% unnest(cols = c(output))
# sessioninfo::session_info()
# 
# devtools::install_version('rlang', version = '0.4.4', repos = "https://cran.r-project.org")


aa <- remapped %>% select(output) %>% unnest(cols = c(output))

