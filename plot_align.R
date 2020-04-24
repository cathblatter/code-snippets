install.packages("ggridges")
install.packages("patchwork")
library(tidyverse)
library(ggridges)
library(patchwork)
p1 <- ggplot(iris, aes(Sepal.Length, Species)) + 
  geom_density_ridges() 
p2 <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp))
p1 + p2  + plot_layout(ncol=1) 

library(cowplot)

p1 <- ggplot(iris, aes(Sepal.Length, Species)) + 
  geom_density_ridges() 
p2 <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp))
p3 <- ggplot(PlantGrowth) + 
  geom_boxplot(aes(group, weight))
p4 <- ggplot(USArrests) + 
  geom_point(aes(Assault, UrbanPop))

grobs <- lapply(list(p1, p2, p3,p4), as_grob)
plot_widths <- lapply(grobs, function(x) {x$widths})
aligned_widths <- align_margin(align_margin(plot_widths, "first"),"last")

# reset widths
for (i in seq_along(grobs)) {
  grobs[[i]]$widths <- aligned_widths[[i]]
}


plot_grid(plotlist = grobs, ncol = 1)

grid.draw(grobs[[1]])

cowplot::ggdraw(grobs[[2]])
cowplot::ggdraw(grobs[[3]])





library(tidyverse)
test_data <- tibble::tribble(~c1, ~c2, ~c3, ~c4, 
                             1, 2, 2, 4, 
                             3, 4, 3, 4, 
                             NA_integer_, 4, 4, 3,
                             NA_integer_, NA_integer_, NA_integer_, 3)

test_data %>% 
  mutate(scale1_sum = rowSums(select(., c1:c4), na.rm = T),
         scale1_mean = rowMeans(select(., c1:c4), na.rm = T),
         scale1_missing = is.na(c1)+is.na(c2)+is.na(c3)+is.na(c4),
         scale1_test = pmap_dbl(list(c1, c2, c3, c4), ~sum(is.na(.))))