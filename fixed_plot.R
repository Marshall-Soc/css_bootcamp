library(dplyr)
library(ggplot2)

mtcars |>
  filter(cyl == 4) |>
  ggplot(aes(x = wt, y = mpg)) +
  geom_point()
