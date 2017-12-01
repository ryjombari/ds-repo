# libraries ----
library(tidyverse)

# data ----
url <- 'https://raw.githubusercontent.com/OHI-Science/data-science-training/master/data/gapminder.csv'
gapminder <- read_csv(url) # View(gapminder)

# ggplot: after filter by country ----

# get list of  country names
uc <- gapminder %>% 
  distinct(country)

country_plot <- function(cntry){
  
  # browser allows you to see workspace variables, like keyboard in matlab
  # Q to quit browser mode
  # browser()
  png <- paste0("gdp_", cntry, ".png")
  cat("country_plot(", cntry, ") -> ", png, "\n")
  
  g <- gapminder %>%
    filter(country == cntry) %>%
    ggplot(aes(x = year, y = gdpPercap)) +
    geom_point() +
    geom_smooth() +
    labs(title = cntry)
  
  # ggsave: after filter by country & plot ----
  ggsave(filename = png, g)
}

# country_plot("Mexico")

countries <- c("United States","Peru","New Zealand")
for (k in countries){
  # for (k in 1:length(countries)){}
  country_plot(k)
  # country_plot(countries[k])
}