### import data
my_data <- read.delim("wmr_cases_deaths_modelling.txt")

## check data
head(my_data)

## burden
my_data %>% filter(year==2020 & deaths > 20000) 

library(tidyverse)
my_data <- my_data %>% mutate(burden = ifelse(country %in% c("Burkina Faso", "Democratic Republic of the Congo",
                                                             "Mali", "Mozambique", "Niger", "Nigeria", "Uganda",
                                                             "United Republic of Tanzania"), "high burden countries",
                                              "low burden countries" ))
table(my_data$burden)

my_data$burden <- as.factor(my_data$burden)
# Make low burden first
my_data$burden <- relevel(my_data$burden, "low burden countries")

p1 <- my_data %>% group_by(year,burden) %>% 
  summarise(n_deaths  = sum(deaths)) %>% 
  ggplot(aes(x=year, y=n_deaths/1000, fill=burden))+
  geom_area()+
  labs(y = "deaths (x1000)")
p1

### heatmap of mortality
p2 <- my_data %>% filter(burden=="high burden countries") %>% group_by(year, country) %>% 
  summarise(mortality = sum(deaths)/100000) %>% 
  ggplot(aes(x = year, y = country, fill = mortality)) +
  geom_tile() +
  coord_fixed()+
  labs(y = "mortality (deaths/100k)")
p2

## bar plot of population
p3 <- my_data %>% filter(burden=="high burden countries") %>% group_by(country) %>% 
  summarise(total_pop = mean(population)/1000000) %>% 
  ggplot(aes(x = country, y = total_pop)) +
  geom_col() +
  labs(y = "population (m)")+
  coord_flip()
p3

plots <- p1 +theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.x = element_blank() )+  guide_area()+ p2 + 
  p3+ theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank() )+ plot_layout(ncol = 2, guides = "collect")+
  plot_annotation(
    title = "Total malaria burden in Africa 2000-21",
    subtitle = "Despite falling mortality, transmission remains dominated by 8 high burden countries",
    caption = "Source: World malaria report 2022"
  )

plots
