### import data
### import data
df <- read.delim("wmr_commodity_distribution.txt")
head(df)

## Are there places where nets are distributed but not used?
d1 <- df %>% filter(region=="Africa") %>%  group_by(country) %>% 
  summarise(ITNs = sum(No_of_LLINs_delivered)) %>% 
  dplyr::filter(ITNs>15000000)# %>% 
  
head(d1)

d1$country <- as.factor(d1$country)
levels(d1$country) <- c("Angola","Cote dIvoire", "DRCongo","Ethiopia","Ghana",
                        "Kenya","Madagascar","Mainland","Mali","Mozambique","Niger",
                        "Nigeria", "Uganda","Tanzania","Zambia"  )
g1 <- d1 %>% ggplot(aes(x = country, y = ITNs/1000000))+
  geom_col()+
  labs(y = "No of nets (M)")+
  coord_flip()
g1

## access to ITNs
g2 <- df %>% filter(region == "Africa") %>% 
  group_by(country) %>% 
  summarise(average_percent = mean(Modelled_percentage_of_population_with_access_to_an_ITN)) %>% 
  filter(average_percent<50) %>% 
  ggplot(aes(country, average_percent))+
  geom_col()+
  coord_flip()+
  labs(y = "% of regions with access to ITNs")
g2

g3 <- df %>% ggplot(aes(No_of_LLINs_delivered, Modelled_percentage_of_population_with_access_to_an_ITN))+
  geom_point()+
  labs(x = "No of LLINs delivered",
       y = " % of population with access to ITN")
g3

### import data
my_data <- read.delim("wmr_cases_deaths_modelling.txt")

## bar plot of population
g4 <- my_data %>% filter(region =="Africa") %>% group_by(country) %>% 
  summarise(total_pop = mean(population)/1000000) %>% 
  top_n(15) %>% 
  ggplot(aes(x = country, y = total_pop)) +
  geom_col() +
  labs(y = "population (m)")+
  coord_flip()
g4


graphs <- g1 +  g4 + g2 + 
  g3+ plot_layout(ncol = 2, guides = "collect")+
  plot_annotation(
    title = "Distribution of nets in Africa",
    subtitle = "Despite many nets being distributed, there are places that they still are not being used",
    caption = "Source: World malaria report 2022"
  )

graphs
