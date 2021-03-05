#How to Analyze Data with R

library(dplyr)
library(gapminder)

head(gapminder)

#select certain columns
gapminder %>%
  select(country, year, pop)

#remove a column
gapminder %>%
  select(-continent)

#filter for a value, in this case 2007
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007)

#filter on multiple values
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007, country == "Poland")

#filter on multiple values, same column
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007, country %in% c("Poland", "Croatia"))

#filter and sort by life expectancy (default)
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007) %>%
  arrange(lifeExp)

#change sort to descending
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007) %>%
  arrange(desc(lifeExp))

#get only the top n values
gapminder %>%
  select(country, year, lifeExp) %>%
  filter(year == 2007) %>%
  arrange(desc(lifeExp)) %>%
  top_n(15)

#use mutate to create a new column
gapminder %>%
  select(country, year, pop, gdpPercap) %>%
  filter(year == 2007) %>%
  mutate(gdp = pop * gdpPercap) %>%
  arrange(desc(gdp)) %>%
  top_n(5)

#transmutate just provides the calculated column
gapminder %>%
  select(country, year, pop, gdpPercap) %>%
  filter(year == 2007) %>%
  transmute(gdp = pop * gdpPercap) %>%
  arrange(desc(gdp)) %>%
  top_n(5)

#summarize a column
gapminder %>%
  summarize(avgLifeExp = mean(lifeExp))

#summarize and filter
gapminder %>%
  filter(year == 2007, continent == "Europe") %>%
  summarize(avgLifeExp = mean(lifeExp))

#summarize, filter and group
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp))

gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp)) %>%
  arrange(desc(avgLifeExp))

gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp)) %>%
  mutate(over75 = if_else(avgLifeExp > 75, "Y", "N"))

#Data Manipulation and Exploration

library(ggplot2)

wine = wine %>% select(-c(description))

wine %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count))

selected_countries = wine %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count)) %>% top_n(10) %>% select(country)
selected_countries = as.character(selected_countries$country)

# creating a country and points data frame containing only the 10 selected countries' data
select_points=wine %>% filter(country %in% selected_countries) %>% select(country, points) %>% arrange(country)

ggplot(wine, aes(points,price)) + geom_point() + geom_smooth()

ggplot(select_points, aes(x=reorder(country,points,median),y=points)) + geom_boxplot(aes(fill=country)) + xlab("Country") + 
  ylab("Points") + ggtitle("Distribution of Top 10 Wine Producing Countries") + theme(plot.title = element_text(hjust = 0.5))

wine %>% filter(!(country %in% selected_countries)) %>% group_by(country) %>%  summarize(median=median(points))  %>% arrange(desc(median)) 

top=wine %>% group_by(country) %>% summarize(median=median(points)) %>% arrange(desc(median))
top=as.character(top$country)
both=intersect(top,selected_countries)
both

topwine = wine %>% group_by(variety) %>% summarize(number=n()) %>% arrange(desc(number)) %>% top_n(10)
topwine=as.character(topwine$variety)
topwine

wine %>% filter(variety %in% topwine) %>% group_by(variety)%>% summarize(median=median(points)) %>% ggplot(aes(reorder(variety,median),median)) + geom_col(aes(fill=variety)) + xlab('Variety') + ylab('Median Point') + scale_x_discrete(labels=abbreviate)

top15percent=wine %>% arrange(desc(points)) %>% filter(points > quantile(points, prob = 0.85))
cheapest15percent=wine %>% arrange(price) %>% head(nrow(top15percent))
goodvalue = intersect(top15percent,cheapest15percent)
goodvalue

