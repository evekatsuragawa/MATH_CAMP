# GVPT MATH CAMP AUG 19-23 by HARRIET GOERS

# AUGUST 19, DATA VISUALISATION
library(tidyverse)
mpg
library(ggplot2)
library(ggthemes)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point()
ggplot(data = mpg, mapping = aes(x = class, y= drv)) + geom_point()
ggplot(data = mpg, mapping = aes(x = hwy, y = cyl)) + geom_col()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = class)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_color_colorblind()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth(method = "lm") +
  scale_color_colorblind()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  labs(title = "Engine Displacement and Highway Miles per Gallon",
       subtitle = "values for seven different types of cars",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon",
       colour = "Type of Cars") +
  geom_smooth(method = "lm") +
  scale_color_colorblind()

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy,
                                              colour = class == "2seater"))
# "==" sign is important

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy),
                                              colour = "red")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy),
                                colour = "blue")
# note the colour argument is not in the aes

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour = cyl)) + 
  geom_point() + 
  labs(title = "Engine Displacement and Highway Miles per Gallon",
       subtitle = "values for seven different types of cars",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, size = cyl)) + 
  geom_point() + 
  labs(title = "Engine Displacement and Highway Miles per Gallon",
       subtitle = "values for seven different types of cars",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, shape = class)) + 
  geom_point() + 
  labs(title = "Engine Displacement and Highway Miles per Gallon",
       subtitle = "values for seven different types of cars",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon")

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  labs(title = "Engine Displacement and Highway Miles per Gallon",
       subtitle = "values for seven different types of cars",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon")

# LESS IS MORE
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  labs(title = "Engine Displacement and Highway Miles per Gallon",
       subtitle = "values for seven different types of cars",
       x = "Engine Displacement (L)",
       y = "Highway Miles per Gallon",
       colour = "Type of Cars") +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_color_colorblind()

select(mpg, manufacturer, model, drv)
ggplot(mpg, aes(x = drv)) + geom_bar()
ggplot(mpg, aes(x = fct_infreq(drv))) + geom_bar()
ggplot(mpg, aes(x = hwy, colour = drv)) + geom_density()
ggplot(mpg, aes(x = hwy, colour = drv, fill = drv)) + geom_density(alpha = 0.2)

# AUGUST 20, DATA TRANSFORMATION
# create objects
x <- 3*4 # use Alt plus minus for "<-" , but please don't use "=" if possible
x <- 15*5
s_10 <- seq(1, 10, 2)

install.packages("gapminder")
library(tidyverse)
library(gapminder)
head(gapminder) # can put a number as a second argument
# "characters" is just words; "factors" can be categorical variables

# filter function: make the data frame smaller
filter(gapminder, country == "Japan", year > 2000)
filter(gapminder, continent %in% c("Asia","Europe")) # all Asian and European countries
filter(gapminder, pop>500000 & pop<1000000) # "and" is more restrictive
filter(gapminder, pop>500000 | pop<1000000) # "or" operator

filter(gapminder, pop>1000000000) # countries with a pop larger than 1 billion (1e9)
filter(gapminder, continent %in% c("Oceania")) # all Oceania countries
filter(gapminder,lifeExp>50 & lifeExp<60) # life expectancy between 50-60
filter(gapminder,lifeExp<50 | lifeExp>60) # life expectancy below 50 or over 60

# arrange function: in a way that may be more useful
arrange(gapminder, lifeExp)
arrange(gapminder, desc(pop))

slice_min(gapminder, lifeExp)
slice_max(gapminder, lifeExp)
slice_min(gapminder, pop)

# select function: pick some columns out of the data frame
select(gapminder, country, year, lifeExp)
select(gapminder, country:lifeExp) # ":" as in "seq"
select(gapminder, -(country:lifeExp)) # "-" for not

vars <- c("country","year","lifeExp")
select(gapminder, any_of(vars)) # same as printing out each variable
select(gapminder, starts_with("c"))

# mutate function
mutate(gapminder, gdp=gdpPercap*pop)
transmute(gapminder, country, year, gdp=gdpPercap*pop) # "transmute" for select and mutate

# summarise function
summarise(gapminder, avgpop = mean(pop), avggdppc = mean(gdpPercap))
continent <- group_by(gapminder, continent)
summarise(continent, avgpop = mean(pop), avggdppc = mean(gdpPercap))

mutate(gapminder, gdp=gdpPercap*pop) # each country-year's GDP

country <- group_by(gapminder, country)
country_avg <- summarise(mutate(country, gdp = pop * gdpPercap), avg_gdp = mean(gdp))
head(country_avg) # avgGDP for all countries

gapminder |> 
  group_by(continent) |> 
  summarise(avg_pop = mean(pop), avg_gdppc = mean(gdpPercap)) |> 
  ggplot(aes(x = continent, y = avg_gdppc)) + 
  geom_col() + 
  theme_minimal()

Americas_2000 <- gapminder |>
  filter(continent == "Americas", year > 2000) |>
  group_by(country) |>
  summarise(avg_gdppc=mean(gdpPercap))
ggplot(Americas_2000, aes(x=avg_gdppc, y=reorder(country, avg_gdppc)))+
  geom_col()+theme_minimal() + 
  labs(title = "Average GDP per capita for Countries in the Americas after year 2000",
       x = "Average GDP per capita (US$)",
       y = NULL) + 
  scale_x_continuous(labels = scales::label_dollar())

# AUGUST 21, DATA WRANGLING
install.packages("usethis")
library(usethis)
use_git_config(user.name = "Katsuragawa", user.email = "zzhu1228@umd.edu")
usethis::create_github_token()
gitcreds::gitcreds_set()

create_from_github(
  "https://github.com/evekatsuragawa/MATH_CAMP.git",
  destdir = "C:\\Users\\katsuragawa\\Desktop\\Maryland 2024-\\Math camp Aug 19-23\\mathcamp"
) # WINDOWS has to be a double backslash \\
