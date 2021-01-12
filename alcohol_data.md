Thesis\_Alcohol
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(ggplot2)
library(patchwork)
library(plotly) 
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(geepack)
```

``` r
## Import Alcohol Dataset: Filter to 2020 
apis_df=
  read_excel("./data/alcsales_August2020.xlsx",
             sheet = 3, 
             guess_max = 10000) %>% 
  janitor::clean_names() %>% 
  filter(year == "2020") %>% 
  mutate(beverage = 
           recode(beverage,
                  `1` = "spirits", 
                  `2` = "wine", 
                  `3` = "beer")) %>% 
  pivot_wider(
    names_from = beverage, 
    values_from = c(gallons, ethanol)
    ) %>% 
  view()
```

``` r
## 2020 Spirits  

spirits=
  apis_df %>%
  select(
    -gallons_beer, -gallons_wine, 
    -ethanol_beer, -ethanol_wine,
    -per_capita3yr) %>% 
  drop_na() %>% 
  arrange(fips) %>% 
  relocate(fips, month) %>% 
  select(-year) %>% 
  view()

## 2020 Wine  

wine=
  apis_df %>%
  select(
    -gallons_beer, -gallons_spirits, 
    -ethanol_beer, -ethanol_spirits,
    -per_capita3yr) %>% 
  drop_na() %>% 
  arrange(fips) %>%
  relocate(fips, month) %>% 
  select(-year) %>% 
  view()

## Beer  

beer=
  apis_df %>%
  select(
    -gallons_spirits, -gallons_wine, 
    -ethanol_spirits, -ethanol_wine,
    -per_capita3yr) %>% 
  drop_na() %>% 
  arrange(fips) %>% 
  relocate(fips, month) %>% 
  select(-year) %>% 
  view()
```

``` r
#### 2020 Spirit Plots

#Beverage 
bev_spirits=
  spirits %>% 
  ggplot(aes(x = month, y = gallons_spirits/1000000, color = fips)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Month", 
       y = "Gallons of Beverage") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) 

#Alcohol 
etoh_spirits=
  spirits %>% 
  ggplot(aes(x = month, y = ethanol_spirits/1000000, color = fips)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Month", 
       y = "Ethanol") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) 


#### 2020 Wine Plots

#Beverage 
bev_wine=
  wine %>% 
  ggplot(aes(x = month, y = gallons_wine/1000000, color = fips)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Month", 
       y = "Gallons of Beverage") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) 

#Alcohol 
etoh_wine=
  wine %>% 
  ggplot(aes(x = month, y = ethanol_wine/1000000, color = fips)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Month", 
       y = "Ethanol") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) 


#### 2020 Beer Plots

#Beverage 
bev_beer =
  beer %>% 
  ggplot(aes(x = month, y = gallons_beer/1000000, color = fips)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Month", 
       y = "Gallons of Beverage") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) 

#Alcohol 
etoh_beer=
  beer %>% 
  ggplot(aes(x = month, y = ethanol_beer/1000000, color = fips)) + 
  geom_point() +
  geom_line() + 
  labs(x = "Month", 
       y = "Ethanol") +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) 
```

``` r
#### On-Premises Establishments - Restaurants Since January 1, 2020

#Original
onprem_rest=
  read_excel("./data/state_alcohol_policies.xlsx", 
             sheet = 2,
             range = cell_cols("A:J"),
             col_names = TRUE,
             col_types = c("text", "numeric", "date", 
                           "text", "text", "text", "text", "text",
                           "text", "text")) %>% 
  janitor::clean_names() %>%
  rename(fips = fips_code)

#Recoded
onprem_rest1=
  read_excel("./data/state_alcohol_policies.xlsx", 
             sheet = 2,
             range = "N1:X191",
             col_names = TRUE,
             guess_max = 10000) %>% 
  janitor::clean_names() %>%
  rename(fips = fips_code) %>% 
  transform(
    month = as.numeric(month)
    ) %>% 
  relocate(fips, month) %>% 
  view()



#### On-Premises Establishments - Bars Since January 1, 2020

#Original
onprem_bar=
  read_excel("./data/state_alcohol_policies.xlsx", 
             sheet = 3,
             range = cell_cols("A:J"),
             col_names = TRUE,
             col_types = c("text", "numeric", "date", 
                           "text", "text", "text", "text", "text",
                           "text", "text")) %>% 
  janitor::clean_names() %>%
  rename(fips = fips_code)  

#Recoded
onprem_bar1=
  read_excel("./data/state_alcohol_policies.xlsx", 
             sheet = 3,
             range = "N1:X191",
             col_names = TRUE) %>% 
  janitor::clean_names() %>%
  rename(fips = fips_code) %>% 
  transform(
    month = as.numeric(month)
    ) %>% 
  relocate(fips, month) %>% 
  view()


#### Off-Premises Establishments - Since January 1, 2020

#Recoded
off_premise=
  read_excel("./data/state_alcohol_policies.xlsx", 
             sheet = 4,
             range = "H1:L191",
             col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  rename(fips = fips_code) %>% 
  transform(
    month = as.numeric(month)
    ) %>% 
  relocate(fips, month) %>% 
  view()
```

``` r
#### Merging Datasets

## Spirits 
spirits_rest=
  merge(spirits, onprem_rest1,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_spirits, ethanol_spirits) 

spirits_bar=
  merge(spirits, onprem_bar1,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_spirits, ethanol_spirits)

spirits_offprem=
  merge(spirits, off_premise,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_spirits, ethanol_spirits) 


## Wine 
wine_rest=
  merge(wine, onprem_rest1,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_wine, ethanol_wine) 

wine_bar=
  merge(wine, onprem_bar1,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_wine, ethanol_wine)

wine_offprem=
  merge(wine, off_premise,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_wine, ethanol_wine) 


## Beer 
beer_rest=
  merge(beer, onprem_rest1,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_beer, ethanol_beer) 

beer_bar=
  merge(beer, onprem_bar1,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_beer, ethanol_beer)

beer_offprem=
  merge(beer, off_premise,
      by = c("fips", "month")) %>% 
  relocate(state, fips, month, gallons_beer, ethanol_beer) 
```

``` r
## Saving tables 

write.table(spirits_rest, "./data/spirits_rest.csv", sep= ",")

write.table(wine_rest, "./data/wine_rest.csv", sep= ",")

write.table(beer_offprem, "./data/beer_offprem.csv", sep= ",")
```

Models to
run

``` r
geeglm(gallons_spirits ~ restaurant_open, data=spirits_rest, family = poisson, id=fips)
```

    ## 
    ## Call:
    ## geeglm(formula = gallons_spirits ~ restaurant_open, family = poisson, 
    ##     data = spirits_rest, id = fips)
    ## 
    ## Coefficients:
    ##     (Intercept) restaurant_open 
    ##     13.95266757      0.02442917 
    ## 
    ## Degrees of Freedom: 135 Total (i.e. Null);  133 Residual
    ## 
    ## Scale Link:                   identity
    ## Estimated Scale Parameters:  [1] 1060787
    ## 
    ## Correlation:  Structure = independence  
    ## Number of clusters:   18   Maximum cluster size: 8

geeglm(gallons\_spirits ~ restaurant\_open, data=spirits\_rest, family =
quasi(link = “identity”, variance = “constant”), id=fips)
