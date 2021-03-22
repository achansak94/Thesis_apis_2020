Thesis\_Alcohol\_Final\_RMD
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
library(lme4)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(sjPlot)
```

    ## #refugeeswelcome

``` r
library(ggplot2)
library(haven)
library(tableone)
```

``` r
##### Import, Clean and Recode Dataset
apis_df=
  read_excel("./data/alcsales_August2020.xlsx",
             sheet = 5, 
             guess_max = 10000) %>% 
  janitor::clean_names() %>% 
  mutate(beverage = 
           recode(beverage,
                  `1` = "spirits", 
                  `2` = "wine", 
                  `3` = "beer")) %>% 
  group_by(year, month, fips) %>% 
  mutate(
    ethanol_total = sum(ethanol),
    gallons_total = sum(gallons)) %>% 
  pivot_wider(
    names_from = beverage, 
    values_from = c(gallons, ethanol)
    ) %>% 
  mutate(
    month_cont = case_when(
      (year == '2017' & month == '9') ~ '1', ##September 2017
      (year == '2017' & month == '10') ~ '2',
      (year == '2017' & month == '11') ~ '3',
      (year == '2017' & month == '12') ~ '4',
   
      (year == '2018' & month == '1') ~ '5',
      (year == '2018' & month == '2') ~ '6',
      (year == '2018' & month == '3') ~ '7',
      (year == '2018' & month == '4') ~ '8',
      (year == '2018' & month == '5') ~ '9',
      (year == '2018' & month == '6') ~ '10',
      (year == '2018' & month == '7') ~ '11',
      (year == '2018' & month == '8') ~ '12',
      (year == '2018' & month == '9') ~ '13',
      (year == '2018' & month == '10') ~ '14',
      (year == '2018' & month == '11') ~ '15',
      (year == '2018' & month == '12') ~ '16',
      
      (year == '2019' & month == '1') ~ '17',
      (year == '2019' & month == '2') ~ '18',
      (year == '2019' & month == '3') ~ '19',
      (year == '2019' & month == '4') ~ '20',
      (year == '2019' & month == '5') ~ '21',
      (year == '2019' & month == '6') ~ '22',
      (year == '2019' & month == '7') ~ '23',
      (year == '2019' & month == '8') ~ '24',
      (year == '2019' & month == '9') ~ '25',
      (year == '2019' & month == '10') ~ '26',
      (year == '2019' & month == '11') ~ '27',
      (year == '2019' & month == '12') ~ '28',
      
      (year == '2020' & month == '1') ~ '29',
      (year == '2020' & month == '2') ~ '30',
      (year == '2020' & month == '3') ~ '31',
      (year == '2020' & month == '4') ~ '32',
      (year == '2020' & month == '5') ~ '33',
      (year == '2020' & month == '6') ~ '34',
      (year == '2020' & month == '7') ~ '35',
      (year == '2020' & month == '8') ~ '36', ##Aug 2020
    )) %>% 
  drop_na(month_cont) %>% 
  transform(
    month_cont = as.numeric(month_cont)) %>%  
  mutate(
    annual_time = 
      case_when(month_cont %in% 1:12 ~'year_1',
                month_cont %in% 13:24 ~'year_2',
                month_cont %in% 25:36 ~'year_3')) %>% 
   view()
```

``` r
#### 2017-2020 Total Gallons of Alcohol & Per Capita 

total_alcohol=
  apis_df %>%
  mutate(
    month_cat = case_when(
      (year == '2017' & month == '9') ~ '1', ##September 2017
      (year == '2017' & month == '10') ~ '2',
      (year == '2017' & month == '11') ~ '3',
      (year == '2017' & month == '12') ~ '4',
      (year == '2018' & month == '1') ~ '5',
      (year == '2018' & month == '2') ~ '6',
      (year == '2018' & month == '3') ~ '7',
      (year == '2018' & month == '4') ~ '8',
      (year == '2018' & month == '5') ~ '9',
      (year == '2018' & month == '6') ~ '10',
      (year == '2018' & month == '7') ~ '11',
      (year == '2018' & month == '8') ~ '12',
      
      (year == '2018' & month == '9') ~ '1',
      (year == '2018' & month == '10') ~ '2',
      (year == '2018' & month == '11') ~ '3',
      (year == '2018' & month == '12') ~ '4',
      (year == '2019' & month == '1') ~ '5',
      (year == '2019' & month == '2') ~ '6',
      (year == '2019' & month == '3') ~ '7',
      (year == '2019' & month == '4') ~ '8',
      (year == '2019' & month == '5') ~ '9',
      (year == '2019' & month == '6') ~ '10',
      (year == '2019' & month == '7') ~ '11',
      (year == '2019' & month == '8') ~ '12',
      
      (year == '2019' & month == '9') ~ '1',
      (year == '2019' & month == '10') ~ '2',
      (year == '2019' & month == '11') ~ '3',
      (year == '2019' & month == '12') ~ '4',
      (year == '2020' & month == '1') ~ '5',
      (year == '2020' & month == '2') ~ '6',
      (year == '2020' & month == '3') ~ '7',
      (year == '2020' & month == '4') ~ '8',
      (year == '2020' & month == '5') ~ '9',
      (year == '2020' & month == '6') ~ '10',
      (year == '2020' & month == '7') ~ '11',
      (year == '2020' & month == '8') ~ '12', ##Aug 2020
    )) %>% 
  select(-gallons_spirits:-ethanol_beer, -per_capita3yr, -pct_change, -per_capita) %>% 
  group_by(year, month_cont, fips) %>% 
  mutate(
    per_capita_ethanol_total = 
      round(ethanol_total/population, 8), ## Total Alcohol/Population
    treat_yr=
      case_when(
        (annual_time == 'year_1' | annual_time == 'year_2') ~ '0',
        (annual_time == 'year_3') ~ '1'),
    month_cat = as.numeric(month_cat)
    ) %>% 
  distinct () %>% 
  view()
```

``` r
####Policies for on-premise and off-premise establishments since January 1, 2020

policy_data=
  read_excel("./data/state_alcohol_policies.xlsx", 
             sheet = 5,
             range = "A1:L191",
             col_names = TRUE,
             guess_max = 10000) %>% 
  janitor::clean_names() %>% 
  rename(fips = fips_code, month=month_3, month_cont=month_4) %>% 
  transform(
    month_cont = as.numeric(month_cont),
    month_month = as.numeric(month)
    ) %>% 
  filter(month_cont != "37" & month_cont != "38") %>% 
  rename(
     rest_restrict = restaurant_capacity_restrictions,
     bar_restrict = bar_capacity_restrictions,
     tout_curb= takeout_or_curbside,
     delohome = deliver_to_homes,
     op_open_restrict = off_premise_open_restrictions,
     op_deliver_restrict = off_premise_deliver_to_homes_restrictions
     ) %>% 
  relocate(fips, month_cont) %>% 
  select(-month_month) %>% 
  view()
```

    ## New names:
    ## * Month -> Month...3
    ## * Month -> Month...4

``` r
## Merged with Alcohol sales with alcohol-related policies 
alco_policydf=
  merge(
    x = total_alcohol,
    y = policy_data,
    by = c("fips", "month_cont"), 
    all.x =TRUE) %>% 
  filter(
    fips %in% c("02", "05", "08", "21", "25", "29", "38", "47", "48")) %>% 
  relocate(state, fips, month_cont, ethanol_total, gallons_total) %>%
  transform(
    month_cont = as.numeric(month_cont)
    )  %>% 
  distinct() %>% 
  mutate(
    restaurant_open = replace_na(restaurant_open, 1),
    bar_open = replace_na(bar_open, 1),
    rest_restrict = replace_na(rest_restrict, 0),
    bar_restrict = replace_na(bar_restrict, 0),
    tout_curb= ifelse(fips %in% c('02','08','21', '38','47','48') &   
                        is.na(tout_curb), 0, tout_curb),
    tout_curb = replace_na(tout_curb, 1),
    delohome= ifelse(fips %in% c('02','08','21', '38') &   
                        is.na(delohome), 0, delohome),
    delohome = replace_na(delohome, 1),
    op_open_restrict = replace_na(op_open_restrict, 1),
    op_deliver_restrict = ifelse(fips %in% c('05','38') & is.na(op_deliver_restrict), 0, 
                                 op_deliver_restrict),
    op_deliver_restrict = replace_na(op_deliver_restrict, 1),
    ) %>% 
  rename(actual_month = month.x) %>% 
  select (-month.y) %>% 
  arrange(month_cont) %>% 
  view()
  
## Total Observations for 9 states: 324 - Confirmed by 9 states x 36 months 
```

#### Histograms - To check distributions of the 9 states

alco\_policydf1 %\>% ggplot(aes(x=per\_capita\_ethanol\_total)) +
geom\_histogram() + labs(x = “Gallons of Alcohol (Mil)”, y = “Count”)

alco\_policydf1 %\>% filter(fips == “02”) %\>%
hist(alco\_policydf1$per\_capita\_ethanol\_total)

qplot(alco\_policydf1$per\_capita\_ethanol\_total, geom=“histogram”)
hist(per\_capita\_ethanol\_total, main=“Histogram of observed data”)

``` r
## Further cleaning of dataset to analyze  

alco_policydf1= 
  alco_policydf %>% 
  mutate(
    state = case_when(
      (fips == '02' ~ 'AK'),
      (fips == '05' ~ 'AR'),
      (fips == '08' ~ 'CO'),
      (fips == '21' ~ 'KY'),
      (fips == '25' ~ 'MA'),
      (fips == '29' ~ 'MO'),
      (fips == '38' ~ 'ND'),
      (fips == '47' ~ 'TN'),
      (fips == '48' ~ 'TX'))
    ) %>% 
  mutate(
    prepost_rest_open = restaurant_open,
    prepost_rest_open = 
      case_when(
        (fips == '02' & month_cont == 32) ~ '1',
        (fips == '02' & month_cont >= 33) ~ '2',
        (fips == '05' & month_cont == 32) ~ '1',
        (fips == '05' & month_cont >= 33) ~ '2',
        (fips == '08' & month_cont == 32|fips == '08' & month_cont == 33) ~ '1',
        (fips == '08' & month_cont >= 34) ~ '2',
        (fips == '21' & month_cont == 31|
           fips == '21' & month_cont == 32|
           fips == '21' & month_cont == 33) ~ '1',
        (fips == '21' & month_cont >= 34) ~ '2',
        (fips == '25' & month_cont == 32|fips == '25' & month_cont ==33) ~ '1',
        (fips == '25' & month_cont >= 34) ~ '2',
        (fips == '29' & month_cont == 32) ~ '1',
        (fips == '29' & month_cont >= 33) ~ '2',
        (fips == '38' & month_cont == 32) ~ '1',
        (fips == '38' & month_cont >= 33) ~ '2',
        (fips == '47' & month_cont == 32) ~ '1',
        (fips == '47' & month_cont >= 33) ~ '2',
        (fips == '48' & month_cont == 32) ~ '1',
        (fips == '48' & month_cont >= 33) ~ '2'),
        prepost_rest_open = replace_na(prepost_rest_open, 0))%>% 
    relocate(fips, month_cont, prepost_rest_open) %>% 
    arrange(fips, month_cont, prepost_rest_open) %>% 
  view ()
```

``` r
#### Generalized Linear Models -- Bivariate Crude Associations (Only policies with change)

### Alaska (AK) Per-Capita Models
model1ak = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "02"), family="gaussian")

model2ak = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "02"), family="gaussian")

model3ak= glm(per_capita_ethanol_total ~ as.factor(tout_curb), data = subset(alco_policydf1, fips == "02"), family="gaussian")

model4ak= glm(per_capita_ethanol_total ~ as.factor(delohome), data = subset(alco_policydf1, fips == "02"), family="gaussian")

tab_model(model1ak, model2ak, model3ak, model4ak, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2374

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2240 – 0.2509

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2362

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2227 – 0.2496

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.2331

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.2195 – 0.2467

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.2331

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.2195 – 0.2467

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0251

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.1056 – 0.0554

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.545

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0206

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0600 – 0.1012

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.619

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

tout\_curb
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.0331

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

\-0.0078 – 0.0739

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

0.122

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

delohome
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.0331

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

\-0.0078 – 0.0739

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

0.122

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.011

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.007

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.069

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.069

</td>

</tr>

</table>

``` r
### Arkansas (AR) Per-Capita Model 
model1ar = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "05"), family="gaussian")

model2ar = glm(per_capita_ethanol_total ~ relevel(as.factor(bar_open), ref="1"), data = subset(alco_policydf1, fips == "05"), family="gaussian")

model3ar = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "05"), family="gaussian")

model4ar = glm(per_capita_ethanol_total ~ as.factor(bar_restrict), data = subset(alco_policydf1, fips == "05"), family="gaussian")

tab_model(model1ar, model2ar, model3ar, model4ar, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1519

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1465 – 0.1572

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1513

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1459 – 0.1567

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.1497

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.1448 – 0.1545

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.1497

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.1448 – 0.1545

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0047

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0276 – 0.0370

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.779

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(bar\_open, ref
=<br>“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0127

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0101 – 0.0355

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.282

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.0279

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.0111 – 0.0447

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>0.003</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

bar\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.0279

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.0111 – 0.0447

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>0.003</strong>

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.002

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.034

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.237

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.237

</td>

</tr>

</table>

``` r
### Colorado (CO) Per-Capita Model 

model1co = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "08"), family="gaussian")

model2co = glm(per_capita_ethanol_total ~ relevel(as.factor(bar_open), ref="1"), data = subset(alco_policydf1, fips == "08"), family="gaussian")

model3co = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "08"), family="gaussian")

model4co = glm(per_capita_ethanol_total ~ as.factor(bar_restrict), data = subset(alco_policydf1, fips == "08"), family="gaussian")

model5co= glm(per_capita_ethanol_total ~ as.factor(tout_curb), data = subset(alco_policydf1, fips == "08"), family="gaussian")

model6co= glm(per_capita_ethanol_total ~ as.factor(delohome), data = subset(alco_policydf1, fips == "08"), family="gaussian")

tab_model(model1co, model2co, model3co, model4co, model5co, model6co, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  4">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  5">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  6">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  7">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  8">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  9">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2394

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2291 – 0.2498

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2379

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2276 – 0.2483

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.2358

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.2262 – 0.2454

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.2374

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.2274 – 0.2473

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.2356

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.2253 – 0.2458

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

0.2356

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

0.2253 – 0.2458

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0003

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0436 – 0.0442

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.989

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(bar\_open, ref
=<br>“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0180

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0179 – 0.0539

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.332

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.0434

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.0101 – 0.0768

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>0.015</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

bar\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.0372

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

\-0.0049 – 0.0794

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

0.092

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

tout\_curb
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.0279

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.0003 – 0.0554

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

0.056

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

delohome
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

0.0279

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

0.0003 – 0.0554

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

0.056

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.000

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.028

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.161

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.081

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.104

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.104

</td>

</tr>

</table>

``` r
### Kentucky (KY) Per-Capita Model 
model1ky = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "21"))

model2ky = glm(per_capita_ethanol_total ~ relevel(as.factor(bar_open), ref="1"), data = subset(alco_policydf1, fips == "21"))

model3ky = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "21"))

model4ky = glm(per_capita_ethanol_total ~ as.factor(bar_restrict), data = subset(alco_policydf1, fips == "21"))

model5ky = glm(per_capita_ethanol_total ~ as.factor(tout_curb), data = subset(alco_policydf1, fips == "21"))

model6ky = glm(per_capita_ethanol_total ~ as.factor(delohome), data = subset(alco_policydf1, fips == "21"))

tab_model(model1ky, model2ky, model3ky, model4ky, model5ky, model6ky, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  4">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  5">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  6">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  7">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  8">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  9">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1683

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1591 – 0.1774

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1680

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1587 – 0.1773

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.1668

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.1578 – 0.1757

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.1671

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.1583 – 0.1758

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.1658

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.1567 – 0.1750

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

0.1658

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

0.1567 – 0.1750

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0032

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0285 – 0.0349

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.846

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(bar\_open, ref
=<br>“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0047

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0231 – 0.0326

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.741

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.0211

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

\-0.0098 – 0.0520

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

0.190

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

bar\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.0264

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

\-0.0108 – 0.0636

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

0.174

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

tout\_curb
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.0194

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

\-0.0051 – 0.0439

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

0.130

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

delohome
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  7">

0.0194

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  8">

\-0.0051 – 0.0439

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  9">

0.130

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.003

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.050

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.054

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.066

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.066

</td>

</tr>

</table>

``` r
### Massachusetts (KY) Per-Capita Model 
model1ma = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "25"), family="gaussian")

model2ma = glm(per_capita_ethanol_total ~ relevel(as.factor(bar_open), ref="1"), data = subset(alco_policydf1, fips == "25"), family="gaussian")

tab_model(model1ma, model2ma, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2145

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2017 – 0.2274

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2114

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1982 – 0.2247

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0045

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0592 – 0.0501

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.871

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(bar\_open, ref
=<br>“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0205

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0151 – 0.0560

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.267

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.036

</td>

</tr>

</table>

``` r
### Missouri (MO) Per-Capita Model 
model1mo = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "29"), family="gaussian")

tab_model(model1mo, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2138

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2053 – 0.2223

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0075

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0434 – 0.0584

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.775

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.002

</td>

</tr>

</table>

``` r
### North Dakota (ND) Per-Capita Model 
model1nd= glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "38"), family="gaussian")

model2nd = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "38"), family="gaussian")

tab_model(model1nd, model2nd, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2659

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2550 – 0.2768

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2613

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2507 – 0.2719

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0114

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0768 – 0.0541

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.735

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0384

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0066 – 0.0702

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>0.024</strong>

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.003

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.142

</td>

</tr>

</table>

``` r
### Tennesse (TN) Per-Capita Model 
model1tn = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "47"), family="gaussian")

model2tn = glm(per_capita_ethanol_total ~ relevel(as.factor(bar_open), ref="1"), data = subset(alco_policydf1, fips == "47"), family="gaussian")

model3tn = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "47"), family="gaussian")

model4tn= glm(per_capita_ethanol_total ~ as.factor(bar_restrict), data = subset(alco_policydf1, fips == "47"), family="gaussian")

model5tn= glm(per_capita_ethanol_total ~ as.factor(tout_curb), data = subset(alco_policydf1, fips == "47"), family="gaussian")

tab_model(model1tn, model2tn, model3tn, model4tn, model5tn, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  4">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  5">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  6">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1822

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1766 – 0.1879

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1819

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1762 – 0.1876

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.1822

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.1766 – 0.1878

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.1830

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.1772 – 0.1888

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.1799

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.1744 – 0.1855

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0098

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0241 – 0.0437

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.576

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(bar\_open, ref
=<br>“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0107

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0135 – 0.0349

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.391

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.0110

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

\-0.0228 – 0.0449

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

0.528

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

bar\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

\-0.0060

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

\-0.0261 – 0.0142

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

0.565

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

tout\_curb
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.0185

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.0036 – 0.0335

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

<strong>0.020</strong>

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.009

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.022

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.012

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.010

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.148

</td>

</tr>

</table>

``` r
### Texas (TX) Per-Capita Model 

model1tx = glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1"), data = subset(alco_policydf1, fips == "48"), family="gaussian")

model2tx = glm(per_capita_ethanol_total ~ relevel(as.factor(bar_open), ref="1"), data = subset(alco_policydf1, fips == "48"), family="gaussian")

model3tx = glm(per_capita_ethanol_total ~ as.factor(rest_restrict), data = subset(alco_policydf1, fips == "48"), family="gaussian")

model4tx= glm(per_capita_ethanol_total ~ as.factor(bar_restrict), data = subset(alco_policydf1, fips == "48"), family="gaussian")

model5tx= glm(per_capita_ethanol_total ~ as.factor(tout_curb), data = subset(alco_policydf1, fips == "48"), family="gaussian")

tab_model(model1tx, model2tx, model3tx, model4tx, model5tx, digits = 4)
```

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col9">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  0">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  1">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  2">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  3">

p

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  4">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  5">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  6">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1934

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1862 – 0.2006

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1914

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.1842 – 0.1986

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.1910

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.1839 – 0.1980

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.1930

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

0.1859 – 0.2001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

<strong>\<0.001

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.1904

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.1833 – 0.1974

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0161

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0273 – 0.0595

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.472

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(bar\_open, ref
=<br>“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0223

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0007 – 0.0439

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

0.051

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

rest\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

0.0259

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

0.0048 – 0.0470

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

<strong>0.022</strong>

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

bar\_restrict
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

0.0293

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

\-0.0133 – 0.0719

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

0.187

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

tout\_curb
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col9">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  0">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  1">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  2">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  3">

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  4">

0.0250

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  5">

0.0060 – 0.0440

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  6">

<strong>0.014</strong>

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.015

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.107

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.145

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.051

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.164

</td>

</tr>

</table>

``` r
#### DID models for each state (prepost_rest_open = 3 levels)


### Alaska (AK) Per-Capita Models

##Option 1 
did1ak= glm(per_capita_ethanol_total ~ as.factor(prepost_rest_open) + as.factor(month_cat) + as.factor(prepost_rest_open)*as.factor(month_cat), data = subset(alco_policydf1, fips == "02"), family="gaussian")
  
## Option 2
did1ak= glm(per_capita_ethanol_total ~ as.factor(prepost_rest_open) + as.factor(treat_yr) + as.factor(month_cat) + as.factor(prepost_rest_open)*as.factor(treat_yr), data = subset(alco_policydf1, fips == "02"), family="gaussian")

## Option 3 - Don't think this is right
did1ak= glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1") + as.factor(prepost_rest_open) + as.factor(month_cat) + relevel(as.factor(restaurant_open), ref="1")*as.factor(prepost_rest_open), data = subset(alco_policydf1, fips == "02"), family="gaussian")

## Option 4 - 
did1ak= glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1") + as.factor(month_cat) + as.factor(treat_yr) + relevel(as.factor(restaurant_open), ref="1")*as.factor(treat_yr), data = subset(alco_policydf1, fips == "02"), family="gaussian")




tab_model(did1ak, digits = 4)
```

    ## Warning: Model matrix is rank deficient. Parameters
    ## relevel(as.factor(restaurant_open), ref = "1")0:as.factor(treat_yr)1 were not
    ## estimable.

<table style="border-collapse:collapse; border:none;">

<tr>

<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">

 

</th>

<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">

per capita ethanol
total

</th>

</tr>

<tr>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">

Predictors

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

Estimates

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

CI

</td>

<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">

p

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

(Intercept)

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2240

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.2086 – 0.2394

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

relevel(restaurant\_open,<br>ref =
“1”)0

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0040

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0294 – 0.0374

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.817

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[2\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0015

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0198 – 0.0229

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.888

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[3\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0022

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0236 – 0.0191

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.839

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[4\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0133

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0080 – 0.0347

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.234

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[5\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0468

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0681 – -0.0254

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[6\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0386

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0599 – -0.0172

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>0.002</strong>

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[7\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0075

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0138 – 0.0288

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.498

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[8\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0093

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0333 – 0.0148

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.459

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[9\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0550

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0336 – 0.0763

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[10\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0603

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0390 – 0.0816

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[11\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0755

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0541 – 0.0968

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

month\_cat
\[12\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0608

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.0394 – 0.0821

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

<strong>\<0.001

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">

treat\_yr
\[1\]

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0064

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

\-0.0161 – 0.0032

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">

0.206

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">

Observations

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">

36

</td>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">

R<sup>2</sup>
Nagelkerke

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">

0.931

</td>

</tr>

</table>

``` r
### Arkansas (AR) Per-Capita Models
did1ar= glm(per_capita_ethanol_total ~ relevel(as.factor(restaurant_open), ref="1") + as.factor(prepost_rest_open) + as.factor(month_cat), data = subset(alco_policydf1, fips == "05"), family="gaussian")
```
