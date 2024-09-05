install.packages("tidyverse")
library(tidyverse)

tr <- as_tibble(trees)
tb <- tibble(a = 1:5, b = 1,sea = "ocean", z = (a + b)^2)

url <- "http://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2022.csv"
q_factor_monthly <- read_csv(url)

pew <- read_csv("data/pew.csv")

pew_long <- pivot_longer(data = pew, 
                         col = -c('Religious tradition','Sample size'),
                         names_to = "income_level",
                         values_to = "sample_pc")

#-c means: dont modify these two variables
#the name of the variable will go to income
#values go to sample_pc

activities <- read_csv("data/activities.csv")

ac_w1 <- pivot_longer(
  data = activities,
  cols = c(work.T1:talk.T2),
  names_to = "variable",
  values_to = "value"
) 

#c means: touch these variables, there is a : to capture all variables.

ac_w2 <- separate(data = ac_w1, col = variable, 
                  into = c("action", "time"))
#seperate allows us to break the one variable to two
#any other seperator other than dot, should be identified. 

activities_wider <-  pivot_wider(data = ac_w2, names_from = "action", 
                                 values_from = "value")

library(tidyverse)
library(dbplyr)

res <- dbSendQuery(wrds, "select gvkey,conm,fyear,revt,ni,at,dltt,teq 
                   from comp.funda
                   where fyear between '2000' and '2020'and exchg = '11' 
                   and datafmt = 'STD' and consol = 'C' and indfmt = 'INDL'")
nyse_df <- dbFetch(res, n=-1)
dbClearResult(res)

nyse_tbl<- tbl(wrds, sql("select * from comp.funda")) |>
  filter(fyear >= '2000' & fyear <= '2020', 
         exchg == '11',datafmt == 'STD',consol == 'C',indfmt == 'INDL') |>
  select(gvkey,conm,fyear,revt,ni,at,dltt,teq,naicsh) |> collect()

nyse_tbl |> filter(fyear == '2005' , revt >='100')

nyse_tbl |> 
  filter(fyear == '2005', revt >= 100) |> 
  arrange(desc(at))

nyse_ratio <- nyse_tbl |> filter(!(str_detect(naicsh, "^52"))) |>
  mutate(npm = ni/revt, ato = revt/at, de = dltt/teq) |>
  arrange(fyear, desc(at))

nyse_tbl |> 
  filter(revt == 0,!(str_detect(naicsh, "^52"))) |> 
  arrange(desc(at))

nyse_ratio <- nyse_tbl |> 
  filter(!(str_detect(naicsh, "^52"))) |>
  mutate(npm = ni/revt, ato = revt/at, de = dltt/teq) |>
  mutate(npm  = replace(npm ,revt == 0,NA))|>
  arrange(fyear, desc(at))

nyse_ratio |> summarise(AvgRev = mean(revt,  na.rm = TRUE))

nyse_ratio |> summarise(m_at = median(at,  na.rm = TRUE))

nyse_ratio |> summarise(sd_npm = sd(npm,  na.rm = TRUE))

nyse_ratio |> summarise(max_de = max(de,  na.rm = TRUE))

nyse_ratio |> filter(fyear=="2010") |> summarise(avg_turnover = mean(ato,  na.rm = TRUE))

nyse_ratio |> filter(at >= 500, fyear=="2015") |> summarise(med_npm = median(npm,  na.rm = TRUE))

nyse_ratio |> group_by(fyear) |> 
  summarise(AvgRev = mean(revt, na.rm = TRUE))

sum1 <- nyse_ratio |>
  summarise(across(
    .cols = c(revt,ni, at, dltt, teq, npm, ato, de), 
    .fns = list(Mean = mean, Median = median, SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))
#use pivot to make it longer












