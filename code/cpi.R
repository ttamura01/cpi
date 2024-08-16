library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(scales)

## download cip data
cpi <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCSL&scale=left&cosd=1947-01-01&coed=2024-06-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-07-28&revision_date=2024-07-28&nd=1947-01-01") %>% 
  rename(date = DATE, cpi = CPIAUCSL)

updates <- tribble(~date, ~cpi,
                   "2024-07-01", 313.534)

cpi <- rbind(cpi, updates)

tail(cpi)

summary(cpi)

ggplot(cpi, aes(x = date, y = cpi)) +
  geom_line()

## initial data 
initial_data <- cpi %>% 
  slice_min(date)

initial_date <- initial_data$date
initial_cpi <- initial_data$cpi
initial_cpi_label <- round(initial_cpi, 2)
initial_year <- year(initial_date)

## latest data
latest_data <- cpi %>% 
  slice_max(date)

latest_date <- latest_data$date  
latest_cpi <- latest_data$cpi
latest_cpi_label <- round((latest_cpi),2)

## interval
class(latest_date)

interval <- interval(initial_date, latest_date) / years(1)
interval_label <- round(interval)

## multiple
multiple <-  (latest_cpi/initial_cpi)
multiple_label <- round(multiple, 1)

#last = A(1 + i) ^n

#313 = 21.5*(1 + i)^77.33

r <- (multiple^(1/interval) -1)*100
r_label <- round(r, 2)

cpi %>% 
  ggplot(aes(x = date, y = cpi)) +
  geom_line() +
  geom_text(data=latest_data, aes(x = date, y = cpi, label = latest_cpi_label, vjust = -0.5), color = "blue") +
  scale_y_continuous(
    limits = c(0, 320),
    breaks = seq(0, 320, 50),
    labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US CPI increased {multiple_label} times to {latest_cpi_label}, past for {interval_label} years from {initial_cpi_label} in {initial_year} (= annual rate at {r_label}%)"),
       x = NULL,
       y = "Consumer Price Index") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"), 
    legend.position = "none"
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_pop_gdp/figures/US_CPI.png", width = 6, height = 4)

cpi %>% slice_max(date)
cpi_2020 <- cpi %>% filter(date == "2020-01-01")
latest_cpi  

