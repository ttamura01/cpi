library(tidyverse)
library(ggtext)
library(patchwork)
library(glue)
library(scales)
library(fredr)

## download cip data

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

cpi <- fredr(series_id = "CPIAUCSL") %>% 
  select(date, cpi = value) 

# updates <- tribble(~date, ~cpi,
#                    "2024-07-01", 313.534)

# cpi <- rbind(cpi, updates)

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
    limits = c(NA, NA),
    # breaks = seq(NA, NA, NA),
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

