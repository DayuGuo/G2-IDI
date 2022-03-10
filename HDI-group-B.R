library(readxl)
library(readr)
library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)

data <- read_csv("HDI-B.csv")

test <- data %>%
  mutate(
    `Human development groups` = case_when(
      `Human development groups` == "VERY HIGH HUMAN DEVELOPMENT" ~ "VERY HIGH HUMAN DEVELOPMENT",
      `Human development groups` == "HIGH HUMAN DEVELOPMENT" ~ "HIGH HUMAN DEVELOPMENT",
      `Human development groups` == "MEDIUM HUMAN DEVELOPMENT" ~ "MEDIUM HUMAN DEVELOPMENT",
      `Human development groups` == "LOW HUMAN DEVELOPMENT" ~ "LOW HUMAN DEVELOPMENT",
      TRUE ~ `Human development groups` # All the other remain the same
    )
  )

country_highlight <- c(
  "Germany", "Norway", "United States of America", "Greece", "Singapore", "Rwanda", 
  "Russia", "Venezuela", "Sudan", "Iraq", "Ghana", "Niger", "Chad", "Kuwait", 
  "Qatar", "Myanmar", "Nepal", "Chile", "Argentina", "Japan", "China"
)

test <- test %>%
  mutate(
    label = ifelse(country %in% country_highlight, country, "")
  )


expohi = ggplot(test, aes(HDI, B)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = `Human development groups`, fill = `Human development groups`),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  ) +
  # Add auto-positioned text
  geom_text_repel(
    aes(label = label),
    color = "black",
    size = 10/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654 # For reproducibility reasons
  ) +
  xlab("HDI") +
  ylab("Global One Health Intrinsic Drivers Index scores")+
  theme_test() 

ggsave("HDIgroup-B", units="in", width=8, height=4, dpi=600, compression = 'lzw')
