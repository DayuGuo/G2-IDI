library(readr)
library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)

idi_group_gni <- read_csv("idi-group-gni.csv")
HDI <- read_csv("HDI.csv")
EXP <- read_csv("EXP.csv")
GDP <- read_csv("GDP per capita (current US$).csv")
Forest <- read_csv("Forest area (% of land area).csv")

data = left_join(idi_group_gni,HDI, by = "iso")

data = left_join(data,EXP, by = "iso")
data = left_join(data,GDP, by = "iso")
data = left_join(data,Forest, by = "iso")

write.csv(data,file = "ALl-Regression model.csv")




test <- data %>%
  mutate(
    Group = case_when(
      Group == "Low income" ~ "Low income",
      Group == "Upper middle income" ~ "Upper middle income",
      Group == "Lower middle income" ~ "Lower middle income",
      Group == "High income" ~ "High income",
      TRUE ~ Group # All the other remain the same
    )
  )

ggplot(test, aes(HDI, B)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = Group, fill = Group),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  )+
  xlab("Human development index") +
  ylab("Global One Health Intrinsic Drivers index scores")+
  theme_test() 
ggsave("HDI-idi.pdf", width=8, height=4, dpi=600)



ggplot(test, aes(GDP2020, B)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = Group, fill = Group),
    size = 2.5, alpha = 0.5, 
    shape = 21 # This is a dot with both border (color) and fill.
  )+
  xlab("GDP per capita (current US$)") +
  ylab("Global One Health Intrinsic Drivers index scores")+
  theme_test() 
ggsave("GDP-idi.pdf", width=8, height=4, dpi=600)
