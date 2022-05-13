library(readr)
library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)
#WorldIncomeLevel <- read_csv("WorldIncomeLevel.csv")

# 合并数据
#GNIISO <- read_csv("GNIISO.csv")
#data = left_join(WorldIncomeLevel,GNIISO, by = "Country")
#write.csv(data,file = "incomelevel-0508.csv")
# excel操作梳理数据抬头

# 
#incomelevel_0508 <- read_csv("incomelevel-0508.csv")
#IDI_last_version <- read_csv("IDI_last version.csv")

#IdiGni = left_join(IDI_last_version,incomelevel_0508, by = "iso")
#write.csv(IdiGni,file = "Idi-Gni.csv")

data <- read_csv("idi-group-gni.csv")

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

country_highlight <- c(
  "Belarus","Jordan",
  "Norway","Singapore",
  "Niger","Rwanda",
  "Uzbekistan","Iran",
  "United States of America","China","Germany",
  "Russia"
)

test <- test %>%
  mutate(
    label = ifelse(country %in% country_highlight, country, "")
  )


expohi = ggplot(test, aes(GNI2020, B)) +
  geom_smooth(method="lm", formula = 'y ~ poly(x,2)' , col="black") +
  geom_point(
    aes(color = Group, fill = Group),
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
  xlab("GNI per capita, Atlas method (current US$)") +
  ylab("Global One Health Intrinsic Drivers index scores")+
  theme_test() 

ggsave("HDI-GIN-Group.pdf", width=8, height=4, dpi=600)



