library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)

library(readr)
library(esquisse)

ohi <- read_csv("IDI_last version.csv")
ohi$region = as.factor(ohi$region)

ohi$region = ordered(ohi$region,levels = c('Sub-Saharan Africa','South Asia',"North America",'Middle East and North Africa',
                                           'Latin America and The Caribbean','Europe and Central Asia','East Asia and Pacific'))


ohiseve = ggplot(ohi, aes(x=region, y=B, fill=region)) +
  geom_boxplot(show.legend = FALSE)+coord_flip()+ylab("Global One Health Intrinsic Drivers Index scores")+xlab("Regions")+
  geom_jitter(shape=16, position=position_jitter(0.2), show.legend = FALSE)+
  scale_y_continuous(breaks = c(30,35,40,45,50,55,60,65))+
  theme_classic()+
  theme(legend.title = element_blank(),text = element_text(size = 10))
        
        #axis.title = element_text(size = 14)

ggsave("test.tiff", units="in", width=8, height=4, dpi=600, compression = 'lzw')


### CDI

library(readr)
data <- read_csv("IDI分布情况.csv")
library(ggridges)
library(ggplot2)

data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# Plot
cid=data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 10),
    text = element_text(size = 10)
  ) +
  xlab("Scores") +
  ylab("Density")

ggsave("cditest.pdf", units="in", width=8, height=4, dpi=600)

#test
library(ggplot2)
library(ggridges)
iris  = iris

library(readr)
data <- read_csv("IDI分布情况.csv")

data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))

# Plot
cid=data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  stat_density_ridges(scale = 0.9,
                      quantile_lines = TRUE)


data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text))+
  geom_density_ridges(
    jittered_points = TRUE, quantile_lines = TRUE, scale = 0.6, alpha = 0.7,
    vline_size = 1, vline_color = "red",
    point_size = 0.4, point_alpha = 1,
    position = position_raincloud(adjust_vlines = TRUE)
  )+
  theme(legend.position="none")+
  xlab("Scores") +
  ylab("Density")+
  theme_ridges()
ggsave("cditest.pdf", units="in", width=12, height=4, dpi=600)


