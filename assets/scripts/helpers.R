# Packages
library(tidyverse)
library(patchwork)
library(ggridges)
library(ggrepel)
library(knitr)
library(kableExtra)
library(latex2exp)
library(lingStuff)
library(broom)
library(viridis)
library(ggfortify)
library(untidydata)

# set seed for reproducibility
set.seed(12345)

# Plot helpers
my_theme <- function(...) {
  list(
    theme_minimal(base_family = 'Times', base_size = 20), 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  )
}

# QQ plot function
gg_qqplot <- function(vec) {

  # Get slope and intercept for qqline
  # from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  int <- y[1L] - slope * x[1L]

  # Calculate vec and qq correlation
  the_cor <- cor(vec, qqnorm(vec)$x)

  # Create plot
  tibble(resid = vec) %>%
  ggplot(., aes(sample = resid)) + 
    geom_point(stat = 'qq', shape = 21, color = "grey30", 
               fill = 'lightblue') + 
    geom_abline(slope = slope, intercept = int) + 
    annotate("text", x = -1, y = max(y), 
             label = paste0("r = ", round(the_cor, 4))) +
    my_theme() + 
    labs(x = "Theoretical quantiles", y = "Sample quantiles") + 
    theme(axis.line = element_line(colour = "black"))

}
