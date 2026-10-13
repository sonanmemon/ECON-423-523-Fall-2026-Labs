

# Author: Sonan Memon

# Load required libraries

library(WDI)

library(tidyverse)
library(dslabs)
library(gapminder)
library(dplyr)
library(ggplot2)

#------------------------------------------
# World Bank Data Import

# Define countries and indicators:

countries <- c("US", "CN")  # USA and China

indicators <- c(
  GDP = "NY.GDP.MKTP.CD",
  Exports = "NE.EXP.GNFS.CD",
  Imports = "NE.IMP.GNFS.CD",
  GDPGrowth = "NY.GDP.MKTP.KD.ZG",
  GDPPerCapita = "NY.GDP.PCAP.CD",
  GDPPerCapitaPPP = "NY.GDP.PCAP.PP.KD",
  Inflation = "FP.CPI.TOTL.ZG",
  Trade_GDP = "NE.TRD.GNFS.ZS",
  Investment_GDP = "NE.GDI.TOTL.ZS",
  FDI_GDP = "BX.KLT.DINV.WD.GD.ZS"
)

# Uncomment below to download data fresh from World Bank

data <- WDI(country = countries, indicator = indicators, 
         start = 2000, end = 2023)


data <- data %>%
  mutate(
    NetExports = Exports - Imports,
    NetExports_GDP_Ratio =
      (NetExports/ GDP) * 100
  ) %>%
  arrange(country, year)




write.csv(data, "US-China.csv", row.names = FALSE)



setwd("C:/Users/sonan/OneDrive/Documents/ECON-320-Fall-2025-GE")


data <- read.csv("US-China.csv")

# variance, covariance and correlations:


data_china <- subset(data, country == "China")

var(data_china$GDPGrowth)

cov(data_china$GDPGrowth, data_china$NetExports_GDP_Ratio, use = "complete.obs")

View(data)

cov_matrix <- cov(
  cbind(
    data_china$GDPGrowth,
    data_china$NetExports_GDP_Ratio
  ),
  use = "complete.obs"
)

cov_matrix

cor_matrix <- cor(
  cbind(
    data_china$GDPGrowth,
    data_china$NetExports_GDP_Ratio
  ),
  use = "complete.obs"
)

cor_matrix










#------------------------------------------
# Plot with Simulated Data

species <- rep(c("turtle", "turkey", "tiger", "tuna"), each = 3)

condition <- rep(c("thriving", "threatened", "extinct"), times = 4)

count <- abs(rnorm(n = 12, mean = 0, sd = 1))

sim_data <- data.frame(species, condition, count)

sim_data

# dodge or fill

# Bar chart with simulated data

p <-  ggplot(sim_data, aes(x = species, y = count, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Bar Chart of Simulated Data",
    x = "Conditions of Animals",
    y = "Count (Millions)"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p

# Export the plot to a file

ggsave("bar_chart.png", plot = p, width = 8, height = 6, dpi = 300)

# position = "stack:,
# position = "fill",
#Similar to "stack", but each bar is rescaled to the same height (100%).



#------------------------------------------
# Data Visualization: Histogram with gapminder data

gapminder <- gapminder %>%
  mutate(loggdp = log(gdpPercap))

ggplot(gapminder, aes(x = loggdp, fill = continent)) +
  geom_histogram(bins = 100) +
  labs(title = "Histogram of log GDP Per Capita", 
       x = "Log GDP Per Capita", y = "Count") +
  theme_minimal()

#------------------------------------------
# Data Visualization: Scatterplot with World Bank data


data <- read.csv("US-China.csv")


# Create Period factor variable
data$Period <- with(data, ifelse(year <= 1990, "1970–1990",
                                 ifelse(year <= 2010, "1990–2010", "2010–2023")))

# Log-transform GDP per capita and calculate net exports ratio:

data$log_gdp <- log(data$GDPPerCapita)
data$nx_gdp <- data$NetExports_GDP_Ratio

# Scatterplot: Net Exports ratio vs log GDP per capita by country:

p_scatter <- ggplot(data) +
  geom_point(aes(x = nx_gdp, y = log_gdp, color = country), alpha = 2) +
  geom_smooth(aes(x = nx_gdp, y = log_gdp, color = country), 
              method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title = "Net Exports and GDP",
    x = "Net Exports / GDP Ratio (%)",
    y = "Log GDP Per Capita (USD)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p_scatter)


#------------------------------------------
# Cross-Correlation Function (CCF)
# USA: Net Exports vs Log GDP Per Capita

us_data <- data %>%
  filter(country == "United States") %>%
  arrange(year) %>%
  filter(
    is.finite(nx_gdp),
    is.finite(log_gdp)
  )

# Calculate and plot cross-correlations
ccf(
  us_data$nx_gdp,
  us_data$log_gdp,
  lag.max = 5,
  na.action = na.fail,
  main = "CCF: US Net Exports and Log GDP Per Capita",
  xlab = "Lag (years)",
  ylab = "Cross-correlation"
)

# try theme_dark() or theme_void()

#------------------------------------------
# Data Visualization: Facet Wrap for China data

china_data <- subset(data, country == "China")

p_facet <- ggplot(china_data, aes(x = nx_gdp, y = log_gdp)) +
  geom_point(alpha = 2, color = "blue") +
  geom_smooth(method = "lm", color = "green", se = FALSE, linetype = "dashed") +
  facet_wrap(~Period) +
  labs(
    title = "Net Exports & GDP per Capita: China",
    x = "Net Exports / GDP Ratio (%)",
    y = "Log GDP Per Capita (USD)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Facet wrap: Creates multiple subplots, one per level of Period.

print(p_facet)

#------------------------------------------
# Data Visualization: Density plot for GDP Growth by country


means <- data %>%
  group_by(country) %>%
  summarise(mean_gdp = mean(GDPGrowth, na.rm = TRUE))

p_density <- ggplot(data, aes(x = GDPGrowth, fill = country)) +
  geom_density(alpha = 0.4) +
  geom_segment(
    data = means,
    aes(
      x = mean_gdp,
      xend = mean_gdp,
      y = 0,
      yend = Inf,
      color = country
    ),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_x_continuous(breaks = seq(-1, 20, by = 2)) +
  labs(
    title = "Densities of Growth: China vs USA",
    x = "GDP Growth Rate (%)",
    y = "Density"
  ) +
  theme_minimal()

p_density


# alpha in density means the density curve 
# (or filled area, if fill is set) will be 40% opaque or 60% transparent.



# Separate correlation heat-maps by country:

make_correlation_plot <- function(df, country_name) {
  
  vars <- df %>%
    select(
      NetExports_GDP_Ratio,
      GDPGrowth,
      FDI_GDP,
      Inflation
    )
  
  corr <- cor(
    vars,
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  corr_df <- as.data.frame(corr) %>%
    tibble::rownames_to_column("Variable1") %>%
    pivot_longer(
      cols = -Variable1,
      names_to = "Variable2",
      values_to = "Correlation"
    )
  
  ggplot(
    corr_df,
    aes(Variable1, Variable2, fill = Correlation)
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = sprintf("%.2f", Correlation)),
      size = 4
    ) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    coord_equal() +
    labs(
      title = paste("Correlation Heatmap:", country_name),
      x = NULL,
      y = NULL,
      fill = "Correlation"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(
        angle = 45, hjust = 1
      )
    )
}

# China
china_heatmap <- make_correlation_plot(
  filter(data, country == "China"),
  "China"
)

print(china_heatmap)

# United States
us_heatmap <- make_correlation_plot(
  filter(data, country == "United States"),
  "United States"
)


print(us_heatmap)


#------------------------------------------
# Violin Plots: Distribution of US Economic Variables

library(tidyverse)

# Filter USA data
usa_data <- data %>%
  filter(country == "United States")

# Select first four variables and reshape to long format
usa_long <- usa_data %>%
  select(
    GDPGrowth,
    FDI_GDP,
    Inflation,
    NetExports_GDP_Ratio
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(is.finite(Value))

# Create violin plots
p_violin_usa <- ggplot(
  usa_long,
  aes(x = Variable, y = Value)
) +
  geom_violin(
    fill = "steelblue",
    color = "darkblue",
    alpha = 0.7,
    na.rm = TRUE
  ) +
  geom_boxplot(
    width = 0.12,
    fill = "white",
    outlier.shape = NA
  ) +
  facet_wrap(
    ~ Variable,
    scales = "free_y"
  ) +
  labs(
    title = "Distribution of Economic Variables: USA",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

print(p_violin_usa)

# Save plot
ggsave(
  "usa_violin_plots.png",
  plot = p_violin_usa,
  width = 10,
  height = 7,
  dpi = 300
)




