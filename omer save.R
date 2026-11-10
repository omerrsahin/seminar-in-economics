
if(!require(readxl)) install.packages("readxl")
if(!require(ggplot2)) install.packages("ggplot2")
library(readxl)
library(ggplot2)

bf_data <- read_excel(file.choose())

bf_data <- bf_data[!is.na(bf_data$Time), ]

gdp_col <- "GDP per capita (current US$) [NY.GDP.PCAP.CD]"
co2_col <- "Carbon dioxide (CO2) emissions excluding LULUCF per capita (t CO2e/capita) [EN.GHG.CO2.PC.CE.AR5]"

bf_data$ln_Y <- log(bf_data[[gdp_col]])
bf_data$ln_CO2 <- log(bf_data[[co2_col]])
bf_data$ln_Y_sq <- (bf_data$ln_Y)^2

ekc_model <- lm(ln_CO2 ~ ln_Y + ln_Y_sq, data = bf_data)

ekc_plot <- ggplot(bf_data, aes(x = ln_Y, y = ln_CO2)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.7) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), 
              color = "orange", linewidth = 1.2, se = FALSE) +
  labs(title = "Environmental Kuznets Curve: Burkina Faso",
       subtitle = "1970 - 2024",
       x = "Natural Log of GDP per capita",
       y = "Natural Log of CO2 Emissions per capita") +
  theme_light() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# 
print(ekc_plot)

#
cat("\n--- OLS REGRESYON SONU??LARI ---\n")
summary(ekc_model)
