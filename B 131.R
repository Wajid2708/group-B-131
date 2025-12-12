
library(ggplot2)
library(dplyr)

data <- read.csv("data_kaggle.csv", stringsAsFactors = FALSE)

data$Price_numeric <- gsub("RM|,", "", data$Price)
data$Price_numeric <- ifelse(grepl("K$", data$Price_numeric),
                             as.numeric(sub("K$", "", data$Price_numeric)) * 1000,
                             ifelse(grepl("M$", data$Price_numeric),
                                    as.numeric(sub("M$", "", data$Price_numeric)) * 1e6,
                                    as.numeric(data$Price_numeric)))

subdata <- data %>%
  filter(Property.Type %in% c("Condominium", "Serviced Residence")) %>%
  mutate(Price_Million = Price_numeric / 1e6)

ggplot(subdata, aes(x = Property.Type, y = Price_Million)) +
  geom_boxplot() +
  labs(title = "Distribution of property prices by type",
       x = "Property Type",
       y = "Price (Million RM)") +
  theme_minimal()

ggplot(subdata, aes(x = Price_Million, fill = Property.Type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Histogram of Prices by Property Type",
       x = "Price (Million RM)",
       y = "Count") +
  theme_minimal() +
  facet_wrap(~Property.Type)

furnish_summary <- subdata %>%
  group_by(Property.Type, Furnishing) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Property.Type) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(Furnishing == "Fully Furnished")

ggplot(furnish_summary, aes(x = Property.Type, y = Proportion * 100)) +
  geom_col() +
  labs(title = "Proportion of Fully Furnished Listings by Property Type",
       x = "Property Type",
       y = "Fully Furnished Proportion (%)") +
  theme_minimal()

t_test_result <- t.test(Price_numeric ~ Property.Type, data = subdata)
print(t_test_result)

