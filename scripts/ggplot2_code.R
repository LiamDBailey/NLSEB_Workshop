library(dplyr)
plot_data <- ChickWeight %>%
  group_by(Diet, Time) %>%
  summarise(Weight = mean(weight))
ggplot(data = plot_data) +
  aes(x = Time, y = Weight,
      colour = as.factor(Diet)) +
  geom_line(size = 1) +
  geom_point(size = 4, colour = "black") +
  theme_classic() +
  theme(axis.title = element_text(size = 20))

ggplot(data = plot_data) +
  aes(x = Time, y = Weight,
      colour = as.factor(Diet)) +
  geom_point(size = 4, colour = "black") +
  geom_line(size = 1) +
  theme(axis.title = element_text(size = 20)) +
  theme_classic()
