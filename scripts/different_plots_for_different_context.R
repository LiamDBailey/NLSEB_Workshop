## Libraries
library(ggplot2)
library(dplyr)
library(ggtext)
library(here)

## Create some made-up data (weight of animal before and after supp feeding)
## Feeding began at day 10
## Include multiple points to show that we will plot raw data too
## Before new diet, animals weight is unaffected by time (i.e. no relationship b/w time and weight)
set.seed(123)
control <- data.frame(time = rep(seq(0, 20, 2), times = 200)) %>%
  mutate(weight = rnorm(n = n(), mean = 10, sd = 2),
         treatment = "control")
## Treatment group increases exponentially
treatment <- data.frame(time = rep(seq(0, 20, 2), times = 200)) %>%
  mutate(weight = 10 + exp(time * 0.15) + rnorm(n = n(), mean = 0, sd = 2),
         treatment = "treatment")

plot_data <- bind_rows(control, treatment) %>%
  mutate(treatment_time = paste(treatment, time))
#Sample randomly so that sample sizes will differ
plot_data <- plot_data[sample(1:nrow(plot_data), size = 500, replace = FALSE), ]
summary_data <- plot_data %>%
  group_by(treatment, time) %>%
  summarise(avg_weight = mean(weight),
            n = n(), .groups = "drop") %>%
  ungroup() %>%
  mutate(y = case_when(treatment == "control" ~ 2,
                       treatment == "treatment" ~ 39))

#For a poster
ggplot() +
  geom_hline(yintercept = seq(5, 35, 5), colour = "grey75", alpha = 0.4) +
  geom_line(data = summary_data, aes(x = time, y = avg_weight, colour = treatment),
            size = 1) +
  geom_boxplot(data = plot_data, aes(x = time, y = weight, group = treatment_time,
                                     fill = treatment), width = 1) +
  geom_text(aes(x = -1, y = c(2, 39), label = "N: "), fontface = "bold") +
  geom_text(data = summary_data,
            aes(x = time, y = y, label = n, colour = treatment), fontface = "bold") +
  geom_richtext(aes(x = 19, y = c(15, 35), label = c("<span style='color:grey75'>**CONTROL**</span>",
                                                     "<span style='color:grey35'>**TREATMENT**</span>")),
                label.colour = NA, fill = NA) +
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 5), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  scale_fill_manual(values = c("grey75", "grey35")) +
  scale_colour_manual(values = c("grey75", "grey35")) +
  labs(x = "Day", y = "Weight (kg)",
       title = "<span style='color:grey35; font-size:15pt'>**TREATMENT DIET**</span> significantly increased weight",
       subtitle = "Individuals were tracked over 20 days",
       caption = "Author et al. (2022) Journal of Diets") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        axis.ticks = element_line(colour = "black"))

ggsave(here("./plots/poster_eg.png"))

#For a talk
#Show mean at day 0 and day 20
summary_data2 <- summary_data %>%
  filter(time %in% c(0, 20))

ggplot() +
  geom_line(data = summary_data, aes(x = time, y = avg_weight, colour = treatment), size = 1.5) +
  geom_point(data = summary_data2, aes(x = time, y = avg_weight, colour = treatment), size = 4) +
  geom_text(data = summary_data2, aes(x = time + c(-1.25, 1.25, -1.25, 1.25),
                                      y = avg_weight + c(-1, 0, 1, 0),
                                          label = paste0(round(avg_weight), "kg"),
                                      colour = treatment),
            fontface = "bold") +
  geom_richtext(aes(x = 19, y = c(12, 32), label = c("<span style='color:grey75'>**CONTROL**</span>",
                                                     "<span style='color:grey35'>**TREATMENT**</span>")),
                label.colour = NA, fill = NA) +
  scale_y_continuous(limits = c(0, 40),
                     breaks = seq(0, 40, 5),
                     labels = paste0(seq(0, 40, 5), "kg "),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 20, 5),
                     limits = c(-2, 22)) +
  scale_fill_manual(values = c("grey75", "grey35")) +
  scale_colour_manual(values = c("grey75", "grey35")) +
  labs(x = "Day", y = "Weight (kg)",
       title = "<span style='color:grey35; font-size:15pt'>**TREATMENT DIET**</span> significantly increased weight<br>") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_markdown(lineheight = 1),
        axis.text = element_text(colour = "black", size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(colour = "black", size = 15),
        axis.ticks = element_line(colour = "black"))

ggsave(here("./plots/talk_eg.png"))
