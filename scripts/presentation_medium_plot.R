library(ggplot2)
library(dplyr)
library(here)

showtext::showtext_auto()
sysfonts::font_add_google(name = "Quicksand", family = "Quicksand")

#Create plot (we just assume it's a simple linear relationship b/w your input and detail needed)
ggplot() +
  geom_line(aes(x = c(0, 1), y = c(1, 0)),
            size = 2, colour = "grey10") +
  labs(title = "THE COMMUNICATION CONTINUUM",
       subtitle = "When you can interact more with your audience your visualization needs less detail",
       x = "<-- LESS INTERACTION                              MORE INTERACTION -->",
       y = "<-- LESS DETAIL                      MORE DETAIL -->") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous() +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 17, family = "Quicksand"),
        plot.subtitle = element_text(size = 15, family = "Quicksand"),
        text = element_text(family = "Quicksand"))

ggsave(here("./plots/communication_continuum.png"))
