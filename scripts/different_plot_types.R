library(gt)
library(dplyr)

mtcars %>%
  tibble::rownames_to_column(var = "car") %>%
  select(car, mpg) %>%
  arrange(desc(mpg)) %>%
  slice(1:10) %>%
  gt() %>%
  cols_label(car = "",
             mpg = "Efficiency (mpg)") %>%
  tab_header(title = md("Toyota Corolla is the most efficient car model available")) %>%
  tab_source_note(source_note = "Data: mtcars data in R") %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(3)),
      cell_text(weight = "bold")
    )
  ) %>%
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  data_color(columns = c(mpg),
             colors = c("#8b0000", "#50C878")) %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  ) %>%
  cols_width(c(car) ~ px(150),
             c(mpg) ~ px(200)) %>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    heading.align = "left")


library(ggplot2)

ggplot() +
  geom_line(aes(x = 1:length(AirPassengers),
                y = AirPassengers), size = 1) +
  labs(title = "Number of airline passengers has increased over time",
       x = "Time", y = "Number of passengers") +
  # scale_x_continuous(name = "Time") +
  # scale_y_continuous(name = "Number of passengers") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 10)),
        axis.title.x = element_text(colour = "black", size = 15, margin = margin(t = 10)),
        plot.margin = margin(t = 5, b = 5, l = 10, r = 20))

ggsave(filename = here("./plots/line_eg.png"),
       width = 10, height = 10, dpi = 300)

library(ggtext)
my_palette <- c("blue", "green", "red")
ggplot() +
  geom_point(data = mtcars, aes(x = mpg, y = disp, fill = as.factor(cyl)),
             shape = 21, size = 3, colour = "black") +
  geom_richtext(data = mtcars %>%
                  group_by(cyl) %>%
                  summarise(mpg = mean(range(mpg)),
                            disp = mean(range(disp))) %>%
                  mutate(colour = rev(my_palette)),
                aes(x = mpg + c(3, 7, 10), y = disp + c(30, 0, 0),
                    label = paste("<span style='color:", colour, "'>", cyl, "cylinder engine</span>")),
                label.colour = NA, fill = NA, fontface = "bold") +
  labs(title = "**4 cylinder engines are more efficienct**",
       x = "Fuel efficiency (mpg)", y = "Displacement of engine") +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        axis.text = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 10)),
        axis.title.x = element_text(colour = "black", size = 15, margin = margin(t = 10)),
        plot.margin = margin(t = 5, b = 5, l = 10, r = 20))

ggsave(filename = here("./plots/scatter_eg.png"),
       width = 10, height = 10, dpi = 300)

head(ChickWeight)

plot_data <- ChickWeight %>%
  mutate(start_weight = mean(weight[Time == 0])) %>%
  group_by(Diet) %>%
  summarise(`0` = first(start_weight),
            `21` = mean(weight[Time == 21])) %>%
  tidyr::pivot_longer(cols = `0`:`21`, names_to = "Time", values_to = "Weight") %>%
  mutate(Time = as.numeric(Time))

my_palette <- c("blue", "green", "red", "orange")

ggplot() +
  geom_line(data = plot_data,
            aes(x = Time, y = Weight, group = Diet,
                colour = Diet), size = 1) +
  geom_point(data = plot_data %>%
               filter(Time != 0),
             aes(x = Time, y = Weight, group = Diet,
                 colour = Diet),
             size = 3) +
  geom_point(data = plot_data %>%
               filter(Time == 0),
             aes(x = Time, y = Weight),
             size = 3, colour = "grey50") +
  geom_richtext(data = plot_data %>%
                  filter(Time != 0),
                aes(x = Time + 0.5, y = Weight,
                    colour = Diet, label = paste("Diet:", Diet)),
                label.colour = NA, fill = NA, fontface = "bold", hjust = 0) +
  coord_cartesian(clip = "off") +
  labs(x = "Age", y = "Weight (g)", title = "Diet 3 is most effective") +
  scale_y_continuous(limits = c(0, 300)) +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_markdown(),
        axis.text = element_text(colour = "black", size = 12),
        axis.title.y = element_text(colour = "black", size = 15, margin = margin(r = 10)),
        axis.title.x = element_text(colour = "black", size = 15, margin = margin(t = 10)),
        plot.margin = margin(t = 5, b = 5, l = 10, r = 50))


### AREA PLOT
all_df <- expand.grid(x = 1:100, y = 1:100)
cases  <- expand.grid(x = 1:15, y = (100 - 15):100)
deaths <- data.frame(x = 1, y = 99:100)

ggplot() +
  geom_tile(data = all_df, aes(x = x, y = y), colour = "white", fill = "grey85") +
  geom_tile(data = cases, aes(x = x, y = y), colour = "white", fill = "grey65") +
  geom_tile(data = deaths, aes(x = x, y = y), colour = "white", fill = "red") +
  geom_segment(aes(x = min(cases$x) - 0.5, xend = max(cases$x) + 0.5,
                   y = min(cases$y) - 0.5, yend = min(cases$y) - 0.5),
               size = 0.75, colour = "white",
               lineend = "round", linejoin = "round") +
  geom_segment(aes(x = max(cases$x) + 0.5, xend = max(cases$x) + 0.5,
                   y = min(cases$y) - 0.5, yend = max(cases$y) + 0.5),
               size = 0.75, colour = "white") +
  geom_segment(aes(x = min(deaths$x) - 0.5, xend = max(deaths$x) + 0.5,
                   y = min(deaths$y) - 0.5, yend = min(deaths$y) - 0.5),
               size = 0.75, colour = "white",
               lineend = "round", linejoin = "round") +
  geom_segment(aes(x = max(deaths$x) + 0.5, xend = max(deaths$x) + 0.5,
                   y = min(deaths$y) - 0.5, yend = max(deaths$y) + 0.5),
               size = 0.75, colour = "white") +
  geom_richtext(aes(x = 45, y = 10, label = "6 million vaccinated"), size = 10, fontface = "bold",
                fill = NA, label.color = NA, hjust = 0) +
  geom_richtext(aes(x = 16, y = 90, label = "2.4% contract<br>covid"),
                size = 10, fontface = "bold", colour = "white", lineheight = 1,
                fill = NA, label.color = NA, hjust = 0) +
  labs(title = "<span>Only </span><span style='color:red'>**0.02%**</span><span> of vaccinated Virginians<br>have died from Covid-19</span>",
       caption = "<span>Data: Virgina Department of Health</span>") +
  coord_equal(expand = FALSE) +
  theme_void() +
  theme(plot.title = element_markdown(lineheight = 1.2, size = 25, margin = margin(b = 10)),
        plot.subtitle = element_markdown(size = 17),
        plot.caption = element_markdown(colour = "grey35", size = 10),
        plot.margin = margin(t = 15, b = 15, r = 15, l = 15),
        text = element_text())

ggsave(filename = here("./plots/square_area.png"),
       width = 10, height = 10, dpi = 300)

