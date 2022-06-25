#Use data from OurWorldInData (TidyTuesday)
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(here)
library(ggrepel)

all_data    <- tt_load('2021-10-12')
focal_data  <- all_data$`capture-fishery-production`

#Identify top producers as of 2018
top_Codes <- focal_data %>%
  filter(Year == max(Year) & !is.na(Code)) %>%
  arrange(desc(`Capture fisheries production (metric tons)`)) %>%
  slice(1:10) %>%
  pull(Code)

#Identify all countries (removing summary categories)
all_Codes <- focal_data %>%
  filter(!is.na(Code) & !(Code %in% c("OWID_CIS", "OWID_WRL"))) %>%
  pull(Code)

#Filter out top countries
top_countries <- focal_data %>%
  filter(Code %in% top_Codes) %>%
  rename(catch = 4) %>%
  #Make col to distinguish China
  mutate(China = Entity == "China")

all_countries <- focal_data %>%
  filter(Code %in% all_Codes) %>%
  rename(catch = 4) %>%
  #Make col to distinguish China
  mutate(China = Entity == "China")

## CLUTTERED EXAMPLE
ggplot(data = all_countries) +
  geom_line(aes(x = Year, y = catch, group = Entity,
                colour = China)) +
  geom_point(aes(x = Year, y = catch,
                 colour = China), size = 1) +
  scale_x_continuous(breaks = seq(1960, 2020, 2)) +
  scale_colour_discrete(labels = c("Other", "China")) +
  labs(y = "Capture fisheries production (metric tons)",
       title = "Fisheries yield over time",
       subtitle = "Data since 1960. Production is measured in metric tons per year",
       caption = "Data: Our World in Data") +
  theme(panel.background = element_rect(colour = "black"),
        plot.background = element_rect(colour = "black", size = 0.75),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "grey80"),
        legend.title = element_blank())

ggsave(here("./plots/cluttered_eg.png"), height = 16, width = 22, units = "cm")

#De-cluttered example plot
ggplot() +
  geom_line(data = all_countries,
            aes(x = Year, y = catch/1e+06, group = Entity, colour = China)) +
  geom_text(data = filter(all_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06, label = Entity),
            colour = "#00bfc4", hjust = -0.25) +
  geom_text(data = filter(all_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "Other"),
            colour = "#f9766e", hjust = -0.25) +
  coord_cartesian(clip = "off") +
  # scale_colour_manual(values = c("grey75", "grey25")) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(y = "Fisheries production (million metric tons)",
       title = "Fisheries yield over time",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 30))

ggsave(here("./plots/uncluttered_eg.png"), height = 16, width = 22, units = "cm")

#Example #1. Use pre-attentive traits to draw attention to most important points
#i.e. China has increase way above other countries over time
ggplot() +
  geom_line(data = filter(all_countries, Entity != "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 0.35, colour = "grey75") +
  geom_line(data = filter(all_countries, Entity == "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 1, colour = "#DC343B") +
  geom_text(data = filter(all_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06, label = Entity),
            colour = "#DC343B", fontface = "bold", hjust = -0.25) +
  geom_text(data = filter(all_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "Other"),
            colour = "grey75", hjust = -0.25) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(y = "Fisheries production (million metric tons)",
       title = "Fisheries yield over time",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 30))

ggsave(here("./plots/attention_eg1.png"), height = 16, width = 22, units = "cm")

#Example #2. Use pre-attentive traits to highlight *importance*
#Don't overuse them (or they become meaningless)
ggplot() +
  geom_line(data = filter(all_countries, Entity != "China"),
            aes(x = Year, y = catch/1e+06, colour = Entity),
            size = 0.35) +
  geom_line(data = filter(all_countries, Entity == "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 1, colour = "#DC343B") +
  geom_text(data = filter(all_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06, label = Entity),
            colour = "#DC343B", fontface = "bold", hjust = -0.25) +
  geom_text(data = filter(all_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "Other"),
            colour = "black", hjust = -0.25) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(y = "Fisheries production (million metric tons)",
       title = "Fisheries yield over time",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 30))

ggsave(here("./plots/attention_eg2.png"), height = 16, width = 22, units = "cm")

#Example #3. Draw attention using text
ggplot() +
  geom_line(data = filter(all_countries, Entity != "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 0.35, colour = "grey75") +
  geom_line(data = filter(all_countries, Entity == "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 1, colour = "#DC343B") +
  geom_text(data = filter(all_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06, label = Entity),
            colour = "#DC343B", fontface = "bold", hjust = -0.25) +
  geom_text(data = filter(all_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "Other"),
            colour = "grey75", hjust = -0.25) +
  geom_segment(data = filter(top_countries, Year == 1995 & Entity == "China"),
               aes(x = Year - 12, xend = Year,
                   y = (catch/1e+06) + 1.5, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 2015 & Entity == "China"),
               aes(x = Year - 5, xend = Year,
                   y = (catch/1e+06) - 4, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 1960 & Entity == "China"),
               aes(x = Year + 5, xend = Year,
                   y = (catch/1e+06) + 3.5, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_point(data = filter(top_countries, Year %in% c(1960, 1995, 2015) & Entity == "China"),
             aes(x = Year,
                 y = (catch/1e+06)),
             size = 3, colour = "#DC343B") +
  geom_text(data = filter(top_countries, Year == 1995 & Entity == "China"),
            aes(x = Year - 14, y = (catch/1e+06) + 3,
                label = "China becomes world's largest\nproducer in 1995"),
            size = 4, colour = "grey25") +
  geom_text(data = filter(top_countries, Year == 2015 & Entity == "China"),
            aes(x = Year - 5, y = (catch/1e+06) - 5,
                label = "China's highest yield was\nmore than 16 million metric tons"),
            size = 4, colour = "grey25") +
  geom_text(data = filter(top_countries, Year == 1960 & Entity == "China"),
            aes(x = Year + 7, y = (catch/1e+06) + 5,
                label = "China is\n4th largest producer"),
            size = 4, colour = "grey25") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(y = "Fisheries production (million metric tons)",
       title = "China has the highest\nfishing yield of any country",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 30),
        plot.title = element_text(hjust = 0.5, colour = "grey25"))

ggsave(here("./plots/attention_eg3.png"), height = 16, width = 22, units = "cm")

#Example #4. Example of clean text with pre-attentive attributes!
ggplot() +
  geom_line(data = filter(all_countries, Entity != "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 0.35, colour = "grey75") +
  geom_line(data = filter(all_countries, Entity == "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 1, colour = "#DC343B") +
  geom_text(data = filter(all_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06, label = toupper(Entity)),
            colour = "#DC343B", fontface = "bold", hjust = -0.25) +
  geom_text(data = filter(all_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "OTHER"),
            colour = "grey75", hjust = -0.25) +
  geom_segment(data = filter(top_countries, Year == 1995 & Entity == "China"),
               aes(x = Year - 12, xend = Year - 12,
                   y = (catch/1e+06) + 0.75, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 1995 & Entity == "China"),
               aes(x = Year - 12.1, xend = Year,
                   y = (catch/1e+06), yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 2015 & Entity == "China"),
               aes(x = Year, xend = Year,
                   y = (catch/1e+06) + 2, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 2015 & Entity == "China"),
               aes(x = Year, xend = Year - 3,
                   y = (catch/1e+06) + 2, yend = catch/1e+06 + 2),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 1960 & Entity == "China"),
               aes(x = Year, xend = Year,
                   y = catch/1e+06, yend = catch/1e+06 + 11.25),
               size = 0.5, colour = "grey25") +
  geom_point(data = filter(top_countries, Year %in% c(1960, 1995, 2015) & Entity == "China"),
             aes(x = Year,
                 y = (catch/1e+06)),
             size = 3, colour = "#DC343B") +
  geom_richtext(data = filter(top_countries, Year == 1995 & Entity == "China"),
                aes(x = Year - 17, y = (catch/1e+06) + 2.5,
                    label = "<span style='color:#DC343B'>**1995**</span><br>China becomes<br>largest producer"),
                size = 4, colour = "grey25", hjust = 0,
                label.colour = NA, fill = NA) +
  geom_richtext(data = filter(top_countries, Year == 2015 & Entity == "China"),
                aes(x = Year - 15, y = (catch/1e+06) + 2.5,
                    label = "<span style='color:#DC343B'>**2015**</span><br>China catches over<br>**16 <i>million</i> tons**<br>of seafood"),
                size = 4, colour = "grey25", hjust = 0,
                label.colour = NA, fill = NA) +
  geom_richtext(data = filter(top_countries, Year == 1960 & Entity == "China"),
                aes(x = Year - 0.5, y = (catch/1e+06) + 13,
                    label = "<span style='color:#DC343B'>**1960**</span><br>China is world's<br>4th largest producer"),
                size = 4, colour = "grey25", hjust = 0,
                label.colour = NA, fill = NA) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(y = "Fisheries production (million metric tons)",
       title = "**China has the highest fishing yield of any country**",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(colour = "grey25", size = 12),
        axis.title.y = element_text(colour = "grey25", size = 13,
                                    margin = margin(r = 7)),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 35),
        plot.title = element_markdown(hjust = 0, colour = "grey25", margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0))

ggsave(here("./plots/attention_eg4.png"), height = 16, width = 22, units = "cm")

#Draw attention #5. NOT USING PRIME-REALESTATE

#Here, we will focus on bar graph of 2018
top_countries_2018 <- top_countries %>%
  filter(Year == 2018)

ggplot(data = top_countries_2018) +
  geom_col(aes(x = Entity, y = catch/1e+06, fill = China)) +
  geom_text(aes(x = Entity, y = catch/1e+06 - 0.6, label = round(catch/1e+06, 1)),
            colour = "white") +
  geom_richtext(aes(x = "India", y = 12,
                    label = "<span style='color:#DC343B; font-size:15pt'>**China**</span> caught twice<br>as much seafood as any other country"),
                hjust = 0, label.color = NA, fill = NA) +
  scale_fill_manual(values = c("grey75", "#DC343B")) +
  labs(y = "Fisheries production (million metric tons)",
       title = "<span style='color:#DC343B; font-size:15pt'>**China**</span> was the most productive fishing nation in 2018",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(colour = "grey25", size = 9),
        axis.title.y = element_text(colour = "grey25", size = 13,
                                    margin = margin(r = 7)),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 15),
        plot.title = element_markdown(hjust = 0, colour = "grey25", margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0))

ggsave(here("./plots/attention_eg5.png"), height = 16, width = 22, units = "cm")

## EXAMPLE 6: USING PRIME-REALESTATE

ggplot(data = top_countries_2018 %>%
         mutate(Entity = (forcats::fct_reorder(.f = toupper(Entity), .x = catch, .desc = FALSE)))) +
  coord_flip() +
  geom_col(aes(x = Entity, y = catch/1e+06, fill = China)) +
  geom_text(aes(x = Entity, y = catch/1e+06 - 0.6, label = round(catch/1e+06, 1)),
            colour = "white") +
  geom_richtext(aes(x = "PERU", y = 8,
                    label = "<span style='color:#DC343B; font-size:15pt'>**China**</span> caught twice<br>as much seafood as any other country"),
                hjust = 0, label.color = NA, fill = NA) +
  scale_fill_manual(values = c("grey75", "#DC343B")) +
  scale_y_continuous(position = "right",
                     limits = c(0, 15),
                     breaks = seq(0, 15, 5),
                     expand = c(0, 0)) +
  labs(title = "<span style='color:#DC343B; font-size:15pt'>**China**</span> was the most productive fishing nation in 2018",
       subtitle = "Fisheries production (million metric tons)",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_markdown(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 20, b = 20, l = 20, r = 20))

ggsave(here("./plots/attention_eg6.png"), height = 16, width = 22, units = "cm")

## EXAMPLE 7: NOT USING PRIME-REALESTATE
#What if we arranged things in the opposite order?!

ggplot(data = top_countries_2018 %>%
         mutate(Entity = (forcats::fct_reorder(.f = toupper(Entity), .x = catch, .desc = TRUE)))) +
  coord_flip() +
  geom_col(aes(x = Entity, y = catch/1e+06, fill = China)) +
  geom_text(aes(x = Entity, y = catch/1e+06 - 0.6, label = round(catch/1e+06, 1)),
            colour = "white") +
  geom_richtext(aes(x = "PERU", y = 8,
                    label = "<span style='color:#DC343B; font-size:15pt'>**China**</span> caught twice<br>as much seafood as any other country"),
                hjust = 0, label.color = NA, fill = NA) +
  scale_fill_manual(values = c("grey75", "#DC343B")) +
  scale_y_continuous(position = "right",
                     limits = c(0, 15),
                     breaks = seq(0, 15, 5),
                     expand = c(0, 0)) +
  labs(title = "<span style='color:#DC343B; font-size:15pt'>**China**</span> was the most productive fishing nation in 2018",
       subtitle = "Fisheries production (million metric tons)",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_markdown(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = 0),
        axis.text.y = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(t = 20, b = 20, l = 20, r = 20))

ggsave(here("./plots/attention_eg7.png"), height = 16, width = 22, units = "cm")

## EXAMPLE 8: SUMMARISE WHERE ACCEPTABLE

## Use average of all countries
other_countries_avg <- top_countries %>%
  filter(Entity != "China") %>%
  group_by(Year) %>%
  summarise(mean = mean(catch/1e+06, na.rm = TRUE))

ggplot() +
  geom_line(data = filter(top_countries, Entity != "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 0.35, colour = "grey75", alpha = 0.65) +
  geom_line(data = other_countries_avg,
            aes(x = Year, y = mean),
            size = 1, colour = "grey75") +
  geom_line(data = filter(top_countries, Entity == "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 1, colour = "#DC343B") +
  geom_text(data = filter(top_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06, label = toupper(Entity)),
            colour = "#DC343B", fontface = "bold", hjust = -0.25) +
  geom_text(data = filter(top_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "OTHER"),
            colour = "grey75", hjust = -0.25) +
  geom_segment(data = filter(top_countries, Year == 1995 & Entity == "China"),
               aes(x = Year - 12, xend = Year - 12,
                   y = (catch/1e+06) + 0.75, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 1995 & Entity == "China"),
               aes(x = Year - 12.1, xend = Year,
                   y = (catch/1e+06), yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 2015 & Entity == "China"),
               aes(x = Year, xend = Year,
                   y = (catch/1e+06) + 2, yend = catch/1e+06),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 2015 & Entity == "China"),
               aes(x = Year, xend = Year - 3,
                   y = (catch/1e+06) + 2, yend = catch/1e+06 + 2),
               size = 0.5, colour = "grey25") +
  geom_segment(data = filter(top_countries, Year == 1960 & Entity == "China"),
               aes(x = Year, xend = Year,
                   y = catch/1e+06, yend = catch/1e+06 + 11.25),
               size = 0.5, colour = "grey25") +
  geom_point(data = filter(top_countries, Year %in% c(1960, 1995, 2015) & Entity == "China"),
             aes(x = Year,
                 y = (catch/1e+06)),
             size = 3, colour = "#DC343B") +
  geom_richtext(data = filter(top_countries, Year == 1995 & Entity == "China"),
                aes(x = Year - 17, y = (catch/1e+06) + 2.5,
                    label = "<span style='color:#DC343B'>**1995**</span><br>China becomes<br>largest producer"),
                size = 4, colour = "grey25", hjust = 0,
                label.colour = NA, fill = NA) +
  geom_richtext(data = filter(top_countries, Year == 2015 & Entity == "China"),
                aes(x = Year - 15, y = (catch/1e+06) + 2.5,
                    label = "<span style='color:#DC343B'>**2015**</span><br>China catches over<br>**16 <i>million</i> tons**<br>of seafood"),
                size = 4, colour = "grey25", hjust = 0,
                label.colour = NA, fill = NA) +
  geom_richtext(data = filter(top_countries, Year == 1960 & Entity == "China"),
                aes(x = Year - 0.5, y = (catch/1e+06) + 13,
                    label = "<span style='color:#DC343B'>**1960**</span><br>China is world's<br>4th largest producer"),
                size = 4, colour = "grey25", hjust = 0,
                label.colour = NA, fill = NA) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  scale_y_continuous(limits = c(0, 20)) +
  labs(y = "Fisheries production (million metric tons)",
       title = "**China has the highest fishing yield of any country**",
       subtitle = "Countries with 10 highest fishing yields (as of 2018)",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(colour = "grey25", size = 12),
        axis.title.y = element_text(colour = "grey25", size = 13,
                                    margin = margin(r = 7)),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 35),
        plot.title = element_markdown(hjust = 0, colour = "grey25"),
        plot.subtitle = element_markdown(hjust = 0, colour = "grey25", margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0))

ggsave(here("./plots/attention_eg8.png"), height = 16, width = 22, units = "cm")

## EXAMPLE 9: INACCESSIBLE

ggplot() +
  geom_line(data = filter(top_countries, Entity != "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 0.35, colour = "grey75", alpha = 0.65) +
  geom_line(data = other_countries_avg,
            aes(x = Year, y = mean),
            size = 1, colour = "grey75") +
  geom_line(data = filter(top_countries, Entity == "China"),
            aes(x = Year, y = catch/1e+06, group = Entity),
            size = 1, colour = "#DC343B") +
  geom_text(data = filter(top_countries, Year == 2018 & Entity == "China"),
            aes(x = 2018, y = catch/1e+06),
            label = "PRC",
            colour = "#DC343B", fontface = "bold", hjust = -0.25) +
  geom_text(data = filter(top_countries, Year == 2018 & Entity != "China"),
            aes(x = 2018, y = mean(range(catch)/1e+06), label = "OTHER"),
            colour = "grey75", hjust = -0.25) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = seq(1960, 2020, 10),
                     labels = paste0("'", c(seq(60, 90, 10), "00", 10, 20))) +
  scale_y_log10() +
  labs(y = "Fisheries production (log<sub>10</sub> Tg)",
       title = "**PRC has the highest fishing yield of any country**",
       subtitle = "Countries with 10 highest fishing yields (as of 2018)",
       caption = "Data: Our World in Data") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(colour = "grey25", size = 12),
        axis.title.y = element_markdown(colour = "grey25", size = 13,
                                    margin = margin(r = 7)),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 35),
        plot.title = element_markdown(hjust = 0, colour = "grey25"),
        plot.subtitle = element_markdown(hjust = 0, colour = "grey25", margin = margin(b = 15)),
        plot.caption = element_text(hjust = 0))

ggsave(here("./plots/attention_eg9.png"), height = 16, width = 22, units = "cm")
