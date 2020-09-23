# Pckgs ---------------------------------------------------------------------------------------
if (!require("pacman")) {
  install.packages("pacman")
}
library(pacman) # for loading packages
p_load(dplyr, ggplot2, ggrepel)


# Functions -----------------------------------------------------------------------------------
# source(here::here("R", "helpers.R"))
# source(here::here("R", "ggplot-theme.R"))

# Load Clean data -----------------------------------------------------------------------------
dat3 <- readRDS(file = "dat3.rds")

# PLOT INPUTS
# time labels
year_dat <- tibble(
  year = c(28, 30),
  label = c("Trump", "HireAmer")
)
# colors 10
qualitative <- c(
  "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
  "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"
)

# grouped data input(BY REGION w/ travel ban class)
greencard_dat2 <- dat3 %>%
  dplyr::group_by(SPECIAL_COUNTRY, YEAR_MONTH) %>%
  dplyr::summarise(
    n = n(),
    NCert = sum(CASE_STATUS == "Certified"),
    PercCert = sum(CASE_STATUS == "Certified") / n() * 100,
    NDeni = sum(CASE_STATUS == "Denied"),
    PercDeni = sum(CASE_STATUS == "Denied") / n() * 100,
    NExp = sum(CASE_STATUS == "Certified-Expired"),
    PercExp = sum(CASE_STATUS == "Certified-Expired") / n() * 100
  ) %>%
  dplyr::rename(region = SPECIAL_COUNTRY, year_month = YEAR_MONTH)

# check
skimr::n_unique(dat3$SPECIAL_COUNTRY) # 8
levels(dat3$SPECIAL_COUNTRY)
skimr::n_unique(dat3$YEAR_MONTH) # 54 --> 432 OK
levels(dat3$YEAR_MONTH) # NULL

# Must put in order dates
greencard_dat2$year_month <- as.factor(greencard_dat2$year_month)
levels(greencard_dat2$year_month) # NULL

greencard_dat2$year_month_n <- factor(greencard_dat2$year_month,
  levels = c(
    "2014-10", "2014-11", "2014-12",
    "2015-1", "2015-2", "2015-3",
    "2015-4", "2015-5", "2015-6", "2015-7", "2015-8", "2015-9", "2015-10",
    "2015-11", "2015-12",
    "2016-1", "2016-2", "2016-3", "2016-4",
    "2016-5", "2016-6", "2016-7", "2016-8", "2016-9", "2016-10",
    "2016-11", "2016-12",
    "2017-1", "2017-2", "2017-3", "2017-4",
    "2017-5", "2017-6", "2017-7", "2017-8", "2017-9", "2017-10",
    "2017-11", "2017-12",
    "2018-1", "2018-2", "2018-3", "2018-4",
    "2018-5", "2018-6", "2018-7", "2018-8", "2018-9", "2018-10",
    "2018-11", "2018-12",
    "2019-1", "2019-2", "2019-3"
  ),
  labels = c(
    "1", "2", "3", # 2014
    "4", "5", "6",
    "7", "8", "9", "10",
    "11", "12", "13", "14", "15", # 2015
    "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25",
    "26", "27", # 2016
    "28", "29", "30", "31", # 2017
    "32", "33", "34", "35", "36", "37",
    "38", "39",
    "40", "41", "42", "43", # 2018
    "44", "45", "46", "47", "48", "49",
    "50", "51",
    "52", "53", "54"
  ) # 2019
)

dput(levels(greencard_dat2$year_month))
dput(levels(greencard_dat2$year_month_n))
skimr::n_unique(greencard_dat2$year_month_n)

# But I need it as N
greencard_dat2$year_month_n <- as.numeric(greencard_dat2$year_month_n)

# smaller set for the chart
gg_gc_static_region <- greencard_dat2 %>%
  group_by(year_month_n, region) %>%
  summarise(n = sum(n))

# (SOME) region labels
label_datR <- gg_gc_static_region %>%
  group_by(region) %>%
  summarize(n = max(n)) %>%
  select(-region) %>%
  inner_join(gg_gc_static_region) %>%
  # filter(!(year == 2018 & region == "Eastern Asia")) %>%
  # filter(!(year == 2004 & region == "Eastern Asia")) %>%
  mutate(year_month_n = ifelse(year_month_n == "EastAsia", 44, year_month_n)) %>%
  mutate(year_month_n = ifelse(year_month_n == "Europe & CentrAsia", 34, year_month_n)) %>%
  mutate(year_month_n = ifelse(year_month_n == "LatAm & Caribb", 43, year_month_n))



# 1/2 LINE PLOT BY REGION ---------------------------------------------------------------------
# REGIONAL
gg_static_region <- gg_gc_static_region %>%
  ggplot(aes(year_month_n, n)) +
  geom_vline(data = year_dat, aes(xintercept = year), alpha = 0.15) +
  geom_label(
    data = year_dat, aes(x = year, y = c(10000, 7000), label = label),
    angle = 0, color = "black"
  ) +
  geom_line(aes(linetype = region, color = region)) +
  # geom_area() +
  theme_minimal() +
  # ggrepel::geom_label_repel(data = label_datR, aes(label = continent, color = continent), show.legend = F) +
  # geom_point(data = label_datR, aes(color = continent)) +
  scale_color_manual("Region", values = qualitative) +
  scale_linetype("Region") +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11, face = NULL),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    legend.key.width = unit(3, "line"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 1:54, labels = c(
    "2014-10", "2014-11", "2014-12",
    "2015-1", "2015-2", "2015-3", "2015-4", "2015-5", "2015-6",
    "2015-7", "2015-8", "2015-9", "2015-10", "2015-11", "2015-12",
    "2016-1", "2016-2", "2016-3", "2016-4", "2016-5", "2016-6",
    "2016-7", "2016-8", "2016-9", "2016-10", "2016-11", "2016-12",
    "2017-1", "2017-2", "2017-3", "2017-4", "2017-5", "2017-6",
    "2017-7", "2017-8", "2017-9", "2017-10", "2017-11",
    "2017-12",
    "2018-1", "2018-2", "2018-3", "2018-4", "2018-5", "2018-6",
    "2018-7", "2018-8", "2018-9",
    "2018-10", "2018-11", "2018-12",
    "2019-1", "2019-2", "2019-3"
  )) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  labs(
    x = "", y = "Number of Employment-Based Green Cards\n",
    title = "Employment Certifications for Green Card Holders in the USA by Year \n(Oct 2014 - Mar 2019)",
    subtitle = "Total Certifications Processed in Timerange: 472672\n",
    caption = "Data: OFFICE OF FOREIGN LABOR CERTIFICATION"
  )

#gg_static_region


# 2/2 area plot BY REGION ---------------------------------------------------------------------


# area plot -----------------------------------------------------------------------------------
gg_static_region_a <- gg_gc_static_region %>%
  ggplot(aes(year_month_n, n)) +
  geom_vline(data = year_dat, aes(xintercept = year + 1), alpha = 0.35) +
  geom_label(
    data = year_dat, aes(x = year, y = c(19000, 15000), label = label),
    angle = 0, color = "black"
  ) +

  geom_area(aes(fill = region), alpha = 0.9) +

  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual("Region", values = qualitative) +
  # same (my lables )
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11, face = NULL),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    legend.key.width = unit(3, "line"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 1:54, labels = c(
    "2014-10", "2014-11", "2014-12",
    "2015-1", "2015-2", "2015-3", "2015-4", "2015-5", "2015-6",
    "2015-7", "2015-8", "2015-9", "2015-10", "2015-11", "2015-12",
    "2016-1", "2016-2", "2016-3", "2016-4", "2016-5", "2016-6",
    "2016-7", "2016-8", "2016-9", "2016-10", "2016-11", "2016-12",
    "2017-1", "2017-2", "2017-3", "2017-4", "2017-5", "2017-6",
    "2017-7", "2017-8", "2017-9", "2017-10", "2017-11",
    "2017-12",
    "2018-1", "2018-2", "2018-3", "2018-4", "2018-5", "2018-6",
    "2018-7", "2018-8", "2018-9",
    "2018-10", "2018-11", "2018-12",
    "2019-1", "2019-2", "2019-3"
  )) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  labs(
    x = "", y = "Number of Employment-Based Green Cards\n",
    title = "Employment Certifications Processed (prospective Green Card) in the USA \n(Oct 2014 - Mar 2019)",
    subtitle = "Total Certifications Processed in Timerange: 472,672\n",
    caption = "Source: OFFICE OF FOREIGN LABOR CERTIFICATION \n Travel Ban Countries = Venezuela, Syria, North Korea, Yemen, Libya, Iran, Somalia"
  )

#gg_static_region_a

ggsave(gg_static_region_a, filename = "gg_static_region_a.png", height = 7, width = 13)


# Check the % Denied --------------------------------------------------------------------------

# the set I am going to use for the chart
gg_denied_region_data <- greencard_dat2 %>%
  group_by(year_month_n, region) %>%
  summarise(n = sum(NDeni))

# region labels
label_datRD <- gg_denied_region_data %>%
  group_by(region) %>%
  summarize(n = max(n)) %>%
  select(-region) %>%
  inner_join(gg_denied_region_data) %>%
  filter(!(region == "EastAsia" & year_month_n == 39)) %>%
  filter(!(region == "Europe & CentrAsia")) %>%
  filter(!(region == "NorthAmerica")) %>%
  filter(!(region == "Sub-Sahar Afr")) %>%
  filter(!(region == "MiddleEast & NorthAfr")) %>%
  filter(!(region == "LatAm & Caribb")) %>%
  # filter (!(region == "LatAm & Caribb" & year_month_n == 5 ) ) %>%
  # filter (!(region == "LatAm & Caribb" & year_month_n == 35 ) )%>%
  # filter (!(region == "LatAm & Caribb" & year_month_n == 44 ) )%>%
  # filter (!(region == "LatAm & Caribb" & year_month_n == 51 )) %>%
  filter(!(region == "TravelBan" & year_month_n == 50)) %>%
  filter(!(region == "TravelBan" & year_month_n == 40)) %>%
  filter(!(region == "TravelBan" & year_month_n == 45)) %>%
  filter(!(region == "TravelBan" & year_month_n == 49)) %>%
  filter(!(region == "TravelBan" & year_month_n == 1)) # %>%



# REGIONAL graph % DENIED
gg_denied_region <- gg_denied_region_data %>%
  ggplot(aes(year_month_n, n)) +
  geom_vline(data = year_dat, aes(xintercept = year), alpha = 0.15) +
  geom_label(
    data = year_dat, aes(x = year, y = c(500, 400), label = label),
    angle = 0, color = "black"
  ) +
  geom_line(aes(linetype = region, color = region)) +
  # geom_area() +
  theme_minimal() +
  # pun name in max
  ggrepel::geom_label_repel(data = label_datRD, aes(label = region, color = region), show.legend = F) +
  geom_point(data = label_datRD, aes(color = region)) +
  scale_color_manual("Region", values = qualitative) +
  scale_linetype("Region") +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11, face = NULL),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    legend.key.width = unit(3, "line"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 1:54, labels = c(
    "2014-10", "2014-11", "2014-12",
    "2015-1", "2015-2", "2015-3", "2015-4", "2015-5", "2015-6",
    "2015-7", "2015-8", "2015-9", "2015-10", "2015-11", "2015-12",
    "2016-1", "2016-2", "2016-3", "2016-4", "2016-5", "2016-6",
    "2016-7", "2016-8", "2016-9", "2016-10", "2016-11", "2016-12",
    "2017-1", "2017-2", "2017-3", "2017-4", "2017-5", "2017-6",
    "2017-7", "2017-8", "2017-9", "2017-10", "2017-11",
    "2017-12",
    "2018-1", "2018-2", "2018-3", "2018-4", "2018-5", "2018-6",
    "2018-7", "2018-8", "2018-9",
    "2018-10", "2018-11", "2018-12",
    "2019-1", "2019-2", "2019-3"
  )) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  labs(
    x = "", y = "Number of Employment-Based Green Cards\n",
    title = "Percent of Denied Employment Certifications (Green Card Holders) by region \n(Oct 2014 - Mar 2019)",
    subtitle = "Total Certifications Processed in Timerange: 472672\n",
    caption = "Data: OFFICE OF FOREIGN LABOR CERTIFICATION"
  )

# gg_denied_region

ggsave(gg_denied_region, filename = "gg_denied_region.png", height = 7, width = 13)


# Top 15 countries ----------------------------------------------------------------------------
# new inputs ----------------------------------------------------------------------------------

# grouped data input(BY REGION w/ travel ban class)
top15data <- dat3 %>%
  dplyr::group_by(COUNTRY_OF_BIRTH, YEAR_MONTH) %>%
  dplyr::summarise(
    n = n(),
    NCert = sum(CASE_STATUS == "Certified"),
    PercCert = sum(CASE_STATUS == "Certified") / n() * 100,
    NDeni = sum(CASE_STATUS == "Denied"),
    PercDeni = sum(CASE_STATUS == "Denied") / n() * 100,
    NExp = sum(CASE_STATUS == "Certified-Expired"),
    PercExp = sum(CASE_STATUS == "Certified-Expired") / n() * 100
  ) %>%
  dplyr::rename(country = COUNTRY_OF_BIRTH, year_month = YEAR_MONTH) # 7324 combns

# top 10 vector
dat3 %>%
  group_by(country = COUNTRY_OF_BIRTH) %>% dplyr::summarise(n = n()) %>%  arrange(desc(n)) %>% .[1:15,] %>% .$country -> top15

# Must put in order dates
top15data$year_month <- as.factor(top15data$year_month)
levels(top15data$year_month) # NULL

top15data$year_month_n <- factor(top15data$year_month,
                                      levels = c(
                                        "2014-10", "2014-11", "2014-12",
                                        "2015-1", "2015-2", "2015-3",
                                        "2015-4", "2015-5", "2015-6", "2015-7", "2015-8", "2015-9", "2015-10",
                                        "2015-11", "2015-12",
                                        "2016-1", "2016-2", "2016-3", "2016-4",
                                        "2016-5", "2016-6", "2016-7", "2016-8", "2016-9", "2016-10",
                                        "2016-11", "2016-12",
                                        "2017-1", "2017-2", "2017-3", "2017-4",
                                        "2017-5", "2017-6", "2017-7", "2017-8", "2017-9", "2017-10",
                                        "2017-11", "2017-12",
                                        "2018-1", "2018-2", "2018-3", "2018-4",
                                        "2018-5", "2018-6", "2018-7", "2018-8", "2018-9", "2018-10",
                                        "2018-11", "2018-12",
                                        "2019-1", "2019-2", "2019-3"
                                      ),
                                      labels = c(
                                        "1", "2", "3", # 2014
                                        "4", "5", "6",
                                        "7", "8", "9", "10",
                                        "11", "12", "13", "14", "15", # 2015
                                        "16", "17", "18", "19",
                                        "20", "21", "22", "23", "24", "25",
                                        "26", "27", # 2016
                                        "28", "29", "30", "31", # 2017
                                        "32", "33", "34", "35", "36", "37",
                                        "38", "39",
                                        "40", "41", "42", "43", # 2018
                                        "44", "45", "46", "47", "48", "49",
                                        "50", "51",
                                        "52", "53", "54"
                                      ) # 2019
)

dput(levels(top15data$year_month))
dput(levels(top15data$year_month_n))
skimr::n_unique(top15data$year_month_n)

# But I need it as N
top15data$year_month_n <- as.numeric(top15data$year_month_n)

# smaller set for the chart
top15_use <- top15data %>%
  filter(country %in% top15)

# colors 15
qualitative2 <- c(
  "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#f0f0f0", # #d9d9d9",  #fb9a99",
  "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
  "#ffff99",  "#b15928", "#228B22", "#CD2626", "#1874CD", "#008B00", "#FF6EB4"
)


# area plot -----------------------------------------------------------------------------------
gg_top15country <- top15_use %>%
  filter(country != "INDIA" & country != "CHINA") %>%
  ggplot(aes(year_month_n, n)) +
  geom_area(aes(fill = country), alpha = 0.9) +
  geom_vline(data = year_dat, aes(xintercept = year + 1), alpha = 0.35) +
  geom_label(data = year_dat, aes(x = year, y = c( 4000, 3000), label = label),
    angle = 0, color = "black") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual("Country", values = qualitative2) +
  # same (my lables )
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    # plot.subtitle = element_text(size = 11, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    legend.key.width = unit(3, "line"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 1:54, labels = c(
    "2014-10", "2014-11", "2014-12",
    "2015-1", "2015-2", "2015-3", "2015-4", "2015-5", "2015-6",
    "2015-7", "2015-8", "2015-9", "2015-10", "2015-11", "2015-12",
    "2016-1", "2016-2", "2016-3", "2016-4", "2016-5", "2016-6",
    "2016-7", "2016-8", "2016-9", "2016-10", "2016-11", "2016-12",
    "2017-1", "2017-2", "2017-3", "2017-4", "2017-5", "2017-6",
    "2017-7", "2017-8", "2017-9", "2017-10", "2017-11",
    "2017-12",
    "2018-1", "2018-2", "2018-3", "2018-4", "2018-5", "2018-6",
    "2018-7", "2018-8", "2018-9",
    "2018-10", "2018-11", "2018-12",
    "2019-1", "2019-2", "2019-3"
  )) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  labs(
    x = "", y = "",
    title = "Employment Certifications Processed (prospective Green Card) in the USA \n TOP countries WITHOUT INDIA & CHINA (Oct 2014 - Mar 2019)",
    subtitle = "",
    caption = "Source: OFFICE OF FOREIGN LABOR CERTIFICATION"
  )

#gg_top15country

ggsave(gg_top15country, filename = "gg_top15country.png", height = 7, width = 13)




# India & CHINA ---------------------------------------------------------------------------------
gg_top2 <- top15_use %>%
  filter(country == "INDIA" | country == "CHINA") %>%
  ggplot(aes(year_month_n, n)) +
  geom_area(aes(fill = country), alpha = 0.9) +
  geom_vline(data = year_dat, aes(xintercept = year + 1), alpha = 0.35) +
  geom_label(data = year_dat, aes(x = year, y = c( 12000, 10000), label = label),
             angle = 0, color = "black") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual("Country", values = qualitative2) +
  # same (my lables )
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 11, face = NULL),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    legend.key.width = unit(3, "line"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 1:54, labels = c(
    "2014-10", "2014-11", "2014-12",
    "2015-1", "2015-2", "2015-3", "2015-4", "2015-5", "2015-6",
    "2015-7", "2015-8", "2015-9", "2015-10", "2015-11", "2015-12",
    "2016-1", "2016-2", "2016-3", "2016-4", "2016-5", "2016-6",
    "2016-7", "2016-8", "2016-9", "2016-10", "2016-11", "2016-12",
    "2017-1", "2017-2", "2017-3", "2017-4", "2017-5", "2017-6",
    "2017-7", "2017-8", "2017-9", "2017-10", "2017-11",
    "2017-12",
    "2018-1", "2018-2", "2018-3", "2018-4", "2018-5", "2018-6",
    "2018-7", "2018-8", "2018-9",
    "2018-10", "2018-11", "2018-12",
    "2019-1", "2019-2", "2019-3"
  )) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.5)) +
  labs(
    x = "", y = "",
    title = "Employment Certifications Processed (prospective Green Card) in the USA \n INDIA & CHINA (Oct 2014 - Mar 2019)",
    subtitle = "",
    caption = "Source: OFFICE OF FOREIGN LABOR CERTIFICATION"
  )

#gg_top2

ggsave(gg_top2, filename = "gg_top2.png", height = 7, width = 13)


# gridExtra::grid.arrange(gg_top15country, gg_top2, nrow = 2)

