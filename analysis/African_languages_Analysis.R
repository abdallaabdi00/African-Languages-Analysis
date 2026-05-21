library(tidyverse)
library(patchwork)

data <- read_csv("data/africa_languages.csv", locale = locale(encoding = "UTF-8"))
glimpse(data)

# Define a custom theme — run this once at the top of your script
theme_african <- theme_minimal() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.caption  = element_text(size = 8, hjust = 1, color = "grey50"),
    axis.title    = element_text(size = 10),
    axis.text     = element_text(size = 9),
    panel.grid.minor = element_blank(),  # remove minor gridlines
    plot.margin   = margin(15, 15, 15, 15)  # padding around the plot
  )

# Data Cleaning Phase -----------------------------------------------------


# Check selected language names after loading the dataset.
data %>% 
  filter(language %in% c("Buru-Angwe", "Bomboli-Bozaba")) %>% 
  select(language,family,country)

data %>% 
  count(family,sort = TRUE)

#Lets find duplicates from the original data

data %>% 
  group_by(language,country) %>% 
  filter(n()>1) %>% 
  arrange(language,country) %>% 
  select(language,family,country,native_speakers)

#Assigning the correct Family for each language
data_clean<- data %>% 
  mutate(family=case_when(
    language=="Shabo" ~ "Nilo-Saharan",
    language=="Bozo" ~ "Niger-Congo",
    language=="Kituba" ~ "Kongo-Creole/Pidgin",
    language=="Nubi" ~ "Arabic-Creole/Pidgin",
    language=="Juba Arabic" ~ "Arabic-Creole/Pidgin",
    language=="Krio" ~ "English-Creole/Pidgin",
    language=="Pichinglis" ~ "English-Creole/Pidgin",
    language=="Mauritian Creole" ~ "French-Creole/Pidgin",
    language=="Seychellois Creole" ~ "French-Creole/Pidgin",
    language=="Cape Verdean Creole" ~ "Portuguese-Creole/Pidgin",
    family=="Afro-Asiatic" ~ "Afroasiatic",
    TRUE ~ family
  )) %>% 
  distinct(language, country, .keep_all = TRUE) #removing duplicated rows
glimpse(data_clean)

#clean family distribution
data_clean %>% 
  count(family,sort = TRUE)

#Confirm row count — should be 760 (796 minus 36 duplicate)
nrow(data)
nrow(data_clean)

# Save so you don't have to redo cleaning every session
write_csv(data_clean, "data/africa_languages_clean.csv")



# Exploratory Data Analysis -----------------------------------------------

#How many unique languages and countries do we have?

data_clean %>% 
  summarise(
    total_languages=n_distinct(language),
    total_countries=n_distinct(country),
    total_families=n_distinct(family)
  )


#1. Which country in Africa has the largest number of spoken languages --------

data_clean %>% 
  group_by(country) %>% 
  summarise(num_languages=n_distinct(language)) %>% 
  arrange(desc(num_languages)) %>% 
  slice_head(n=10)#top 10 countries



data_clean %>%
  group_by(country) %>%
  summarise(num_languages = n_distinct(language)) %>%
  arrange(desc(num_languages)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, num_languages), y = num_languages)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 African Countries by Number of Languages",
    x = "Country",
    y = "Number of Languages",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african


# Which family of languages has the highest density of speakers? ----------

data_clean %>%
  group_by(family) %>%
  summarise(
    total_speakers  = sum(native_speakers),
    median_speakers = median(native_speakers),
    num_languages   = n_distinct(language)
  ) %>%
  arrange(desc(total_speakers))

# 1. Graph for Total speakers by family
data_clean %>%
  group_by(family) %>%
  summarise(total_speakers = sum(native_speakers)) %>%
  arrange(desc(total_speakers)) %>%
  ggplot(aes(x = reorder(family, total_speakers), y = total_speakers / 1e6)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Native Speakers by Language Family",
    x = "Language Family",
    y = "Total Speakers (Millions)",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african

# 2. Median speakers by family — fairer comparison
data_clean %>%
  group_by(family) %>%
  summarise(median_speakers = median(native_speakers)) %>%
  arrange(desc(median_speakers)) %>%
  ggplot(aes(x = reorder(family, median_speakers), y = median_speakers / 1e6)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Median Native Speakers by Language Family",
    subtitle = "Median is more representative than mean for skewed data",
    x = "Language Family",
    y = "Median Speakers (Millions)",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african


# 3. Number of languages per family
data_clean %>%
  group_by(family) %>%
  summarise(num_languages = n_distinct(language)) %>%
  arrange(desc(num_languages)) %>%
  ggplot(aes(x = reorder(family, num_languages), y = num_languages)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Number of Languages per Family",
    x = "Language Family",
    y = "Number of Languages",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african



# Are there any languages that cut across multiple countries? -------------

data_clean %>%
  group_by(language) %>%
  summarise(
    num_countries = n_distinct(country),
    countries = paste(sort(unique(country)), collapse = ", "),
    family = first(family)
  ) %>%
  filter(num_countries > 1) %>%
  arrange(desc(num_countries))




data_clean %>%
  group_by(language) %>%
  summarise(
    num_countries = n_distinct(country),
    family = first(family)
  ) %>%
  filter(num_countries > 1) %>%
  arrange(desc(num_countries)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(language, num_countries), 
             y = num_countries,
             fill = family)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Widespread African Languages",
    subtitle = "Languages spoken in more than one country",
    x = "Language",
    y = "Number of Countries",
    fill = "Family",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african




#languages spoken in only 1 country
data_clean %>%
  group_by(language) %>%
  summarise(num_countries = n_distinct(country)) %>%
  count(num_countries) %>%
  arrange(desc(num_countries))


data_clean %>%
  group_by(language) %>%
  summarise(num_countries = n_distinct(country)) %>%
  count(num_countries) %>%
  ggplot(aes(x = factor(num_countries), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  labs(
    title = "How Many Countries Do African Languages Span?",
    subtitle = "Most languages are endemic to a single country",
    x = "Number of Countries",
    y = "Number of Languages",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african

data_clean %>%
  ggplot(aes(x = reorder(family, native_speakers, median), 
             y = native_speakers,
             fill = family)) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Distribution of Native Speakers by Language Family",
    subtitle = "Log scale — showing spread within each family",
    x = "Language Family",
    y = "Native Speakers (log scale)",
    caption = "Source: African Languages Dataset"
  ) +
  theme_african


# African-inspired colour palette
africa_colours <- c(
  "Niger-Congo"              = "#E07B39",  # warm orange
  "Nilo-Saharan"             = "#4A90A4",  # steel blue
  "Afroasiatic"              = "#C0392B",  # deep red
  "Indo-European"            = "#7D3C98",  # purple
  "Khoe-Kwadi"               = "#27AE60",  # green
  "Kxa"                     = "#2ECC71",  # light green
  "Ubangian"                 = "#F39C12",  # amber
  "Tuu"                      = "#1ABC9C",  # teal
  "Austronesian"             = "#3498DB",  # blue
  "Kongo-Creole/Pidgin"      = "#E74C3C",  # red
  "Arabic-Creole/Pidgin"     = "#D4AC0D",  # gold
  "English-Creole/Pidgin"    = "#85929E",  # grey
  "French-Creole/Pidgin"     = "#5D6D7E",  # dark grey
  "Portuguese-Creole/Pidgin" = "#A04000"   # brown
)


p1 <- data_clean %>%
  group_by(country) %>%
  summarise(num_languages = n_distinct(language)) %>%
  arrange(desc(num_languages)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(country, num_languages), y = num_languages)) +
  geom_col(fill = "#E07B39", width = 0.7) +
  geom_text(aes(label = num_languages), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 110)) +  # make room for labels
  labs(
    title    = "Top 10 African Countries by Number of Languages",
    subtitle = "Cameroon leads with extraordinary linguistic diversity",
    x        = NULL,  # no axis title needed — countries are self-explanatory
    y        = "Number of Languages",
    caption  = "Source: African Languages Dataset"
  ) +
  theme_african

p1




p2 <- data_clean %>%
  group_by(language) %>%
  summarise(
    num_countries = n_distinct(country),
    family = first(family)
  ) %>%
  filter(num_countries > 1) %>%
  arrange(desc(num_countries)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(language, num_countries), 
             y = num_countries,
             fill = family)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = num_countries), 
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = africa_colours) +
  scale_y_continuous(limits = c(0, 14)) +
  labs(
    title    = "Most Widespread African Languages",
    subtitle = "Top 15 languages by number of countries spanned",
    x        = NULL,
    y        = "Number of Countries",
    fill     = "Language Family",
    caption  = "Source: African Languages Dataset"
  ) +
  theme_african +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

p2


p3 <- data_clean %>%
  ggplot(aes(x = reorder(family, native_speakers, median), 
             y = native_speakers,
             fill = family)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = africa_colours) +
  coord_flip() +
  labs(
    title    = "Native Speaker Distribution by Language Family",
    subtitle = "Log scale — showing spread and outliers within each family",
    x        = NULL,
    y        = "Native Speakers (log scale)",
    caption  = "Source: African Languages Dataset"
  ) +
  theme_african

p3


# Combine all three charts into one figure
combined_plot <- p1 / p2 / p3 +
  plot_annotation(
    title    = "African Languages — A Statistical Overview",
    subtitle = "Diversity, spread and speaker distribution across the continent",
    caption  = "Source: African Languages Dataset",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      plot.caption  = element_text(size = 8, hjust = 1, color = "grey50")
    )
  )

# Save it as a high resolution image
ggsave("figures/african_languages_overview.png", 
       combined_plot, 
       width = 12, height = 18, dpi = 300)









