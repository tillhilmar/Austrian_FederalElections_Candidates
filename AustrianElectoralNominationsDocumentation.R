library(readxl)
library(dplyr)
library(stringr)
library(quanteda)
library(quanteda.textstats)
library(stringi)
library(quanteda.textplots)
library(ggplot2)
library(ggwordcloud)
library(knitr)
library(kableExtra)
library(tidyr)
library(sf)
library(tidygeocoder)
library(rnaturalearth)
library(rnaturalearthdata)

## Cleaning and Import of Lists Not Shown

## 2024 Elections for Analysis of Occupations ----
## Combine Bundes_df and LandesRegional_df
ALL2024 <- bind_rows(Bundes_df, LandesRegional_df)

saveRDS(ALL2024, "ALL2024.rds")

# Check for duplicates based on "Familienname", "Vorname", and "Geb.Jahr"
duplicates <- ALL2024 %>%
  group_by(Familienname, Vorname, Geb.Jahr) %>%
  filter(n() > 1) %>%
  arrange(Familienname, Vorname, Geb.Jahr)

# Remove duplicates
ALL2024_clean <- ALL2024 %>%
  distinct(Familienname, Vorname, Geb.Jahr, .keep_all = TRUE)

table(ALL2024_clean$Partei)
party_counts <- as.data.frame(table(ALL2024_clean$Partei)) %>%
  as_tibble() %>%
  rename(Partei = Var1, Count = Freq)

ALL2024_clean <- ALL2024_clean %>%
  mutate(Geb.Jahr = as.numeric(Geb.Jahr))
# Calculate the mean birth year for each party
mean_birth_years <- ALL2024_clean %>%
  group_by(Partei) %>%
  summarise(mean_Geb_Jahr = mean(Geb.Jahr, na.rm = TRUE))

saveRDS(ALL2024_clean, "ALL2024_clean.rds")

# Beruf by Partei, NRW 2024

# Count frequencies of each "Beruf" by "Partei"
beruf_by_partei <- ALL2024_clean %>%
  group_by(Partei, Beruf) %>%
  summarise(Frequency = n()) %>%
  arrange(Partei, desc(Frequency))  # Sort by Partei and frequency

# Quanteda
# Standardize the "Partei" variable
ALL2024_clean <- ALL2024_clean %>%
  mutate(
    # Convert to lowercase and remove leading/trailing spaces
    Partei = tolower(trimws(Partei)),
    # Replace special characters (e.g., "ö" -> "o", "ü" -> "u")
    Partei = stri_trans_general(Partei, "Latin-ASCII"),
    # Remove extra spaces between words
    Partei = gsub("\\s+", " ", Partei),
    # Replace known variations with standardized names
    Partei = case_when(
      grepl("gruene|grune", Partei) ~ "grune",
      grepl("fpoe|fpo", Partei) ~ "fpo",
      grepl("spoe|spo", Partei) ~ "spo",
      grepl("neoes|neos", Partei) ~ "neos",
      grepl("oevp|ovp", Partei) ~ "ovp",
      TRUE ~ Partei
    )
  )

ALL2024_mod <- ALL2024_clean %>%
  mutate(
    # Convert to lowercase and trim whitespace to standardize
    Partei = tolower(trimws(Partei)),
    Partei = stri_trans_general(Partei, "Latin-ASCII"), # Convert umlauts to ASCII equivalents
    # Remove whitespaces that follow a comma
    Beruf = gsub(",\\s+", ",", Beruf),
    # Replace all remaining spaces with underscores
    Beruf = gsub("\\s+", "_", Beruf),
    # Replace hyphens with underscores and remove any trailing whitespace
    Beruf = gsub("-\\s*", "_", Beruf),
    # Split words separated by commas into separate words
    Beruf = gsub(",", " ", Beruf),
    # Remove periods from "Beruf"
    Beruf = gsub("\\.", "", Beruf),
    # Create a unique doc_id
    doc_id = paste0(row_number())
  )

# Convert the data to a corpus with the unique document ID
corpus <- corpus(ALL2024_mod, text_field = "Beruf", docid_field = "doc_id")

# Tokenize the corpus
tokens_beruf <- tokens(corpus, remove_punct = TRUE)

# Create a document-feature matrix without grouping
dfm_beruf <- dfm(tokens_beruf)

# Initialize an empty dataframe to store results
top_beruf_by_partei <- data.frame()

# Loop through each unique party to get top words for each
for (party in unique(ALL2024_mod$Partei)) {
  # Filter DFM for the current party
  dfm_party <- dfm_subset(dfm_beruf, docvars(dfm_beruf, "Partei") == party)
  
  # Get top 10 words for the current party
  top_terms <- textstat_frequency(dfm_party, n = 10)
  
  # Add "Partei" column to the result
  top_terms$Partei <- party
  
  # Append to the final result dataframe
  top_beruf_by_partei <- bind_rows(top_beruf_by_partei, top_terms)
}


# Define the parties to include
selected_parties <- c("spo", "ovp", "neos", "grune", "fpo")

# Subset the DFM to include only the specified parties
dfm_selected_parties <- dfm_subset(dfm_beruf, Partei %in% selected_parties)

# Group the DFM by "Partei" to aggregate counts by party
dfm_grouped <- dfm_group(dfm_selected_parties, groups = docvars(dfm_selected_parties, "Partei"))

## compute lexical diversity

lexdiv_results <- textstat_lexdiv(dfm_grouped, measure = c("TTR", "C"))
print(lexdiv_results)

##

# Calculate keyness with 'fpo' as the target and all others as reference
keyness_results_fpo <- textstat_keyness(dfm_grouped, target = "fpo")
textplot_keyness(keyness_results_fpo, margin = 0.3)

keyness_results_grune <- textstat_keyness(dfm_grouped, target = "grune")
textplot_keyness(keyness_results_grune, margin = 0.3)

keyness_results_spo <- textstat_keyness(dfm_grouped, target = "spo")
textplot_keyness(keyness_results_spo, margin = 0.3)

keyness_results_ovp <- textstat_keyness(dfm_grouped, target = "ovp")
textplot_keyness(keyness_results_ovp, margin = 0.3)

keyness_results_neos <- textstat_keyness(dfm_grouped, target = "neos")
textplot_keyness(keyness_results_neos, margin = 0.3)

# Subset the DFM for specific parties
dfm_selected <- dfm_subset(dfm_grouped, docvars(dfm_grouped, "Partei") %in% c("spo", "ovp", "neos", "grune", "fpo"))

# Calculate frequency data by party
set.seed(1)
freq_data <- textstat_frequency(dfm_selected, n = 20, groups = docvars(dfm_selected, "Partei")) %>%
  arrange(-frequency)

# Create a word cloud using ggwordcloud / no legend and labels
# same wordcloud but bold
ggplot(freq_data, aes(label = feature, size = frequency, colour = group)) +
  geom_text_wordcloud(fontface = "bold") +
  scale_size_area(max_size = 8) +
  scale_color_manual(values = c("spo" = "red", "ovp" = "lightblue", "neos" = "pink", "grune" = "darkgreen", "fpo" = "blue")) +
  theme_minimal() +
  guides(colour = guide_legend(title = "Party"))

# Create a dummy dataset for legend
size_legend_data <- data.frame(
  label = c(""),
  size = c(3, 6, 9)
)

# Create the main word cloud and add the frequency legend using size
ggplot(freq_data, aes(label = feature, size = frequency, colour = group)) +
  geom_text_wordcloud(fontface = "bold") +
  scale_size_area(max_size = 8) +
  scale_color_manual(values = c("spo" = "red", "ovp" = "lightblue", "neos" = "pink", "grune" = "darkgreen", "fpo" = "blue")) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Party",
      override.aes = list(
        colour = c("blue", "darkgreen", "pink", "lightblue", "red")
      )
    )
  ) +
  geom_text(data = size_legend_data, aes(label = label, size = size), 
            x = Inf, y = Inf, hjust = 1, vjust = 1, color = "black", show.legend = TRUE) +
  labs(title = "Häufigste Berufsbezeichnungen nach Partei bei der NRW 2024")


# Combining the data from 2017, 2019 and 2024 for the analysis of places -----

ALL2017_2019_2024_clean <- bind_rows(ALL2017_clean, ALL2019_clean, ALL2024_clean)

ALL2017_2019_2024_clean <- ALL2017_2019_2024_clean %>%
  mutate(Ort = str_trim(Ort, side = "both"))

table(ALL2017_2019_2024_clean$Partei)
table(ALL2017_2019_2024_clean$Ort)

# Words to keep lowercase
exceptions <- c("im", "am", "zum", "an", "der", "des", "und", "von", "zu", "in", "i.", "a.d.", "bei") 

# Function to apply title case while preserving exceptions
clean_ort <- function(x) {
  # Convert to lowercase and split into words
  words <- str_split(str_to_lower(x), "\\s+")[[1]]
  
  # Apply capitalization to words not in exceptions
  words <- ifelse(words %in% exceptions, words, str_to_title(words))
  
  # Rejoin words into a single string
  str_c(words, collapse = " ")
}

# Apply the function to the Ort variable
ALL2017_2019_2024_clean <- ALL2017_2019_2024_clean %>%
  mutate(Ort = sapply(Ort, clean_ort))

ALL2017_2019_2024_clean <- ALL2017_2019_2024_clean %>%
  mutate(Partei = ifelse(Partei %in% c("kpo", "kpoe"), "kpo", Partei))

ALL2017_2019_2024_subset <- ALL2017_2019_2024_clean %>%
  filter(Partei %in% c("fpo", "grune", "kpo", "neos", "ovp", "spo"))

table(ALL2017_2019_2024_subset$Partei)

# Summarize the counts by Partei
party_table <- ALL2017_2019_2024_subset %>%
  count(Partei, name = "Anzahl")

# Generate table
html_table <- kable(
  party_table,
  col.names = c("Partei", "Anzahl"),
  caption = "Anzahl der Kandidaturen nach Partei"
) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")
save_kable(html_table, "party_table.html")

# To find out how many individual people are in this data, identify duplicates/ note: duplicates were removed within the three years, but they are not removed across the three years 
duplicates_ALL2017_2019_2024_subset <- ALL2017_2019_2024_subset %>%
  filter(duplicated(select(., Familienname, Vorname, Geb.Jahr)))
# Remove duplicates for working with the individual people 
ALL2017_2019_2024_no_duplicates <- ALL2017_2019_2024_subset %>%
  distinct(Familienname, Vorname, Geb.Jahr, .keep_all = TRUE)
table(ALL2017_2019_2024_no_duplicates$Partei)

#calculate the share of those who live outside of the nine largest cities
ALL2017_2019_2024_no_duplicates

# Step 1: Define the list of the 10 largest cities in Austria
top_cities <- c("Wien", "Graz", "Linz", "Salzburg", "Innsbruck", "Klagenfurt", "Villach", "Wels", "St. Pölten", "Dornbirn")

# Step 2: Add a column indicating whether a politician lives in a top city
ALL2017_2019_2024_no_duplicates_1 <- ALL2017_2019_2024_no_duplicates %>%
  mutate(top_city = ifelse(Ort %in% top_cities, "Top 10 City", "Other"))

# Step 3: Calculate the share for each Partei
shares_of_cities <- ALL2017_2019_2024_no_duplicates_1 %>%
  group_by(Partei, top_city) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Partei) %>%
  mutate(share = count / sum(count) * 100)

# Step 4: Create a kable table
html_table2 <- shares_of_cities %>%
  mutate(share = sprintf("%.1f %%", share)) %>% 
  select(Partei, top_city, count, share) %>%
  pivot_wider(names_from = top_city, values_from = c(count, share)) %>% 
  kable(
    format = "html",
    col.names = c("Partei", "Anzahl außerhalb großer Städte", "Anzahl innerhalb", "Prozent außerhalb (%)", "Prozent innerhalb (%)"),
    caption = "Anteile der Politiker:innen in großen Städten vs. außerhalb"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Save the table as an HTML file
save_kable(html_table2, "AnzahlStadt_table.html")

top_ort_frequencies <- ALL2017_2019_2024_subset %>%
  group_by(Ort) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(20, count) 

# Create a contingency table (cross-tabulation) for "Partei" and "Ort"
contingency_table <- table(ALL2017_2019_2024_subset$Partei, ALL2017_2019_2024_subset$Ort)

# Convert the contingency table to a dataframe for easier manipulation
contingency_df <- as.data.frame(as.table(contingency_table))

# Rename the columns for clarity
colnames(contingency_df) <- c("Partei", "Ort", "Frequency")

# VISUALIZATION

# Step 1: Load Austria map data
austria_map <- ne_states(country = "Austria", returnclass = "sf")

# Step 2: Prepare the data for geocoding
geocoded_data <- contingency_df %>%
  distinct(Ort) %>%  # Keep unique locations
  mutate(Ort = as.character(Ort)) %>%
  filter(!is.na(Ort) & Ort != "") %>%  # Remove NA and empty values
  mutate(Ort_with_country = paste(Ort, ", Austria"))  # Append ", Austria" to each place name

# Step 3: Geocode the locations
geocoded_data <- geocoded_data %>%
  geocode(Ort_with_country, method = "osm", lat = latitude, long = longitude)

# Drop mismatches
contingency_df_clean <- contingency_df %>%
  filter(Ort %in% geocoded_data$Ort)

unique_places_by_party <- contingency_df_clean %>%
  group_by(Partei) %>%
  summarise(unique_places = n_distinct(Ort)) %>%
  arrange(desc(unique_places))

# Step 4: Filter geocoded locations to retain only valid Austrian coordinates
# Use bounding box for Austria: longitude (9.5 to 17), latitude (46 to 49)
geocoded_data <- geocoded_data %>%
  filter(latitude >= 46 & latitude <= 49 & longitude >= 9.5 & longitude <= 17)

# Merge cleaned data with geocoded data
contingency_df_geo <- contingency_df_clean %>%
  left_join(geocoded_data, by = "Ort") %>%
  filter(!is.na(latitude) & !is.na(longitude))  # Remove rows with missing geocodes

# Filter out rows where Frequency is 0
contingency_df_geo_filtered <- contingency_df_geo %>%
  filter(Frequency > 0)

# this is applied to have the large dots move to the background
contingency_df_geo_filtered <- contingency_df_geo_filtered %>%
  arrange(desc(Frequency))

install.packages("ggiraph")
library(ggiraph)
library(ggplot2)
library(htmlwidgets)

# Create a ggplot object with interactive points
interactive_plot <- ggplot() +
  geom_sf(data = austria_map, fill = "white", color = "gray") +
  geom_point_interactive(
    data = contingency_df_geo_filtered, 
    aes(
      x = longitude, y = latitude, size = Frequency, color = Partei,
      tooltip = paste("Ort:", Ort, "<br>Frequency:", Frequency, "<br>Partei:", Partei)
    ),
    alpha = 0.5, position = position_jitter(width = 0.02, height = 0.02)
  ) +
  scale_size_continuous(
    name = "Anzahl",
    range = c(1, 10),  # Increased size range for better differentiation
    breaks = c(1, 2, 5, 10),  # Explicit breaks for clarity
    trans = "sqrt"  # Apply square root scaling for better visual contrast
  ) +
  scale_color_manual(values = c("fpo" = "blue", "grune" = "green", "kpo" = "darkred", "neos" = "pink", "ovp" = "black", "spo" = "red")) +
  coord_sf(xlim = c(9.5, 17), ylim = c(46, 49)) +
  labs(title = "Wohnorte der Kandidat:innen, Anzahl Kandidaturen der NRW 2017, 2019, und 2024") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.text.y = element_blank(),  # Remove y-axis text
    plot.title = element_text(size = 9, hjust = 0.5)  # Adjust title size and center it
  )

# Convert to interactive plot using ggiraph
girafe(ggobj = interactive_plot)

girafe(ggobj = interactive_plot) %>%
  htmlwidgets::saveWidget(file = "interactive_plot.html", selfcontained = TRUE)

htmlwidgets::saveWidget(girafe(ggobj = interactive_plot), "interactive_plot.js", selfcontained = TRUE)

## now, creating the plots for parties in comparison using a function

# Define a function to create an interactive plot
create_interactive_plot <- function(data, party_colors, file_name) {
  interactive_plot <- ggplot() +
    geom_sf(data = austria_map, fill = "white", color = "gray") +
    geom_point_interactive(
      data = data,
      aes(
        x = longitude, y = latitude, size = Frequency, color = Partei,
        tooltip = paste("Ort:", Ort, "<br>Frequency:", Frequency, "<br>Partei:", Partei)
      ),
      alpha = 0.4,
      position = position_jitter(width = 0.02, height = 0.02)
    ) +
    scale_size_continuous(
      name = "Anzahl",
      range = c(1, 10),  # Adjust size range
      breaks = c(1, 2, 5, 10),
      trans = "sqrt"
    ) +
    scale_color_manual(values = party_colors) +
    coord_sf(xlim = c(9.5, 17), ylim = c(46, 49)) +
    labs(title = "Wohnorte der Kandidat:innen, Anzahl Kandidaturen der NRW 2017, 2019, und 2024") +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 9, hjust = 0.5)
    )
  
  # Save as an HTML file
  girafe(ggobj = interactive_plot) %>%
    htmlwidgets::saveWidget(file = file_name, selfcontained = TRUE)
}

# Subset and create plots for each pair of parties
# Plot for fpo and grune
create_interactive_plot(
  data = contingency_df_geo_filtered %>% filter(Partei %in% c("fpo", "grune")),
  party_colors = c("fpo" = "blue", "grune" = "green"),
  file_name = "interactive_fpo_grune.html"
)

# Plot for ovp and spo
create_interactive_plot(
  data = contingency_df_geo_filtered %>% filter(Partei %in% c("spo", "ovp")),
  party_colors = c("spo" = "red", "ovp" = "black"),
  file_name = "interactive_ovp_spo.html"
)

# Plot for fpo and spo
create_interactive_plot(
  data = contingency_df_geo_filtered %>% filter(Partei %in% c("spo", "fpo")),
  party_colors = c("spo" = "red", "fpo" = "blue"),
  file_name = "interactive_fpo_spo.html"
)

# Plot for ovp and fpo
create_interactive_plot(
  data = contingency_df_geo_filtered %>% filter(Partei %in% c("ovp", "fpo")),
  party_colors = c("ovp" = "black", "fpo" = "blue"),
  file_name = "interactive_ovp_fpo.html"
)


# Using the geocoded (filtered) data as a basis for further insights
table(contingency_df_geo_filtered$Partei)
unique(contingency_df_geo_filtered$Ort)
sum(contingency_df_geo_filtered$Frequency)

# Rename "Frequency" to "Anzahl"
contingency_df_clean <- contingency_df_clean %>%
  rename(Anzahl = Frequency)

# Step 1: Calculate total Anzahl per Ort
total_counts <- contingency_df_clean %>%
  group_by(Ort) %>%
  summarise(total_count = sum(Anzahl), .groups = "drop")

# Step 2: Merge total counts with the original dataframe
contingency_df_with_percent <- contingency_df_clean %>%
  left_join(total_counts, by = "Ort") %>%  # Merge total counts
  mutate(percentage = (Anzahl / total_count) * 100)  # Calculate percentage share

# Step 3: Fill missing parties with Anzahl = 0 and percentage = 0
all_parties <- c("fpo", "grune", "kpo", "neos", "ovp", "spo")

contingency_df_full <- expand.grid(
  Ort = unique(contingency_df_with_percent$Ort),
  Partei = all_parties
) %>%
  left_join(
    contingency_df_with_percent %>%
      select(Ort, Partei, Anzahl, percentage),
    by = c("Ort", "Partei")
  ) %>%
  mutate(
    Anzahl = ifelse(is.na(Anzahl), 0, Anzahl),
    percentage = ifelse(is.na(percentage), 0, percentage)
  )

# identify "strongholds": at least 3, and percentage more than 70
fpoe_strongholds <- contingency_df_full %>%
  filter(Partei == "fpo", Anzahl > 3, percentage > 70) %>%
  arrange(desc(Anzahl))

fpoe_strongholds_table <- fpoe_strongholds %>%
  arrange(desc(Anzahl)) %>% 
  slice(1:15) %>% 
  select(Ort, Anzahl, percentage, Partei) %>% 
  mutate(percentage = sprintf("%.1f %%", percentage)) %>% 
  kable(
    format = "html",
    col.names = c("Ort", "Anzahl", "Prozent", "Partei"),
    caption = "Top FPÖ Strongholds"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria")

# Save the table as an HTML file
save_kable(fpoe_strongholds_table, "fpoe_strongholds_table.html")
