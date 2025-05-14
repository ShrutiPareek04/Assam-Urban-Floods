# Primary Data Analysis
# to assess the flood risk perception on-the-ground, primary data was collected from 70 respondents in Assam
# the historical experience, flood risk awareness & preparedness, trust in local & govt institutions are assessed.

# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(stringr)
library(tidygeocoder)
library(leaflet)

# load data
df = read_xlsx('Primary_data_edit.xlsx',sheet = 'Questionaire-edit')
str(df)
colnames(df)

# Age distribution
ggplot(df, aes(Age)) + geom_bar(fill = "steelblue") + theme_minimal() + ggtitle("Age Distribution")
# Gender distribution
ggplot(df, aes(Gender)) + geom_bar(fill = "coral") + theme_minimal() + ggtitle("Gender Distribution")
# Education level
ggplot(df, aes(Education)) + geom_bar(fill = "darkseagreen") + theme_minimal() + ggtitle("Education Levels")
# Income bracket
ggplot(df, aes(Income)) + geom_bar(fill = "goldenrod") + theme_minimal() + ggtitle("Household Income Brackets")

# interactive maps
df <- df %>%
  mutate(Location_clean = str_to_title(str_trim(Location))) %>%
  mutate(Location_clean = case_when(
    Location_clean %in% c("Christianbasti", "Christian Basti") ~ "Christian Basti",
    Location_clean %in% c("Zoo Road Tenali", "Zoo Tiniali") ~ "Zoo Tiniali",
    Location_clean %in% c("Kamrup Metro.Dispur,Jatia") ~ "Dispur Jatia",
    Location_clean %in% c("Seuji Path, R.G.B Road, Chandmari") ~ "Chandmari",
    TRUE ~ Location_clean
  ))

locations_unique <- df %>% distinct(Location_clean) %>% rename(address = Location_clean)
# geo_locations <- geocode(locations_unique, address = "address", method = "osm")

geo_locations = read_xlsx('Primary_data_edit.xlsx',sheet = 'geolocation')

df_counts <- df %>% count(Location_clean)

map_data <- geo_locations %>% rename(Location_clean = address) %>% left_join(df_counts, by = "Location_clean")

# leaflet(data = map_data) %>% addTiles() %>%
#   addCircleMarkers(lng = ~long, lat = ~lat, radius = ~sqrt(n) * 3, color = "blue", fillOpacity = 0.6, popup = ~paste0("<strong>", Location_clean, "</strong><br/>Respondents: ", n)) %>%
#   addLegend(position = "bottomright", colors = "blue", labels = "Respondent Count", title = "Survey Data: Assam")

# section 2 - Flood Risk Awareness & Threat perception
# Flood Risk Awareness & Threat Perception Index (FRATPI).
df <- df %>% mutate(q1_num = if_else(q1 == "Yes", 1, 0), q3_num = case_when(q3 == "Yes" ~ 5, q3 == "Maybe" ~ 3, q3 == "No" ~ 1, TRUE ~ NA_real_))
df <- df %>% mutate(FRATPI = rowMeans(select(., q1_num, q2, q3_num, q4, q5, q6), na.rm = TRUE))
df <- df %>% mutate(FRATPI_level = case_when(FRATPI <= 2.5 ~ "Low", FRATPI <= 3.5 ~ "Moderate", FRATPI > 3.5 ~ "High", TRUE ~ NA_character_))

ggplot(df, aes(x = Age, y = FRATPI)) + geom_boxplot(fill = "skyblue") + theme_minimal() + labs(title = "Flood Risk Awareness & Threat Perception by Age Group", x = "Age Group", y = "FRATPI Score")
ggplot(df, aes(x = Income, y = FRATPI)) + geom_boxplot(fill = "salmon") + theme_minimal() + labs(title = "Flood Risk Awareness & Threat Perception by Income Bracket", x = "Income Bracket", y = "FRATPI Score") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

fratpi_by_location <- df %>%
  group_by(Location_clean) %>%
  summarise(Avg_FRATPI = mean(FRATPI, na.rm = TRUE),
            Respondents = n())
map_data_fratpi <- geo_locations %>%
  rename(Location_clean = address) %>%
  left_join(fratpi_by_location, by = "Location_clean")

pal <- colorNumeric("YlOrRd", domain = map_data_fratpi$Avg_FRATPI)

# leaflet(data = map_data_fratpi) %>% addTiles() %>%
#   addCircleMarkers(lng = ~long, lat = ~lat, radius = ~sqrt(Respondents) * 3, color = ~pal(Avg_FRATPI), fillOpacity = 0.85, popup = ~paste0("<strong>", Location_clean, "</strong><br/>", "Avg. FRATPI: ", round(Avg_FRATPI, 2), "<br/>", "Respondents: ", Respondents)) %>%
#   addLegend("bottomright", pal = pal, values = ~Avg_FRATPI, title = "Avg. FRATPI", labFormat = labelFormat(digits = 2))

# section 2: Historical Flood Experience
df <- df %>% mutate(q7_num = if_else(q7 == "Yes", 1, 0))
df <- df %>% mutate(q9_num = case_when(q9 == "More than 3 times" ~ 4, q9 == "1-3 times" ~ 1.5, q9 == "Never" ~ 0, TRUE ~ NA_real_))
df <- df %>% mutate(q10_num = if_else(q10 == "Yes", 1, 0), q11_num = if_else(q11 == "Yes", 1, 0))

impact_scores <- df %>% group_by(Location_clean) %>% summarise(Affected_Score = mean(q9_num, na.rm = TRUE), Damage_Rate = mean(q10_num, na.rm = TRUE), Income_Loss_Rate = mean(q11_num, na.rm = TRUE))

map_data_impact <- geo_locations %>% rename(Location_clean = address) %>% left_join(impact_scores, by = "Location_clean")

pal <- colorNumeric(palette = "YlOrRd", domain = map_data_impact$Damage_Rate)
# leaflet(data = map_data_impact) %>% addTiles() %>%
#   addCircleMarkers(lng = ~long, lat = ~lat, radius = ~Damage_Rate*10, color = ~pal(Damage_Rate), fillOpacity = 0.85, label = ~paste0(Location_clean),  popup = ~paste0("<b>", Location_clean, "</b><br/>", "<b>Avg. Times Affected:</b> ", round(Affected_Score, 1), "<br/>", "<b>Damage Rate:</b> ", scales::percent(Damage_Rate, accuracy = 1), "<br/>", "<b>Income Loss Rate:</b> ", scales::percent(Income_Loss_Rate, accuracy = 1)) ) %>%
#   addLegend("bottomright", pal = pal, values = ~Damage_Rate, title = "Home Damage Rate", labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x), opacity = 1)

ggplot(df, aes(x = as.factor(q7), y = FRATPI)) + geom_boxplot(fill = "lightblue") +
  labs(title = "Flood Risk Perception by Experience", x = "Experienced Flooding (Q7)", y = "FRATPI")

# section 3: Flood preparedness behaviours and adaptive capacity
ggplot(df, aes(x = q12)) + geom_bar(fill = "forestgreen") + theme_minimal() + labs(title = "Households with Emergency Supplies", x = "Response", y = "Number of Households")

df %>% count(q13) %>% ggplot(aes(x = "", y = n, fill = q13)) + geom_col(width = 1) + coord_polar(theta = "y") + theme_void() + scale_fill_brewer(palette = "Set2") + labs(title = "Willingness to Purchase Flood Insurance", fill = "Response")

ggplot(df, aes(x = q14)) + geom_bar(fill = "steelblue") + theme_minimal() + labs(title = "Knowledge of Nearest Flood Shelter", x = "Response", y = "Number of Respondents")

# section 4: government trust
df_long_trust <- df %>% select(q15, q16, q17) %>% pivot_longer(cols = everything(), names_to = "Question", values_to = "Score")

ggplot(df_long_trust, aes(x = factor(Score), fill = Question)) + geom_bar(position = "dodge") + theme_minimal() +
  labs(title = "Government Trust and Infrastructure Adequacy", x = "Score (1 = Low Trust, 5 = High Trust)", y = "Number of Respondents")

df$q18_clean <- case_when(str_detect(tolower(df$q18), "yes") ~ "Yes", str_detect(tolower(df$q18), "no") ~ "No", TRUE ~ NA_character_)
ggplot(df, aes(x = q18_clean)) + geom_bar(fill = "darkolivegreen") + theme_minimal() +
  labs(title = "Received Support from Local Leaders/Orgs", x = "Response", y = "Count")


df_suggestions <- tibble::tibble(respondent_no = 1:length(df$q19), suggestion = df$q19)
df_suggestions <- df_suggestions %>% drop_na() %>% 
  mutate(theme = case_when(str_detect(suggestion, regex("drainage|sewer|clean", ignore_case = TRUE)) ~ "Drainage & Infrastructure",
    str_detect(suggestion, regex("road|urban planning|congestion", ignore_case = TRUE)) ~ "Urban Planning & Roads",
    str_detect(suggestion, regex("forecasting|sdrf|ndrf|support|assistance", ignore_case = TRUE)) ~ "Preparedness & Relief",
    str_detect(suggestion, regex("garbage|waste|segregation", ignore_case = TRUE)) ~ "Solid Waste Management",
    str_detect(suggestion, regex("dams|rivers", ignore_case = TRUE)) ~ "Water Bodies Management",
    str_detect(suggestion, regex("policy", ignore_case = TRUE)) ~ "Policy & Governance"))

df_suggestions %>% count(theme) %>% ggplot(aes(x = reorder(theme, n), y = n, fill = theme)) +  geom_col() + coord_flip() + theme_minimal() + scale_fill_brewer(palette = "Set2") + labs(title = "Most Suggested Government Interventions", x = "Theme", y = "Number of Respondents")

### Flood Vulnerability Index (using Z-scores)
# Step 1: set up data
df <- df %>% mutate(q7_num = if_else(q7 == "Yes", 1, 0), q9_num = case_when(q9 == "More than 3 times" ~ 4, q9 == "1-3 times" ~ 1.5, q9 == "Never" ~ 0, TRUE ~ NA_real_),
                    q10_num = if_else(q10 == "Yes", 1, 0), q11_num = if_else(q11 == "Yes", 1, 0), q12_num = if_else(q12 == "Yes", 1, 0), q14_num = if_else(q14 == "Yes", 1, 0), 
                    income_num = case_when(Income == "Below ₹15,000" ~ 1, Income == "₹15,000-₹50,000" ~ 2, Income == "Above ₹50,000" ~ 3, TRUE ~ NA_real_) )
# Step 2: Standardize selected variables (Z-Scores)
vars_to_scale <- c("q7_num", "q9_num", "q10_num", "q11_num", "q12_num", "q14_num", "income_num")
df_z <- df %>% select(all_of(vars_to_scale)) %>% scale(center = TRUE, scale = TRUE) %>% as_tibble()
# Step 3: Compute composite vulnerability index (mean of z-scores, higher = more vulnerable)
df$Vulnerability_Index <- rowMeans(df_z, na.rm = TRUE)
# Step 4: Aggregate vulnerability by location
vulnerability_by_location <- df %>% group_by(Location_clean) %>% summarise(Avg_Vulnerability = mean(Vulnerability_Index, na.rm = TRUE), Respondents = n() )
# Step 5: Join with geolocation data
map_data_vuln <- geo_locations %>% rename(Location_clean = address) %>% left_join(vulnerability_by_location, by = "Location_clean")
# Step 6: Create interactive vulnerability heatmap
pal_vuln <- colorNumeric(palette = "YlOrRd", domain = map_data_vuln$Avg_Vulnerability)
# leaflet(data = map_data_vuln) %>% addTiles() %>%
#   addCircleMarkers(lng = ~long, lat = ~lat, radius = ~Respondents*3, color = ~pal_vuln(Avg_Vulnerability), fillOpacity = 0.85, popup = ~paste0("<b>", Location_clean, "</b><br/>", "Avg. Vulnerability Index: ", round(Avg_Vulnerability, 2), "<br/>", "Respondents: ", Respondents)) %>%
#   addLegend("bottomright", pal = pal_vuln, values = ~Avg_Vulnerability, title = "Avg. Vulnerability Index", labFormat = labelFormat(digits = 2))
