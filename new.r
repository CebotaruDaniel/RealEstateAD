library(r2country)
library(dplyr)
library(maps)
library(ggplot2)
library(gridExtra)


my_dataset <- read.csv("dataset.csv")
costofliving <- read.csv("Cost_of_Living_Index_2022.csv")
glimpse(costofliving)


data(country_continent)
data(country_names)


finaldb <- cbind(country_names,country_continent)%>% 
  select(-ID)
head(finaldb)

finaldb<- finaldb%>% 
  rename(country =name)

final_realestate <- left_join(my_dataset,finaldb, by ="country")
final_realestate <- final_realestate %>% 
  select(-location)%>% 
  select(-url)%>% 
  filter(!is.na(continent))%>%
  select(-image)
glimpse(final_realestate)

final_realestate <- left_join(final_realestate, costofliving, by = c("country" = "Country"))

final_realestate <- final_realestate %>%
  mutate(continent = ifelse(continent == "Europe & Asia", "Asia", continent))%>%
  filter(!is.na(continent))%>%
  filter(continent != "Oceania" & continent != "North America")

final_realestate <- final_realestate %>%
  mutate(apartment_total_area_numeric = as.numeric(gsub("[^0-9.]", "", apartment_total_area)))

final_realestate <- final_realestate %>%
  mutate(price_per_m2 = price_in_USD / apartment_total_area_numeric)

average_price_per_m2 <- final_realestate %>%
  group_by(continent) %>%
  summarize(average_price_per_m2 = mean(price_per_m2, na.rm = TRUE))

ggplot(average_price_per_m2, aes(x = continent, y = average_price_per_m2, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Prețul Mediu pe Metru Pătrat în Asia și Europa",
       x = "Continent",
       y = "Preț Mediu pe Metru Pătrat (USD)") +
  theme_minimal()

final_realestate <- final_realestate %>%
  mutate(realstate_type = case_when(
    grepl("penthouse", title, ignore.case = TRUE) ~ "penthouse",
    grepl("apartment", title, ignore.case = TRUE) ~ "apartment",
    grepl("casă pe ", title, ignore.case = TRUE) ~ "house",
    grepl("cottage", title, ignore.case = TRUE) ~ "cottage",
    grepl("condo", title, ignore.case = TRUE) ~ "condo",
    grepl("villa", title, ignore.case = TRUE) ~ "villa",
    TRUE ~ NA_character_
  ))

average_col <- final_realestate %>%
  group_by(continent) %>%
  summarize(Avg_COL = mean(Cost.of.Living.Index, na.rm = TRUE))

ggplot(average_col, aes(x = continent, y = Avg_COL, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Costul Mediu de Trăi în Asia și Europa",
       x = "Continent",
       y = "Indexul Mediu de Cost al Vieții") +
  theme_minimal()


avg_col_by_country <- final_realestate %>%
  group_by(country, continent) %>%
  summarize(Avg_COL = mean(Cost.of.Living.Index, na.rm = TRUE))

top_countries <- avg_col_by_country %>%
  group_by(continent) %>%
  top_n(3, wt = Avg_COL)

ggplot(top_countries, aes(x = reorder(country, -Avg_COL), y = Avg_COL, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Top 6 Țări după Costul Vieții",
       x = "Țară",
       y = "Indexul Mediu de Cost al Vieții") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


average_price_per_m2 <- final_realestate %>%
  group_by(continent, realstate_type) %>%
  filter(!is.na(realstate_type))%>%
  summarize(average_price_per_m2 = mean(price_per_m2, na.rm = TRUE))

ggplot(average_price_per_m2, aes(x = realstate_type, y = average_price_per_m2, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Prețul Mediu pe Metru Pătrat în Funcție de Tipul Proprietății Imobiliare",
       x = "Tip Proprietate Imobiliară",
       y = "Preț Mediu pe Metru Pătrat (USD)",
       fill = "Continent") +
  theme_minimal()

count_by_type <- final_realestate %>%
  group_by(continent, realstate_type) %>%
  summarize(count = n())

ggplot(count_by_type, aes(x = realstate_type, y = count, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Numărul de Proprietăți Imobiliare de Fiecare Tip în Asia și Europa",
       x = "Tip de Proprietate Imobiliară",
       y = "Număr",
       fill = "Continent") +
  theme_minimal()



my_subset <- final_realestate %>%
  select(continent,price_in_USD)

by_continent <- my_subset %>%
  group_by(continent) %>%
  summarize(mean_price = mean(price_in_USD,na.rm = TRUE))

by_continent <- by_continent %>%
  filter(!is.na(continent))
  

palette <- scale_fill_manual(values = rainbow(length(unique(my_subset$continent))))
ggplot(by_continent, aes(x = continent, y = mean_price,fill=continent)) +
  geom_bar(stat = "identity") +
  labs(title="Media Preturilor Pe Continente" ,x = "Continent", y = "Media Prețului în USD")

data_europe <- final_realestate %>% 
  filter(continent == "Europe")

ggplot(data_europe, aes(x = price_in_USD)) +
  geom_histogram(fill = "blue", bins = 100) +
  labs(title = "Distribuția Prețurilor Apartamentelor în Europa", x = "Preț în USD", y = "Număr de Apartamente")+
  scale_x_continuous(limits = c(0, 2000000))


data_asia <- final_realestate %>% 
  filter(continent == "Asia")

ggplot(data_asia, aes(x = price_in_USD)) +
  geom_histogram(fill = "purple", bins = 100) +
  labs(title = "Distribuția Prețurilor Apartamentelor în Asia", x = "Preț în USD", y = "Număr de Apartamente")+
  scale_x_continuous(limits = c(0, 2000000))


diagram_europa_etaje <- ggplot(data_europe, aes(x = building_total_floors)) +
  labs(title = "Frecventa etajelor in Europa", x = "Numarul de Etaje din Cladire", y = "Frecventa") +
  geom_bar(position = "dodge",fill = "blue",color="black") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 25, by = 1))

diagram_europa_cameri <- ggplot(data_europe, aes(x = apartment_rooms)) +
  labs(title = "Frecventa Camerelor in Europa", x = "Numarul de Camere in Apartament", y = "Frecventa") +
  geom_bar(position = "dodge",fill = "blue",color="black") +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))

# Diagramele pentru Asia
diagram_asia_etaje <- ggplot(data_asia, aes(x = building_total_floors)) +
  labs(title = "Frecventa etajelor in Asia", x = "Numarul de Etaje din Cladire", y = "Frecventa") +
  geom_histogram(position = "dodge", fill = "purple",color="black") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 3))

diagram_asia_cameri <- ggplot(data_asia, aes(x = apartment_rooms)) +
  labs(title = "Frecventa Camerelor in Asia", x = "Numarul de Camere in Apartament", y = "Frecventa") +
  geom_bar(position = "dodge",fill="purple",color="black") +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1))

grid.arrange(diagram_europa_etaje, diagram_europa_cameri, diagram_asia_etaje, diagram_asia_cameri, ncol = 2)

data_europe$location <- "Europa"
data_asia$location <- "Asia"

combined_data <- rbind(data_europe, data_asia)

combined_histogram <- ggplot(combined_data, aes(x = price_in_USD, fill = location)) +
  geom_histogram(binwidth = 10000, breaks = intervale, color = "black", alpha = 0.5) +
  labs(title = "Histograma Prețurilor Apartamentelor în Europa și Asia (Interval de 10,000 USD)",
       x = "Preț în USD",
       y = "Număr de Apartamente") +
  theme_minimal() +
  scale_fill_manual(values = c("Europa" = "blue", "Asia" = "purple"))

combined_histogram


apartament_cautat_europa <- final_realestate %>%
  filter(apartment_rooms == 1, apartment_floor == 7, building_construction_year == 2020, continent == "Europe")

if (nrow(apartament_cautat_europa) > 0) {
  cat("Prețul apartamentului cu 1 cameră, etajul 7 și construit în 2020 în Europa este:", apartament_cautat_europa$price_in_USD, "USD")
} else {
  cat("Nu a fost găsit niciun apartament care să corespundă criteriilor.")
}

apartament_cautat_asia <- final_realestate %>%
  filter(apartment_rooms == 1, apartment_floor == 7, building_construction_year == 2020, continent == "Asia")

if (nrow(apartament_cautat_asia) > 0) {
  cat("Prețul apartamentului cu 1 cameră, etajul 7 și construit în 2020 în Asia este:", apartament_cautat_asia$price_in_USD, "USD")
} else {
  cat("Nu a fost găsit niciun apartament care să corespundă criteriilor.")
}

apartamente_1_camera_europe <- data_europe %>%
  filter(apartment_rooms == 1) %>%
  mutate(origin = "Europa")

apartamente_1_camera_asia <- data_asia %>%
  filter(apartment_rooms == 1, !is.na(building_construction_year), !is.na(price_in_USD)) %>%
  mutate(origin = "Asia")

combined_data <- rbind(apartamente_1_camera_europe, apartamente_1_camera_asia)

ggplot(combined_data, aes(x = building_construction_year, y = price_in_USD, color = origin)) +
  geom_point() +
  labs(title = "Prețurile Apartamentelor cu 1 Cameră în funcție de Anul de Construcție",
       x = "Anul de Construcție",
       y = "Preț în USD",
       color = "Origine") +
  scale_x_continuous(limits = c(2000, 2023), breaks = seq(2000, 2023, by = 1)) +
  scale_y_continuous(limits = c(10000, 1000000), breaks = seq(10000, 1000000, by = 150000)) +
  theme_minimal()

  plot_top6 <- ggplot(top_countries, aes(x = reorder(country, -Avg_COL), y = Avg_COL, fill = continent)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = "Top 6 Țări după Costul Vieții",
         x = "Țară",
         y = "Indexul Mediu de Cost al Vieții") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_continent <- ggplot(avg_col_by_country, aes(x = reorder(country, -Avg_COL), y = Avg_COL, fill = continent)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = "Indexul Mediu de Cost al Vieții pe Țară și Continent",
         x = "Țară",
         y = "Indexul Mediu de Cost al Vieții") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(plot_top6, plot_continent, ncol = 2)
  

selected_data <- final_realestate %>%
    mutate(log_area = log1p(apartment_total_area_numeric)) %>%
    select(price_in_USD, log_area, building_total_floors, 
           apartment_floor, apartment_rooms, apartment_bedrooms, apartment_bathrooms)
  
  cleaned_data <- na.omit(selected_data)
  
  lm_model <- lm(price_in_USD ~ ., data = cleaned_data)
  
  summary(lm_model)

  ggplot(cleaned_data, aes(x = log_area, y = price_in_USD)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Regresie Liniară",
         x = "log_area",
         y = "price_in_USD")
  
  glimpse(final_realestate)  
  
  
  library(ggplot2)
  

  apartamente_cautate_europa <- final_realestate %>%
    filter(apartment_rooms == 3, apartment_floor== 7, continent == "Europe")
  
  apartamente_cautate_asia <- final_realestate %>%
    filter(apartment_rooms == 3,apartment_floor== 7, continent == "Asia")
  

  pret_pe_m2_cautat <- rbind(
    data.frame(Continent = "Europa", Pret_pe_m2 = apartamente_cautate_europa$price_per_m2),
    data.frame(Continent = "Asia", Pret_pe_m2 = apartamente_cautate_asia$price_per_m2)
  )
  

  ggplot(pret_pe_m2_cautat, aes(x = Pret_pe_m2, fill = Continent)) +
    geom_density(alpha = 0.9) +
    labs(title = "Distribuția prețului pe metru pătrat pentru apartamentele căutate",
         x = "Preț pe metru pătrat (USD)",
         y = "Densitate") +
    theme_minimal()


  
      
  
  
  

  