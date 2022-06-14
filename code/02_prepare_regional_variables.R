# Objective: Prepare explanatory regional variables at the household level.


# Import Libraries --------------------------------------------------------

library(tidyverse)
library(haven)
library(janitor)


# Load Data ---------------------------------------------------------------

# Regional information is sourced from the sec0a.dta file. 

# Regional information
# Data file: sec0a
# Columns: region, district, eanum, clust, nh, ez, loc2, loc5, loc3

# Select enumeration area number columns, (eanum) and (clust), and household ID
# (nh) to use as a unique key for each household. Eanum and clust are both the 
# enumeration area number, but slightly different format. Clust includes a 4 and 
# however many 0s are needed at the front to create a four digit number. 
# Keeping both clust and eanum in if needed for data merging. Select region 
# (region), district (district), ecological zone (ez), urban/rural (loc2), 
# urban/rural by ecological zone (loc5), and capital/urban/rural (loc3) as
# variables for regional comparison. Translate coded values and rename nh to hh
# to standardize with other data tables.Values were translated using the Annex 
# District Codes (p.188) from the "Ghana Living Standards Survey Round Four 
# (GLSS p) 1998/99 Data User's Guide" (G4USERSG.pdf).

region <- read_dta("raw_data/sec0a.dta") %>%
  transmute(
    clust,
    eanum,
    hh = nh,
    region = case_when(
      region == 1 ~ "Western",
      region == 2 ~ "Central",
      region == 3 ~ "Greater Accra",
      region == 4 ~ "Eastern",
      region == 5 ~ "Volta",
      region == 6 ~ "Ashanti",
      region == 7 ~ "Brong Ahafo",
      region == 8 ~ "Northern",
      region == 9 ~ "Upper East",
      region == 10 ~ "Upper West",
      TRUE ~ NA_character_
    ),
    district = case_when(
      (region == "Western" & district == 1) ~ "Jomoro",
      (region == "Western" & district == 2) ~ "Nzema",
      (region == "Western" & district == 3) ~ "Ahanta West",
      (region == "Western" & district == 4) ~ "Shama-Ahanta East",
      (region == "Western" & district == 5) ~ "Mpohor-Wassa East",
      (region == "Western" & district == 6) ~ "Wassa West",
      (region == "Western" & district == 7) ~ "Wassa Amemfi",
      (region == "Western" & district == 8) ~ "Aowin-Suaman",
      (region == "Western" & district == 9) ~ "Juabeso-Bia",
      (region == "Western" & district == 10) ~ "Sefwi-Wiawso",
      (region == "Western" & district == 11) ~ "Bibiani/Anhwiaso/Wekwai",
      (region == "Central" & district == 1) ~ "Komenda/Edina/Eguafo/Abirem",
      (region == "Central" & district == 2) ~ "Cape Coast",
      (region == "Central" & district == 3) ~ "Abura/Asebu Kwamankese",
      (region == "Central" & district == 4) ~ "Mfantsiman",
      (region == "Central" & district == 5) ~ "Gomoa",
      (region == "Central" & district == 6) ~ "Awutu/Efutu Senya",
      (region == "Central" & district == 7) ~ "Agona",
      (region == "Central" & district == 8) ~ "Asikuma/Odoben/Brakwa",
      (region == "Central" & district == 9) ~ "Ajumako/Enyan/Essiam",
      (region == "Central" & district == 10) ~ "Assin",
      (region == "Central" & district == 11) ~ "Twifo/Heman/Lower Denkyira",
      (region == "Central" & district == 12) ~ "Upper Denkyira",
      (region == "Volta" & district == 1) ~ "South Tongu",
      (region == "Volta" & district == 2) ~ "Keta",
      (region == "Volta" & district == 3) ~ "Ketu",
      (region == "Volta" & district == 4) ~ "Akatsi",
      (region == "Volta" & district == 5) ~ "North Tongu",
      (region == "Volta" & district == 6) ~ "Ho",
      (region == "Volta" & district == 7) ~ "Hohoe",
      (region == "Volta" & district == 8) ~ "Kpandu",
      (region == "Volta" & district == 9) ~ "Jasikan",
      (region == "Volta" & district == 10) ~ "Kadjebi",
      (region == "Volta" & district == 11) ~ "Nkwanta",
      (region == "Volta" & district == 12) ~ "Krachi",
      (region == "Northern" & district == 1) ~ "Bole",
      (region == "Northern" & district == 2) ~ "West Gonja",
      (region == "Northern" & district == 3) ~ "East Gonja",
      (region == "Northern" & district == 4) ~ "Nanumba",
      (region == "Northern" & district == 5) ~ "Zabzugu-Tatale",
      (region == "Northern" & district == 6) ~ "Chereponi-Saboba",
      (region == "Northern" & district == 7) ~ "Yendi",
      (region == "Northern" & district == 8) ~ "Gushiegu-Karaga",
      (region == "Northern" & district == 9) ~ "Savelugu-Nanton",
      (region == "Northern" & district == 10) ~ "Tamale",
      (region == "Northern" & district == 11) ~ "Tolon-Kumbungu",
      (region == "Northern" & district == 12) ~ "West Mamprusi",
      (region == "Northern" & district == 13) ~ "East Mamprusi",
      (region == "Brong Ahafo" & district == 1) ~ "Asunafo",
      (region == "Brong Ahafo" & district == 2) ~ "Asutifi",
      (region == "Brong Ahafo" & district == 3) ~ "Tanaso",
      (region == "Brong Ahafo" & district == 4) ~ "Sunyani",
      (region == "Brong Ahafo" & district == 5) ~ "Dormaa",
      (region == "Brong Ahafo" & district == 6) ~ "Jaman",
      (region == "Brong Ahafo" & district == 7) ~ "Berekum",
      (region == "Brong Ahafo" & district == 8) ~ "Wenchi",
      (region == "Brong Ahafo" & district == 9) ~ "Techiman",
      (region == "Brong Ahafo" & district == 10) ~ "Nkroranza",
      (region == "Brong Ahafo" & district == 11) ~ "Kintampo",
      (region == "Brong Ahafo" & district == 12) ~ "Atebubu",
      (region == "Eastern" & district == 1) ~ "Birim North",
      (region == "Eastern" & district == 2) ~ "Birim South",
      (region == "Eastern" & district == 3) ~ "West Akim",
      (region == "Eastern" & district == 4) ~ "Kwaebibirem",
      (region == "Eastern" & district == 5) ~ "Suhum/Kraboa/Coaltar",
      (region == "Eastern" & district == 6) ~ "East Akim",
      (region == "Eastern" & district == 7) ~ "Fanteakwa",
      (region == "Eastern" & district == 8) ~ "New Juaben",
      (region == "Eastern" & district == 9) ~ "Akwapim South",
      (region == "Eastern" & district == 10) ~ "Akwapim Northr",
      (region == "Eastern" & district == 11) ~ "Yilo Krobo",
      (region == "Eastern" & district == 12) ~ "Manya Krobo",
      (region == "Eastern" & district == 13) ~ "Asuagyaman",
      (region == "Eastern" & district == 14) ~ "Afram Plains",
      (region == "Eastern" & district == 15) ~ "Kwahu South",
      (region == "Greater Accra" & district == 1) ~ "Accra",
      (region == "Greater Accra" & district == 2) ~ "Ga",
      (region == "Greater Accra" & district == 3) ~ "Tema",
      (region == "Greater Accra" & district == 4) ~ "Dangbe West",
      (region == "Greater Accra" & district == 5) ~ "Dangbe East",
      (region == "Upper West" & district == 1) ~ "Wa",
      (region == "Upper West" & district == 2) ~ "Nadowli",
      (region == "Upper West" & district == 3) ~ "Sissala",
      (region == "Upper West" & district == 4) ~ "Jirapah-Lambussie",
      (region == "Upper West" & district == 5) ~ "Lawra",
      (region == "Upper East" & district == 1) ~ "Builsa",
      (region == "Upper East" & district == 2) ~ "Kassena-Nankani",
      (region == "Upper East" & district == 3) ~ "Bongo",
      (region == "Upper East" & district == 4) ~ "Bolgatanga",
      (region == "Upper East" & district == 5) ~ "Bawku Westi",
      (region == "Upper East" & district == 6) ~ "Bawku East",
      (region == "Ashanti" & district == 1) ~ "Atwima",
      (region == "Ashanti" & district == 2) ~ "Amansie West",
      (region == "Ashanti" & district == 3) ~ "Amansie East",
      (region == "Ashanti" & district == 4) ~ "Adansi West",
      (region == "Ashanti" & district == 5) ~ "Adansi East",
      (region == "Ashanti" & district == 6) ~ "Asante Akim South",
      (region == "Ashanti" & district == 7) ~ "Asante Akim North",
      (region == "Ashanti" & district == 8) ~ "Ejisu Juaben",
      (region == "Ashanti" & district == 9) ~ "Bosomtwi/Atwima/Kwanwoma",
      (region == "Ashanti" & district == 10) ~ "Kumasi",
      (region == "Ashanti" & district == 11) ~ "Afigya/Kwabre",
      (region == "Ashanti" & district == 12) ~ "Afigya Sekyere",
      (region == "Ashanti" & district == 13) ~ "Sekyere East",
      (region == "Ashanti" & district == 14) ~ "Sekyere West",
      (region == "Ashanti" & district == 15) ~ "Ejisu/Sekyedumasi",
      (region == "Ashanti" & district == 16) ~ "Offinso",
      (region == "Ashanti" & district == 17) ~ "Ahafo Ano South",
      (region == "Ashanti" & district == 18) ~ "Ahafo Ano North",
      TRUE ~ NA_character_
    ),
    ez = case_when(
      ez == 1 ~ "Coastal",
      ez == 2 ~ "Forest",
      ez == 3 ~ "Savannah",
      TRUE ~ NA_character_
    ),
    u_r = case_when(
      loc2 == 1 ~ "Urban",
      loc2 == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    ez_u_r = case_when(
      loc5 == 1 ~ "Accra",
      loc5 == 2 ~ "Other Urban",
      loc5 == 3 ~ "Rural Coastal",
      loc5 == 4 ~ "Rural Forest",
      loc5 == 5 ~ "Rural Savannah",
      TRUE ~ NA_character_
    ),
    u_r_c = case_when(
      loc3 == 1 ~ "Accra",
      loc3 == 2 ~ "Other Urban",
      loc3 == 3 ~ "Rural",
      TRUE ~ NA_character_
    )
  )


# Clean Data --------------------------------------------------------------

# There is an issue with translating the coded values. The Annex document 
# lists the number for a 6th Upper West district, but does not provide
# a name.

region %>%
  filter(region == "Upper West") %>%
  group_by(district) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# There are 80 households belonging to the missing Upper West district. Filling
# with placeholder name 'Upper West 6' for now. 

region <- region %>%
  mutate(district = case_when(
    region == "Upper West" & is.na(district) == TRUE ~ "Upper West 6",
    region == "Upper West" ~ district,
    region != "Upper West" ~ district,
    TRUE ~ NA_character_
  ))


# Check for missing values.

region %>%
  filter(if_any(everything(), is.na))

# There are 160 missing values, all seem to be in the district column.

region %>%
  filter(if_any(everything(), is.na)) %>%
  group_by(region) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# All missing values remaining are in the Volta region. Loading the original 
# data to view original district region breakdown. 

read_dta("raw_data/sec0a.dta") %>%
  tabyl(region, district)

# Volta is region 5 and shows 15 districts, however there are only 12 districts
# for Volta listed in the District Codes Annex. Interestingly, there are only 
# 12 districts in the data for Eastern, but there are 15 districts listed in the
# District Codes Annex. Based on this information the region and district codes
# may be incorrect. As these variables are suspect, they are being removed from
# the data and regional comparisons will be limited to ecological zone and 
# urban vs. rural.

region <- region %>%
  select(- c(district, region))

# Check for duplicate values.

region %>%
  group_by(clust, hh) %>%
  filter(n() > 1)

# No duplicate rows. Region variables are ready to be merged with other data.

