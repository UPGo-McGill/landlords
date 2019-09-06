
############### DATA IMPORT ##################

## Load helpers

source("R/00_helper_functions.R")


## Import Airbnb files

property <- read_csv("data/Canada-montreal_Property_Match_2019-05-28.csv", col_types = cols_only(
    `Property ID` = col_character(),
    `Listing Title` = col_character(),
    `Property Type` = col_character(),
    `Listing Type` = col_character(),
    `Created Date` = col_date(format = ""),
    `Last Scraped Date` = col_date(format = ""),
    Latitude = col_double(),
    Longitude = col_double(),
    Neighborhood = col_character(),
    `Bedrooms` = col_double(),
    `Bathrooms` = col_double(),
    `Max Guests` = col_double(),
    `Calendar Last Updated` = col_date(format = ""),
    `Response Rate` = col_double(),
    `Airbnb Superhost` = col_logical(),
    `HomeAway Premier Partner` = col_logical(),
    `Cancellation Policy` = col_character(),
    `Security Deposit (USD)` = col_double(),
    `Cleaning Fee (USD)` = col_double(),
    `Extra People Fee (USD)` = col_double(),
    `Check-in Time` = col_character(),
    `Checkout Time` = col_character(),
    `Minimum Stay` = col_double(),
    `Number of Reviews` = col_double(),
    `Number of Photos` = col_double(), 
    `Instantbook Enabled` = col_logical(),
    `Overall Rating` = col_double(),
    `Airbnb Property ID` = col_double(),
    `Airbnb Host ID` = col_double(),
    `HomeAway Property ID` = col_character(),
    `HomeAway Property Manager` = col_character()))%>%
    set_names(c(    "Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
                    "Created", "Scraped", "Latitude", "Longitude", "Neighbourhood",
                    "Bedrooms", "Bath", "Max_Guests", "Last_Update", "Response_Rate", 
                    "Superhost", "HomeAway_PP", "Cancellation_Policy", "Security_Deposit",
                    "Cleaning_Fee", "Ex_People_Fee", "Checkin_Time", "Checkout_Time",
                    "Minimum_Stay", "Reviews", "Photos", "Instantbook", "Rating",
                    "Airbnb_PID", "Airbnb_HID", "HomeAway_PID", "HomeAway_Manager")) %>% 
  arrange(Property_ID) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618) %>% 
  filter(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", "")) %>% 
  select(-Property_Type)




daily <- 
  read_csv("data/Canada-montreal_Daily_Match_2019-05-28.csv", col_types = cols(
    `Property ID` = col_character(),
    Date = col_date(format = ""),
    Status = col_factor(levels = c("U", "B", "A", "R")),
    `Booked Date` = col_skip(),
    `Price (USD)` = col_double(),
    `Price (Native)` = col_skip(),
    `Currency Native` = col_skip(),
    `Reservation ID` = col_skip(),
    `Airbnb Property ID` = col_double(),
    `HomeAway Property ID` = col_character())) %>% 
  set_names(c("Property_ID", "Date", "Status", "Price", "Airbnb_PID", 
              "HomeAway_PID")) %>% 
  filter(!is.na(Status)) %>%
  arrange(Property_ID, Date)


## Trim listings to last available full year, May 2018-April 2019, join property and daily file

property <-
  property %>% 
  filter(Property_ID %in% daily$Property_ID,
         Scraped >= "2018-05-01",
         Created <= "2019-04-30") 

daily <- 
  daily %>% 
  filter(Property_ID %in% property$Property_ID,
         Date >= "2018-05-01",
         Date <= "2019-04-30") %>%
  inner_join(st_drop_geometry(property))  %>% 
  select(Property_ID, Date, Status, Price, Airbnb_PID, HomeAway_PID, Airbnb_HID, Listing_Type)


## Find total reserved and available nights, and total revenue

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarise(
    n_reserved = sum(Status == "R"),
    n_available = sum(Status == "A" | Status == "R"),
    revenue = sum((Status == "R") * Price)) %>% 
  inner_join(property, .)



## Find multi-listings and and Frequenty Rented Entire Homes (FREH)

multilistings <- strr_multilistings(daily, listing_type = Listing_Type,
                                    host_ID = Airbnb_HID, date = Date) %>%
  group_by(Property_ID) %>% 
  summarise(ML = as.logical(ceiling(mean(ML))))

daily_FREH <- strr_FREH(daily, start_date = end_date, end_date = end_date) %>%
  filter(FREH == TRUE) %>%
  select(Property_ID) %>%
  distinct()


property <- property %>%
  inner_join(multilistings) %>%
  mutate (FREH = property$Property_ID %in% daily_FREH$Property_ID) 

rm(multilistings, daily_FREH)

# Identify ghost hotels

GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, "2018-05-01",
             "2019-04-30", listing_type = Listing_Type) %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(Property_ID %in% GH_list, TRUE, FALSE))

rm(GH_list)

##Remove listings that were not reserved once over the last year

active_property <- property %>%
  filter(n_reserved > 0)


##save as new file   
write.csv(property, file = "data/property_cleaned.csv")






##convert variables to numeric for calculations

property_numeric <- active_property %>% 
  mutate(FREH = as.binary(FREH, logic=TRUE),
        Superhost = as.binary(Superhost, logic=TRUE),
        Last_Update = as.numeric(difftime(Scraped, Last_Update, units = "days")),
        Age = as.numeric(difftime(Scraped, Created, units = "days")),
     #   Downtown = if_else(Neighbourhood == "Ville-Marie" | Neighbourhood == "Le Plateau-Mont-Royal", 1, 0),
        Cancellation_Strictness = case_when(
          str_detect(Cancellation_Policy, "Flexible")     ~ 0,
          str_detect(Cancellation_Policy, "Moderate")     ~ 0.5,
          str_detect(Cancellation_Policy, "Strict")       ~ 1),
        Checkin_Time = if_else(is.na(Checkin_Time) | Checkin_Time == "Flexible", 0, 1),
        Checkout_Time = if_else(is.na(Checkout_Time), 0, 1),
        Security_Deposit = if_else(is.na(Security_Deposit), 0, 1),
        Cleaning_Fee = if_else(is.na(Cleaning_Fee), 0, 1),
        Ex_People_Fee = if_else(is.na(Ex_People_Fee), 0, 1),
        Strictness_Index = (Checkin_Time + Checkout_Time + Security_Deposit + 
                              Cleaning_Fee + Ex_People_Fee + Cancellation_Strictness)/6,
        Minimum_Stay_30 = if_else(Minimum_Stay >= 30, 1, 0),
        Minimum_Stay_1 = if_else(Minimum_Stay <=1, 1, 0),
        Minimum_Stay_2to3 = if_else(Minimum_Stay ==2 | Minimum_Stay ==3 , 1, 0),
        Minimum_Stay_4to29 = if_else(Minimum_Stay >3 & Minimum_Stay <30 , 1, 0),
        Response_100 = if_else(Response_Rate < 100,0, 1),
        Instantbook = if_else(Instantbook == TRUE, 1, 0),
        GH = if_else(GH == TRUE, 1, 0),
    #    Response_90 = if_else(Response_Rate < 90,0, 1),
    #    EH = if_else(Listing_Type == "Entire home/apt", 1, 0),
    #    PR = if_else(Listing_Type == "Private room", 1, 0),
    #    SR = if_else(Listing_Type == "Shared room", 1, 0),
        HomeAway_PID = if_else(is.na(HomeAway_PID), 0, 1),
        Occupancy_Rate = if_else(n_available == 0, 0, n_reserved/n_available),
        avg_nightly_rate = revenue/n_reserved) %>% 
  select(-Property_ID, -Listing_Type, -Listing_Title, -Created, -Scraped, 
         -Neighbourhood, -HomeAway_PP, -HomeAway_Manager, -Airbnb_PID 
    #     -Cancellation_Policy, -Cancellation_Strictness, -Checkin_Time, 
    #     -Checkout_Time, -Security_Deposit, -Cleaning_Fee, -Ex_People_Fee
         ) %>% 
  st_drop_geometry() 



##Group by host and make find the mean of all their listings for each value
host_numeric <- property_numeric %>%
  group_by(Airbnb_HID) %>% 
  mutate(Total_Listings = n()) %>%
  summarise_all(list(mean)) %>%
  mutate (Total_Revenue = revenue*Total_Listings,
          Listing_2to9 = if_else(Total_Listings > 1 & Total_Listings < 10, 1, 0),
          Listing_1 = if_else(Total_Listings == 1, 1, 0),
          Listing_10up = if_else(Total_Listings > 10, 1, 0)) %>%
  select (-Airbnb_HID)

hosts_categorical <- host_numeric %>% mutate(
Listings = case_when(
 Total_Listings == 1    ~ "1 listing",
  Total_Listings > 1 & Total_Listings < 10     ~ "2-9 listings",
  Total_Listings > 10     ~ "10+ listings"), 
Superhost = if_else(Superhost == 1, "Yes", "No"))




##Create correlation matrix
host_matrix <- host_numeric %>% 
  correlate(method = "spearman") %>%  
  stretch(na.rm = TRUE, remove.dups = TRUE) %>% 
  mutate (r = round(r,3))

Superhost_corr <- host_matrix %>% 
  filter(x == "Superhost" | y == "Superhost") %>% 
  arrange(-r)

photos_corr <- host_matrix %>% 
  filter(x == "Photos" | y == "Photos") %>% 
  arrange(-r)

Minimum_stay_corr <-  host_matrix %>% 
  filter( str_detect(x, "^Minimum") | str_detect(y, "^Minimum")) %>% 
  arrange(-r)

no_HomeAway <- host_numeric %>% 
  filter(HomeAway_PID == 0) %>%
  correlate(method = "spearman") %>% 
  stretch(na.rm = TRUE, remove.dups = TRUE) %>% 
  mutate (r = round(r,3))

availability_corr <- no_HomeAway %>%
  filter(x == "n_reserved" | x == "n_available" | y == "n_reserved" | y == "n_available") %>% 
  arrange(-r)

revenue_corr <- no_HomeAway %>%
  filter(x == "revenue" | x == "avg_nightly_rate" | y == "revenue" | y == "avg_nightly_rate" | y == "Total_Revenue") %>% 
    arrange(-r)  
  
View(host_matrix %>%
       filter( str_detect(x, "^Minimum") | str_detect(y, "^Minimum")) %>% filter()%>% arrange(-r) )


GH <- host_numeric %>% filter(GH>0)

GH_corr <- host_matrix %>%
  filter(x == "GH" | y == "GH") %>% 
  arrange(-r) 

## Examine distribution of hosts' total listings
host_numeric %>% count(cut_width(Total_Listings, 4))
  
##Find percentage of Superhosts
mean(host_numeric$Instantbook, na.rm = TRUE)

##Examine Response rate

library(Hmisc)
describe(hosts$GH)

