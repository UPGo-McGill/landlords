

host_numeric %>% filter(Minimum_Stay <= 7) %>% 
  filter(n_reserved >90) %>% 
  ggplot(aes(Minimum_Stay, count(Minimum_Stay))) + 
  geom_point(aes(size = Total_Listings))

ggplot(host_numeric, aes(Rating)) + 
  geom_freqpoly()


host_numeric %>% filter(Minimum_Stay <= 7 & n_reserved >90 & n_available > 183 & Total_Listings <20) %>% ggplot(aes(avg_nightly_rate, Total_Listings)) +
  geom_point(alpha = 1/10)

host_numeric %>% filter(Total_Listings < 26) %>% ggplot(aes(Total_Listings, Total_Revenue)) +
  stat_smooth()

ggplot(filter(host_numeric, Total_Listings < 26 & HomeAway_PID == 0), aes(Total_Listings, Total_Revenue)) +
  geom_point(alpha = 1/200)

hosts %>% filter(Minimum_Stay < 30) %>% ggplot(aes(Minimum_Stay, Age)) +
  geom_point(alpha = 0.1)

hosts  %>%  ggplot(mapping = aes(Strictness_Index)) + 
  geom_freqpoly(binwidth = 0.15)

## Superhost analysis
hosts %>% na.omit(Superhost) %>%   filter(Total_Listings <= 20) %>% ggplot(mapping = aes(Total_Listings, ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Superhost), binwidth = 3)

hosts  %>%  na.omit(Superhost) %>%  ggplot(mapping = aes(Strictness_Index, ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Superhost), binwidth = .10)

hosts  %>%  na.omit(Superhost) %>%  ggplot(mapping = aes(n_available, ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Superhost), binwidth = 20)

hosts  %>%  na.omit(Superhost) %>% filter(Total_Revenue < 100000) %>% ggplot(aes(Superhost, Total_Revenue)) + 
  geom_boxplot(aes(group = Superhost))



hosts %>%  na.omit(Listings) %>% ggplot(aes(n_reserved,  ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Listings))

hosts %>% filter(Total_Listings <= 15) %>% ggplot(aes(Total_Listings, Strictness_Index)) + 
  geom_boxplot(aes(group = cut_width(Total_Listings, 1)))

hosts %>% filter(Total_Listings <= 15) %>% ggplot(aes(Total_Listings, Response_Rate)) + 
  geom_boxplot(aes(group = cut_width(Total_Listings, 1)))


library(hexbin)

##Looking at only multilistings
ggplot(host_numeric %>% filter(Total_Listings < 50 & ML == 1)) + 
  geom_hex(mapping = aes(Total_Listings, avg_nightly_rate))

ggplot(host_numeric %>% filter(Total_Listings < 30 )) + 
  geom_hex(mapping = aes(Total_Listings, Strictness_Index), bins = 35) + labs(x = "Total Listings", y = "Strictness Index", title = "Host listings and nightly rate")



## Correlation matrix visualizations

host_matrix %>%
  rearrange(method = "MDS", absolute = FALSE) %>%
  shave() %>% 
  rplot(shape = 15, colors = c("red", "green"))