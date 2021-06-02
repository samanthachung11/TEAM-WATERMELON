covid_19 <- read.csv("country_vaccinations.csv")

zimbabwe_data <- covid_19[covid_19$country == "Zimbabwe", c("date", "daily_vaccinations_per_million")]
#Zimbabwe population

south_africa_data <- covid_19[covid_19$country == "South Africa",c("date", "daily_vaccinations_per_million")]

angola_data <- covid_19[covid_19$country == "Angola", c("date", "daily_vaccinations_per_million")]

nigeria_data <- covid_19[covid_19$country == "Nigeria", c("date", "daily_vaccinations_per_million")]

senegal_data <- covid_19[covid_19$country == "Senegal", c("date", "daily_vaccinations_per_million")]

uganda_data <- covid_19[covid_19$country == "Uganda", c("date", "daily_vaccinations_per_million")]

togo_data <- covid_19[covid_19$country == "Togo", c("date", "daily_vaccinations_per_million")]



namibia_data <- covid_19[covid_19$country == "Namibia", c("date", "daily_vaccinations_per_million")]

plot(zimbabwe_data$date, zimbabwe_data$daily_vaccinations_per_million, type = "l", xlab = "Time", ylab = "Daily vaccinations per million", main = "Daily vaccinations for countries in continent Africa")

points(south_africa_data$date, south_africa_data$daily_vaccinations_per_million, col = "orange")
points(angola_data$date, angola_data$daily_vaccinations_per_million, col = "blue")
points(nigeria_data$date, nigeria_data$daily_vaccinations_per_million, col = "pink")
points(senegal_data$date, senegal_data$daily_vaccinations_per_million, col = "green")
points(uganda_data$date, uganda_data$daily_vaccinations_per_million, col = "purple")
points(togo_data$date, togo_data$daily_vaccinations_per_million, col = "red")
points(namibia_data$date, namibia_data$daily_vaccinations_per_million, col = "coral")

legend("bottomleft", c("South Africa", "Zimbabwe", "Angola", "Nigeria", "Senegal", "Uganda", "Togo", "Namibia"), fill = c("orange", "black", "blue", "pink", "green", "purple", "red", "coral"), cex = 0.7)

p <- ggplot(zimbabwe_data, aes(x=date, y= daily_vaccinations_per_million, group= 1)) +
  geom_line(aes(color = "red")) + theme(axis.text.x = element_text(angle = 90)) + xlab("time") + ylab("Daily Vaccinations Per Million") + ggtitle ("Daily Vaccinations Per Million against Time for a subset of Countries in Continent Africa") +
 geom_line(data = south_africa_data, aes(x = date, y = daily_vaccinations_per_million, color = "blue"))+
geom_line(data = angola_data, aes(x = date, y = daily_vaccinations_per_million, col = "orange")) +
  geom_line(data = nigeria_data, aes(x = date, y = daily_vaccinations_per_million, col = "pink")) +
  geom_line(data = senegal_data, aes(x = date, y = daily_vaccinations_per_million, col = "coral")) +
  geom_line(data = uganda_data, aes(x = date, y = daily_vaccinations_per_million, col = "green"))+
  geom_line(data = togo_data, aes(x= date, y = daily_vaccinations_per_million, col = "purple"))+
  geom_line(data = namibia_data, aes(x= date, y = daily_vaccinations_per_million, col = "coral4"))+
scale_color_identity(name = "Countries",
                     breaks = c( "red", "blue", "orange", "pink", "coral", "green", "purple", "coral4"),
                     labels = c("Zimbabwe", "South Africa", "Angola", "Nigeria", "Senegal", "Uganda", "Togo", "Namibia"),
                     guide = "legend") + scale_x_date(breaks = data_breaks("1 month"))

p


### Daily vaccinations for a subset of countries in South America

brazil_data <- covid_19[covid_19$country == "Brazil",]
chile_data <- covid_19[covid_19$country == "Chile",]
argentina_data <- covid_19[covid_19$country == "Argentina",]
panama_data <- covid_19[covid_19$country == "Panama",]
bolivia_data <- covid_19[covid_19$country == "Bolivia",]
peru_data <- covid_19[covid_19$country == "Peru",]
venezuela_data <- covid_19[covid_19$country == "Venezuela",]
colombia_data <- covid_19[covid_19$country == "Colombia",]


p_1 <- ggplot(brazil_data, aes(x=date, y= daily_vaccinations_per_million, group= 1)) +
  geom_line(aes(color = "red")) + theme(axis.text.x = element_text(angle = 90)) + xlab("time") + ylab("Daily Vaccinations Per Million") + ggtitle ("Daily Vaccinations Per Million against Time for a subset of Countries in Continent South America") +
  geom_line(data = chile_data, aes(x = date, y = daily_vaccinations_per_million, color = "cornsilk4"))+
  geom_line(data = argentina_data, aes(x = date, y = daily_vaccinations_per_million, color = "pink"))+
  geom_line(data = panama_data, aes(x = date, y = daily_vaccinations_per_million, color = "cyan"))+
  geom_line(data = bolivia_data, aes(x = date, y = daily_vaccinations_per_million, color = "chocolate"))+
  geom_line(data = peru_data, aes(x = date, y = daily_vaccinations_per_million, color = "green")) +
  geom_line(data = venezuela_data, aes (x = date, y = daily_vaccinations_per_million, color = "aquamarine1")) +
  geom_line(data = colombia_data, aes (x = date, y = daily_vaccinations_per_million, color = "darkgoldenrod4"))+
  scale_color_identity(name = "Countries",
                       breaks = c( "red", "cornsilk4", "pink","cyan", "chocolate", "green", "aquamarine1", "darkgoldenrod4"),
                       labels = c("Brazil", "Chile", 'Argentina', "Panama", "Bolivia", "Peru", "Venezuela", "Colombia"),
                       guide = "legend")
p_1


### Canada and Australia

canada_data <- covid_19[covid_19$country=="Canada", c("date", "daily_vaccinations_per_million")]
australia_data <- covid_19[covid_19$country == "Australia", c("date", "daily_vaccinations_per_million")]

### US data
us_data <- covid_19[covid_19$country == "United States", c("date", "daily_vaccinations_per_million")]

## Countries in Europe

italy_data <- covid_19[covid_19$country == "Italy", c("date", "daily_vaccinations_per_million")]
finland_data <- covid_19[covid_19$country == "Finland", c("date", "daily_vaccinations_per_million")]
poland_data <- covid_19[covid_19$country == "Poland", c("date", "daily_vaccinations_per_million")]
norway_data <- covid_19[covid_19$country == "Norway", c("date", "daily_vaccinations_per_million")]
france_data <- covid_19[covid_19$country == "France", c("date", "daily_vaccinations_per_million")]
germany_data <- covid_19[covid_19$country == "Germany", c("date", "daily_vaccinations_per_million")]
iceland_data <- covid_19[covid_19$country == "Iceland", c("date", "daily_vaccinations_per_million")]

p_2 <- ggplot(us_data, aes(x=date, y= daily_vaccinations_per_million, group= 1)) +
  geom_line(aes(color = "purple")) + theme(axis.text.x = element_text(angle = 90)) + xlab("time") + ylab("Daily Vaccinations Per Million") + ggtitle ("Daily Vaccinations Per Million against Time for countries in North America") +
  geom_line(data = canada_data, aes(x = date, y = daily_vaccinations_per_million, color = "orange")) + 
   scale_color_identity(name = "Countries",
                        breaks = c( "purple", "orange"),
                        labels = c("United States", "Canada"),
                        guide = "legend")
 
p_3 <- ggplot(australia_data, aes(x = date, y = daily_vaccinations_per_million, group = 1))+
  geom_line(aes(color = "red")) + theme(axis.text.x = element_text(angle = 90)) + xlab("time") + ylab("Daily Vaccinations Per Million") + ggtitle("Daily Vaccinations Per Million against Time for Australia")+
  scale_color_identity(name = "Countries",
                       breaks = c("red"),
                       labels = c("Australia"),
                       guide = "legend")

p_4 <- ggplot(italy_data, aes(x = date, y = daily_vaccinations_per_million, group =1))+
  geom_line(aes(color = "blue")) + theme(axis.text.x = element_text(angle = 90)) + xlab("time") + ylab("Daily Vaccinations Per Million") + ggtitle("Daily Vaccinations Per Million against time for a subset of Countries in Europe") + geom_line(data = finland_data, aes(x = date, y = daily_vaccinations_per_million, color = "red"))+
  geom_line(data = poland_data, aes(x = date, y = daily_vaccinations_per_million, color = "brown4"))+
  geom_line(data = norway_data, aes (x = date, y = daily_vaccinations_per_million, color = "cyan4"))+
  geom_line(data = france_data, aes(x = date, y = daily_vaccinations_per_million, color = "chocolate2"))+
  geom_line(data = germany_data, aes(x = date, y = daily_vaccinations_per_million, color = "chartreuse4"))+
  geom_line(data = iceland_data, aes (x = date, y = daily_vaccinations_per_million, color = "blueviolet")) +
  scale_color_identity(name = "Countries",
                       breaks = c("blue","red", "brown4", "cyan4", "chocolate2", "chartreuse4", "blueviolet"),
                       labels = c("Italy", "Finland", "Poland", "Norway", "France", "Germany", "Iceland"),
                       guide= "legend")