# Data Programming with R
# Collecting COVID data from Our World in Data (OWID, https://ourworldindata.org)

Installed <- TRUE  # For checking if package is installed
toInstall <- c("vroom", "finalfit", "tidyverse", "descr", "RColorBrewer", "scales")
if(Installed){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, require, character.only = TRUE) # call into library

# Reading all real time data
# vroom is the champion in reading github date, < 3 sec.
owidall = vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true")

# Subset by year

owid2022 = subset(owidall, format(as.Date(date),"%Y")==2022)
owid2021 = subset(owidall, format(as.Date(date),"%Y")==2021)
owid2020 = subset(vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"), format(as.Date(date),"%Y")==2020)
owid09292022 = subset(owidall,  date == "2022-09-29")

# Clean up OWID*  cases
# Deselect cases/rows with OWID
owidall = owidall[!grepl("^OWID", owidall$iso_code), ] 
owid2022 = owid2022[!grepl("^OWID", owid2022$iso_code), ] 
owid2021 = owid2021[!grepl("^OWID", owid2021$iso_code), ]
owid2020 = owid2020[!grepl("^OWID", owid2020$iso_code), ]
owidall$location=as.factor(owidall$location)

# Subset by country/region: Taiwan, United States, EU, Asia
owidtw = subset(owidall, location=="Taiwan")
owidus = subset(owidall, location=="United States")
owideu = subset(owidall, continent=="Europe")
owidasia = subset(owidall, continent=="Asia")

# Get today's COVID data
owidtoday = subset(vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"), date == Sys.Date())


owidtwtoday = subset(owid2022, location == "Taiwan" & date >="2023-09-25")
options(scipen=999) # Disable scientific notation
par(family = "Palatino")
format(owideu$new_cases, big.mark = ",", scientific = FALSE)

# Europe data
y = owideu$new_deaths
x = as.Date(owideu$date)
plot(x,y, pch=20, col="#E7298A", cex = .5, xaxt='n', xlab = "Date", ylab = "COVID Deaths in Europe (Daily)")
axis(1, x, format(x, "%Y-%m"), cex.axis = .7, las = 3 , gap.axis =1.5, tick = FALSE)
identify(x,y,owideu$location, ps=8, atpen=TRUE) # Manually identify cases by mouse click

# Asia data
y = owidasia$new_deaths
x = as.Date(owidasia$date)
plot(x,y, pch=20, col="#66A61E",  cex = .5, xaxt='n', xlab = "Date", ylab = "COVID Deaths in Asia (Daily)")
axis(1, x, format(x, "%Y-%m"), cex.axis = .7, las = 3 , gap.axis =1.5, tick = FALSE)
identify(x,y,owidasia$location, ps=8, atpen=TRUE)

# Format date
owidall$date<-as.Date(owidall$date,format="%Y-%m-%d")


options(scipen=999) # No sci notation

# Plot daily new cases over time
format(owidall$new_cases, big.mark = ",", scientific = FALSE)
ggplot(owidall, aes(date, new_cases, color = continent, alpha = new_cases)) + 
  geom_point(size = .8) +
  theme_bw() +
  #  scale_colour_viridis_d(option = "plasma", direction = -1)+
  scale_x_date(breaks = "month", date_labels= "%Y-%m", limits = as.Date(c("2020-02-01","2022-05-22"))) +
  scale_y_continuous(labels = comma) + 
  xlab("Date") +
  ylab("New COVID Cases (Daily)") +
  theme(axis.text.x = element_text(angle = 90), legend.position = c(.1, .8)) +
  guides(alpha = "none") +
  theme(text=element_text(size=9, family="Palatino")) +
  scale_colour_brewer(palette = "Dark2", direction = -1)
# Names of Dark2 palette: 
## [1] "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02" "#A6761D"
## [8] "#666666"
# South America
# North America: 7570B3
# Asia: 66A61E
# Europe: E7298A
# Africa: E6AB02
# Oceania: D95F02