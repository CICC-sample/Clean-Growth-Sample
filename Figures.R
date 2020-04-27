#specify the packages of interest
packages = c("ggplot2","directlabels","reshape", "readxl", "tidytext", "dplyr", "data.table")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded, else installed then loaded.

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

## Figure 1.1 ##
decoupling <- read.csv("E:/Report/Jobs/Decoupling_provinces.csv")
decoupling <- decoupling[Place !="Canada", ]


labels <- c(`0 British Columbia`="British Columbia",
            `1 Alberta`="Alberta",
            `2 Saskatchewan`="Saskatchewan",
            `3 Manitoba`="Manitoba",
            `4 Ontario`="Ontario",
            `5 Quebec` ="Quebec",
            `6 New Brunswick`="New Brunswick",
            `7 Nova Scotia`="Nova Scotia", 
            `8 PEI`="PEI", 
            `9 Newfoundland & Labrador`="Newfoundland & Labrador")

ggplot(decoupling, aes(x=Year), group = Place) + 
  geom_line(aes(y = GDP), size=1, color="darkred") +
  geom_line(aes(y = GHG), size=1, color="steelblue") + 
  facet_wrap(. ~ Place, ncol = 2, labeller = labeller(Place = labels)) +
  theme_bw() +
  scale_x_continuous(name="Year", limits=c(2005, 2017), breaks = c(2005, 2009, 2013, 2017)) +
  theme(panel.spacing.x = unit(7, "mm")) +
  scale_y_continuous(name="Change since 2005", limits=c(60, 135), breaks = c(70, 100, 130)) +
  labs(x="Year", y="Change since 2005") +
  #ggtitle("Figure 1.1: Decoupling GHGs from GDP by province (Index)") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=22,face="bold")) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))

#######
# DECOUPLING EMPLOYMENT
#######

emp_ghg <- read.csv("E:/Report/Jobs/decoupling_employment.csv")
attach(emp_ghg)

emp_ghg <- emp_ghg[Place !="Canada", ]
detach(emp_ghg)
attach(emp_ghg2)

labels <- c(`0 British Columbia`="British Columbia",
            `1 Alberta`="Alberta",
            `2 Saskatchewan`="Saskatchewan",
            `3 Manitoba`="Manitoba",
            `4 Ontario`="Ontario",
            `5 Quebec` ="Quebec",
            `6 New Brunswick`="New Brunswick",
            `7 Nova Scotia`="Nova Scotia", 
            `8 PEI`="PEI", 
            `9 Newfoundland & Labrador`="Newfoundland & Labrador")

ggplot(emp_ghg, aes(x=Year), group = Place) + 
  geom_line(aes(y = Employment), size=1, color="darkred") +
  geom_line(aes(y = GHG), size=1, color="steelblue") + 
  facet_wrap(. ~ Place, ncol = 2, labeller = labeller(Place = labels)) +
  theme_bw() +
  #ggtitle("Figure X: Decoupling GHGs from Employment by province (Index)") +
  scale_x_continuous(name="Year", limits=c(2005, 2017), breaks = c(2005, 2009, 2013, 2017)) +
  theme(panel.spacing.x = unit(7, "mm")) +
  scale_y_continuous(name="Change since 2005", limits=c(60, 135), breaks = c(70, 100, 130)) +
  labs(x="Year", y="Change since 2005") +
  #ggtitle("Figure 1.1: Decoupling Employment from GDP by province (Index)") +
  theme(legend.position = "none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=22,face="bold")) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))


## FIGURE 8.2 ##

aq <- read_excel("C:/.../Health_data_minimal.xlsx", sheet = "Air_Quality")
# aq <- fread("https://raw.githubusercontent.com/CICC-sample/Clean-Growth-Sample/master/data/Fig_8.2_data.csv") #Uncomment to load from URL

aq <- as.data.frame(aq)


# Reformat data from "wide" to "long"
aq2 <- melt(aq, id.vars = c("City", "Jurisdiction"), measure.vars = c(3:6))


# Add two new columns for CAAQS (2020 AND 2025)
aq2 <- aq2 %>%
  mutate(CAAQS_2020 = ifelse(variable == "PM2.5", 8.8, 
                        ifelse(variable == "SO2", 5,
                          ifelse(variable == "NO2", 17.7, NA)))
         ) %>%
  mutate(CAAQS_2025 = ifelse(variable == "SO2", 4,
                              ifelse(variable == "NO2", 12, NA))
         )


# Make faceted bar graph of air quality data by city
ggplot(aq2, aes(x = reorder_within(City, value, variable), y=value, fill = Jurisdiction)) + 
  geom_bar(stat = "identity") +
  facet_wrap(.~ variable, scales = "free", ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  
  geom_hline(aes(yintercept = CAAQS_2020), color = "darkred", size = 2, linetype = "dashed") +
  geom_hline(aes(yintercept = CAAQS_2025), color = "steelblue", size = 2, linetype = "dashed")

