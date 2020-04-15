# Load in libraries

library(ggplot2)
library(dplyr)

# Load data directly from github
df <- read.csv("https://raw.githubusercontent.com/CICC-sample/Example_project/master/GDP_Pollutant_Index.csv?token=APGIFINNNF3P3UBKPPEU4OK6S43BS")

# Reformat data from "wide" to "long"
df2 <- melt(df, id.vars = "Index", measure.vars = c(2:6))

# Make line graph of indicies
ggplot(df2, aes(x=Index, y=value, col=variable)) + 
  geom_line(size = 2) +
  theme_classic()

 
