if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rworldmap)) install.packages("rworldmap", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(doMC)) install.packages("doMC", repos="http://R-Forge.R-project.org")


library(tidyverse)
library(caret)
library(lubridate)
library(ggrepel)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(kableExtra)
library(knitr)
library(rworldmap)
library(rattle)
library(rpart)
library(rpart.plot)
library(kernlab)
library(gbm)
library(doMC)


# We set a seed number before beginning the project
set.seed(1111)

# We set decimal limit to 6
options(digits = 6)

# We then download the csv file from the git link
link_datasetchocolate <- "https://raw.githubusercontent.com/sejalarora21/CYO-project-/main/read_csv.csv"

# Next, we read file into raw table and remove non-printable characters
datachocolate <- read.csv(gsub("[^[:print:]]","",link_datasetchocolate),
                           na = c(""," ","NA"))

# Using rworldmap package, we create a Country-Region mapping
data_countryregion <- countryRegions %>% mutate(CountryName = ADMIN, CountryCode = ISO3,GeoRegion = GEO3) %>% filter(!is.na(GeoRegion)) %>% select(CountryName, CountryCode, GeoRegion)


#####################################
# ORIGINAL DATA - INITIAL EXPLORATION 
#####################################

# structure of the dataset
str(datachocolate)

# columns in the dataset
ncol(datachocolate)

# rows in the dataset
nrow(datachocolate)

# missing vales (if any) in the dataset
sum(is.na(datachocolate))

# renaming the columns
name_of_columns <- c("CompanyName", "ChocolateBarName", "Reference", "ReviewYear", "CocoaPercentage", "CompanyCountry", "Rating", "BeanType", "BeanOrigin")

names(datachocolate) <-name_of_columns

# missing values (if any) in each column of the dataset 
missingvalues <- tibble("Column Name" = c("CompanyName", "ChocolateBarName", "Reference", "ReviewYear", "CocoaPercentage", "CompanyCountry", "Rating", "BeanType", "BeanOrigin"),
"Missing Values" = c(sum(is.na(datachocolate$CompanyName)), sum(is.na(datachocolate$ChocolateBarName)), sum(is.na(datachocolate$Reference)), sum(is.na(datachocolate$ReviewYear)), sum(is.na(datachocolate$CocoaPercentage)), sum(is.na(datachocolate$CompanyCountry)), sum(is.na(datachocolate$Rating)), sum(is.na(datachocolate$BeanType)), sum(is.na(datachocolate$BeanOrigin))))

# missing Value counts
missingvalues %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), full_width = FALSE, font_size = 15, position = "center")

# first 6 rows of the dataset
head(datachocolate) %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), full_width = FALSE, font_size = 12, position = "center", latex_options = "scale_down")

#######################################
# CUSTOMIZE AND STANDARDIZE THE DATASET 
#######################################

# Based on country or Sub-Region International Organization for Standardization (ISO) -3166, standardize the BeanOrigin column 
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Africa, Carribean, C. Am."] <- "Western Africa|Caribbean|Meso-America"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Burma"] <- "Myanmar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Carribean"] <- "Caribbean"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Carribean(DR/Jam/Tri)"] <- "Dominican Republic|Jamaica|Trinidad and Tobago"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Central and S. America"] <- "Meso-America|South America"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Colombia, Ecuador"] <- "Colombia|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Congo"] <- "Republic of the Congo"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Cost Rica, Ven"] <- "Costa Rica|Venezuela"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Dom. Rep., Madagascar"] <- "Dominican Republic|Madagascar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Dominican Rep., Bali"] <- "Dominican Republic|Indonesia"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "DR, Ecuador, Peru"] <- "Dominican Republic|Ecuador|Peru"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ecuador, Costa Rica"] <- "Ecuador|Costa Rica"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ecuador, Mad., PNG"] <- "Ecuador|Madagascar|Papua New Guinea"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ghana & Madagascar"] <- "Ghana|Madagascar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ghana, Domin. Rep"] <- "Ghana|Dominican Republic"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ghana, Panama, Ecuador"] <- "Ghana|Panama|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Gre., PNG, Haw., Haiti, Mad"] <- "Grenada|Papua New Guinea|South Pacific|Haiti|Madagascar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Guat., D.R., Peru, Mad., PNG"] <- "Guatemala|Dominican Republic|Peru|Madagascar|Papua New Guinea"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Indonesia, Ghana"] <- "Indonesia|Ghana"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Mad., Java, PNG"] <- "Madagascar|Indonesia|Papua New Guinea"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Madagascar & Ecuador"] <- "Madagascar|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru(SMartin,Pangoa,nacional)"] <- "Peru"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru, Belize"] <- "Peru|Belize"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru, Dom. Rep"] <- "Peru|Dominican Republic"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru, Ecuador"] <- "Peru|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru, Ecuador, Venezuela"] <- "Peru|Ecuador|Venezuela"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru, Mad., Dom. Rep."] <- "Peru|Madagascar|Dominican Republic"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Peru, Madagascar"] <- "Peru|Madagascar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "PNG, Vanuatu, Mad"] <- "Papua New Guinea|Vanuatu|Madagascar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Trinidad, Ecuador"] <- "Trinidad and Tobago|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ven, Bolivia, D.R."] <- "Venezuela|Bolivia|Dominican Republic"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ven, Trinidad, Ecuador"] <- "Venezuela|Trinidad and Tobago|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ven., Indonesia, Ecuad."] <- "Venezuela|Indonesia|Ecuador"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ven., Trinidad, Mad."] <- "Venezuela|Trinidad and Tobago|Madagascar"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Ven.,Ecu.,Peru,Nic."] <- "Venezuela|Ecuador|Peru|Nicaragua"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venez,Africa,Brasil,Peru,Mex"] <- "Venezuela|Western Africa|Brazil|Peru|Mexico"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela, Carribean"] <- "Venezuela|Caribbean"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela, Dom. Rep."] <- "Venezuela|Dominican Republic"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela, Ghana"] <- "Venezuela|Ghana"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela, Java"] <- "Venezuela|Indonesia"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela, Trinidad"] <- "Venezuela|Trinidad and Tobago"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela/ Ghana"] <- "Venezuela|Ghana"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Domincan Republic"] <- "Dominican Republic"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Hawaii"] <- "South Pacific"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Principe"] <- "Sao Tome and Principe"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Sao Tome"] <- "Sao Tome and Principe"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Sao Tome & Principe"] <- "Sao Tome and Principe"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "St. Lucia"] <- "Saint Lucia"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "South America, Africa"] <- "South America|Western Africa"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Tanzania"] <- "United Republic of Tanzania"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Tobago"] <- "Trinidad and Tobago"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Trinidad"] <- "Trinidad and Tobago"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Trinidad, Tobago"] <- "Trinidad and Tobago"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Trinidad-Tobago"] <- "Trinidad and Tobago"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Venezuela"] <- "Venezuela"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Vietnam"] <- "Vietnam"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "Martinique"] <- "Caribbean"
datachocolate$BeanOrigin[datachocolate$BeanOrigin == "West Africa"] <- "Western Africa"

# As per ISO codes, we need to correct the misspelled countries in the 'CompanyCountry' column 
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Amsterdam"] <- "Netherlands"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Domincan Republic"] <- "Dominican Republic"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Eucador"] <- "Ecuador"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Niacragua"] <- "Nicaragua"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "St. Lucia"] <- "Saint Lucia"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Sao Tome"] <- "Sao Tome and Principe"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Scotland"] <- "United Kingdom"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "U.K."] <- "United Kingdom"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "U.S.A."] <- "United States of America"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Venezuela"] <- "Venezuela"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Wales"] <- "United Kingdom"
datachocolate$CompanyCountry[datachocolate$CompanyCountry == "Martinique"] <- "France"


# Next, we need to group the BeanType column on the basis of different species of the bean
criollo <- c("Criollo", "Criollo (Amarru)", "Criollo (Ocumare)", "Criollo (Ocumare 61)", "Criollo (Ocumare 67)", "Criollo (Ocumare 77)", "Criollo (Porcelana)", "Criollo (Wild)")

forastero <- c("Forastero", "Forastero (Amelonado)", "Forastero (Arriba)", "Forastero (Arriba) ASS", "Forastero (Arriba) ASSS", "Forastero (Catongo)", "Forastero (Parazinho)")

nacional <- c("Forastero (Nacional)", "Nacional", "Nacional (Arriba)")

trinitario <- c("Trinitario", "Trinitario (Amelonado)", "Trinitario (Scavina)")

blend <- c("Amazon", "Amazon mix","Amazon, ICS", "Blend", "Blend-Forastero,Criollo", "Criollo, +", "Criollo, Forastero", "Criollo, Trinitario", "Forastero, Trinitario", "Forastero(Arriba, CCN)", "Trinitario (85% Criollo)", "Trinitario, Criollo", "Trinitario, Forastero", "Trinitario, Nacional", "Trinitario, TCGA", "Beniano", "CCN51", "EET", "Matina")

datachocolate$BeanType[which(datachocolate$BeanType %in% criollo)] <- "Criollo"
datachocolate$BeanType[which(datachocolate$BeanType %in% forastero)] <- "Forastero"
datachocolate$BeanType[which(datachocolate$BeanType %in% nacional)] <- "Nacional"
datachocolate$BeanType[which(datachocolate$BeanType %in% trinitario)] <- "Trinitario"
datachocolate$BeanType[which(datachocolate$BeanType %in% blend)] <- "Blend"


# (i) If there are missing values (NA) for multiple countries under the 'BeanOrigin' and 'BeanType' column;
# (ii) If there is 'Blend' or 'blend' or ',' in the 'ChocolateBarName' column and if there are missing values (NA) in the 'BeanType' column, then
# Replace missing value for 'BeanType' column to 'Blend' column.
datachocolate$BeanType[is.na(datachocolate$BeanType) & str_detect(datachocolate$BeanOrigin, "\\|")] <- "Blend"
datachocolate$BeanType[is.na(datachocolate$BeanType) & str_detect(datachocolate$ChocolateBarName, "blend")] <- "Blend"
datachocolate$BeanType[is.na(datachocolate$BeanType) & str_detect(datachocolate$ChocolateBarName, "Blend")] <- "Blend"
datachocolate$BeanType[is.na(datachocolate$BeanType) & str_detect(datachocolate$ChocolateBarName, "\\,")] <- "Blend"
 
# For visualization, we create a new column 'RatingClass'
datachocolate <- datachocolate %>%
mutate(RatingClass = case_when(Rating >= 1.00 & Rating <= 1.75  ~ "10-Unpleasant", Rating >= 2.00 & Rating <= 2.75  ~ "20-Disappointing", Rating >= 3.00 & Rating <= 3.75  ~ "30-Satisfactory",Rating >= 4.00 & Rating <= 4.75  ~ "40-Premium", Rating > 4.75  ~ "50-Elite"))

# Using pipe-separator, we separate 'BeanOrigin' column into rows
datachocolate <- datachocolate %>%
separate_rows(BeanOrigin, sep = "\\|", convert = FALSE)

#Next, we create a new column called 'BeanOriginGeoRegion' by providing the Geo-region based on the 'BeanOrigin' column
datachocolate <- datachocolate %>%
left_join(data_countryregion, by = c("BeanOrigin" = "CountryName")) %>%
mutate(BeanOriginGeoRegion = if_else(condition = is.na(GeoRegion), true = BeanOrigin, false = GeoRegion, missing = BeanOrigin)) %>%
select(- CountryCode, - GeoRegion)

# Now, we create another column 'CompanyGeoRegion' by providing the Geo-region based on the 'CompanyCountry' column
datachocolate <- datachocolate %>%
left_join(data_countryregion, by = c("CompanyCountry" = "CountryName")) %>%
mutate(CompanyGeoRegion = if_else(condition = is.na(GeoRegion), true = CompanyCountry, false = GeoRegion, missing = CompanyCountry)) %>%
select(- CountryCode, - GeoRegion)

# We now convert the 'CocoaPercentage' column to Numeric by removing the % sign and rounding it to nearest integer
datachocolate$CocoaPercentage <- as.numeric(sub("%", "", datachocolate$CocoaPercentage, fixed = TRUE))
datachocolate$CocoaPercentage <- round(datachocolate$CocoaPercentage, digits = 0)

# Next, we convert 'Rating' column to numeric 
datachocolate$Rating <- as.numeric(datachocolate$Rating)

# Further, we convert all other remaining columns to Factor
datachocolate$CompanyName <- as.factor(datachocolate$CompanyName)
datachocolate$CompanyCountry <- as.factor(datachocolate$CompanyCountry)
datachocolate$CompanyGeoRegion <- as.factor(datachocolate$CompanyGeoRegion)
datachocolate$BeanType <- as.factor(datachocolate$BeanType)
datachocolate$BeanOrigin <- as.factor(datachocolate$BeanOrigin)
datachocolate$BeanOriginGeoRegion <- as.factor(datachocolate$BeanOriginGeoRegion)
datachocolate$ReviewYear <- as.factor(datachocolate$ReviewYear)
datachocolate$RatingClass <- as.factor(datachocolate$RatingClass)

# Finally, after ordering the columns and removing the unused ones (like 'Reference'), we create a new clean dataset
cleandata <- datachocolate %>%
select(ChocolateBarName, CompanyName, CompanyCountry, CompanyGeoRegion, BeanType, BeanOrigin, BeanOriginGeoRegion, CocoaPercentage, ReviewYear, Rating, RatingClass)

# cleandata example
head(cleandata) %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), full_width = FALSE, font_size = 15, position = "center", latex_options = "scale_down")


#######################################
# FINAL DATA STRUCTURE
#######################################

# Cleandata data structure
str(cleandata)

# Number of columns in cleandata Dataset
ncol(cleandata)

# Number of rows in cleandata Dataset
nrow(cleandata)


# Structure of cleandata dataset
dataset <- tibble("Feature Name" = c("ChocolateBarName", "CompanyName", "CompanyCountry", "CompanyGeoRegion", "BeanType", "BeanOrigin", "BeanOriginGeoRegion", "CocoaPercentage", "ReviewYear", "Rating", "RatingClass"),
"Data Type" = c(class(cleandata$ChocolateBarName), class(cleandata$CompanyName), class(cleandata$CompanyCountry), class(cleandata$CompanyGeoRegion), class(cleandata$BeanType), class(cleandata$BeanOrigin), class(cleandata$BeanOriginGeoRegion), class(cleandata$CocoaPercentage), class(cleandata$ReviewYear), class(cleandata$Rating), class(cleandata$RatingClass)),
"Distinct Values" = c(n_distinct(cleandata$ChocolateBarName), n_distinct(cleandata$CompanyName), n_distinct(cleandata$CompanyCountry), n_distinct(cleandata$CompanyGeoRegion), n_distinct(cleandata$BeanType), n_distinct(cleandata$BeanOrigin), n_distinct(cleandata$BeanOriginGeoRegion), n_distinct(cleandata$CocoaPercentage), n_distinct(cleandata$ReviewYear), n_distinct(cleandata$Rating), n_distinct(cleandata$RatingClass)),
"Missing Values" = c(sum(is.na(cleandata$ChocolateBarName)), sum(is.na(cleandata$CompanyName)), sum(is.na(cleandata$CompanyCountry)), sum(is.na(cleandata$CompanyGeoRegion)), sum(is.na(cleandata$BeanType)), sum(is.na(cleandata$BeanOrigin)), sum(is.na(cleandata$BeanOriginGeoRegion)), sum(is.na(cleandata$CocoaPercentage)), sum(is.na(cleandata$ReviewYear)), sum(is.na(cleandata$Rating)), sum(is.na(cleandata$RatingClass))))

# Missing values
dataset %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), full_width = FALSE, font_size = 15, position = "center")



###############################################
# EXPLORATIOON
###############################################

# Distribution of overall Chocolate Bar Ratings
ratings <- mean(cleandata$Rating)

cleandata %>%
ggplot(aes(Rating)) +
geom_histogram(color = "black", fill = "yellow", binwidth = 0.2) +
geom_vline(xintercept = ratings, col = "black",linetype = "dotted") +
labs(title = "Distribution of Chocolate Rating System", x = "Rating", y = "Frequency")+
theme_classic()+
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5))



# Distribution of Chocolate Bar Rating Class 
cleandata %>%
ggplot(aes(RatingClass)) +
geom_bar(color = "black", fill = "pink") +
labs(title = "Distribution of Chocolate Bar Rating Class",x = "Scale of Rating",y = "Frequency") +
theme_classic()+
theme(plot.title = element_text(size = 13, color = "black", hjust = 0.5),
axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5))

# Distribution of Chocolate Bar Ratings by percentage of cocoa
cocoapercent <- mean(cleandata$CocoaPercentage)

cleandata %>%
ggplot(aes(CocoaPercentage)) +
geom_histogram(color = "black", fill = "lightgreen", binwidth = 0.75) +
geom_vline(xintercept = cocoapercent, col = "black", linetype = "dotted") +
labs(title = "Distribution of Cocoa Percentage", x = "Cocoa Percentage", y = "Frequency") +
theme_classic() +
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5))

# To check the presence of any correlation between Chocolate Bar Ratings and percentage of cocoa
cleandata %>%
ggplot(aes(y = Rating, x = CocoaPercentage)) +
geom_jitter(color = "brown") +
geom_smooth(method = "lm") +
labs(title = "Correlation - Chocolate Bar Rating and Cocoa Percentage", x = "Cocoa Percentage",y = "Rating") +
theme_classic() +
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5))

# Distribution of Chocolate Bar Ratings on the basis of Bean Type
cleandata%>%
filter(!is.na(BeanType)) %>%
group_by(BeanType) %>%
summarize(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
ggplot(aes(x = reorder(BeanType, Rating_Count), y = Rating_Count)) +
geom_bar(stat = "identity", color = "black", fill = "darkblue") +
labs(title = "Chocolate Bar Rating Distribution By Bean Type", x = "Bean Type", y = "Frequency") +
theme_classic() +
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5), axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5))

# Distribution of Chocolate Bar Ratings on the basis of Bean Type
cleandata %>%
filter(!is.na(BeanType)) %>%
ggplot(aes(x = BeanType, y = Rating)) +
geom_boxplot(color = "black", fill = "lightblue") +
labs(title = "Chocolate Bar Rating Distribution By Bean Type", x = "Bean Type", y = "Rating") +
theme_classic() +
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5))

# Chocolate Bar Rating Average on the basis of Bean Origin [Country]
BeanOriginMap <- cleandata %>%
filter(!is.na(BeanOrigin)) %>%
group_by(BeanOrigin) %>%
summarize(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
joinCountryData2Map(joinCode="NAME", nameJoinColumn="BeanOrigin",verbose = FALSE)

mapCountryData(mapToPlot = BeanOriginMap, nameColumnToPlot="Rating_Average", oceanCol = 'lightblue', missingCountryCol = 'white', borderCol = 'darkgrey', colourPalette = "terrain",  mapTitle = "Chocolate Bar Rating Average By Bean Origin (Country)", catMethod = "fixedWidth")

# Top 15 rankings of the Chocolate Bar Rating Average on the basis of Bean Origin [Country]
cleandata%>%
filter(!is.na(BeanOrigin)) %>%
group_by(BeanOrigin) %>%
summarize(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
filter(Rating_Count >= 10) %>%
arrange(desc(Rating_Average)) %>%
head(15) %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"),font_size = 20, position = "center", full_width = FALSE)

# Distribution of Chocolate Bar Ratings on the basis of Review Year
cleandata %>%
filter(!is.na(ReviewYear)) %>%
ggplot(aes(x = ReviewYear,y = Rating)) +
geom_boxplot(color = "black", fill = "pink") +
labs(title = "Chocolate Bar Rating Distribution on the basis of Review Year", x = "Review Year", y = "Rating") +
theme_classic() +
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5))

# Top 15 Chocolate Bar Rating Average by Company Name 
cleandata %>%
group_by(CompanyName, CompanyCountry) %>%
summarize(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
filter(Rating_Count >= 10) %>%
arrange(desc(Rating_Average)) %>%
head(15) %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"),font_size = 20, position = "center", full_width = FALSE)

# Distribution of Chocolate Bar Rating Average on the basis of location of the company [Country]
cleandata%>%
group_by(CompanyCountry) %>%
summarise(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
arrange(desc(Rating_Average)) %>%
ggplot(aes(y = Rating_Average,x = reorder(CompanyCountry, Rating_Average))) +
geom_point(aes(size = Rating_Count,colour = factor(Rating_Average)),alpha = 0.5) +
labs(title = "Chocolate Bar Rating Average Distribution on the basis of Company Location [country]",x = "Country",y = "Rating Average") +
theme_classic() +
theme(plot.title = element_text(size = 10, color = "black", hjust = 0.5),
axis.text.x = element_text(size = 7, angle = 90, hjust = 1),legend.position="none")

# Distribution of Chocolate Bar Ratings Average on the basis of location of the company [Country]
CompanyCountryMap <- cleandata %>%
group_by(CompanyCountry) %>%
summarise(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
joinCountryData2Map(joinCode="NAME", nameJoinColumn="CompanyCountry", verbose = FALSE)


mapCountryData(mapToPlot = CompanyCountryMap, nameColumnToPlot="Rating_Average", oceanCol = 'lightblue', borderCol = 'white', colourPalette = "terrain", mapTitle = "Chocolate Bar Rating Distribution on the basis of Company Location [country]", catMethod = "fixedWidth")

# Top 15 rankings of the Chocolate Bar Rating Average on the basis of Company Geo-Region
cleandata %>%
filter(!is.na(CompanyCountry)) %>%
group_by(CompanyCountry) %>%
summarize(Rating_Count = n(), Rating_Average = mean(Rating)) %>%
filter(Rating_Count >= 10) %>%
arrange(desc(Rating_Average)) %>%
head(15) %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"),font_size = 20, position = "center", full_width = FALSE)


############################################################
#  FILTERING AND CREATING THE FINAL DATASET 
############################################################

# Create final Dataset
finaldataset <- cleandata %>%
select(CocoaPercentage, BeanType, BeanOrigin, CompanyCountry, ReviewYear,RatingClass) %>% 
drop_na()

###########################################################################
# SPLITTING THE DATASET INTO TRAINING SET (70%) AND VALIDATION SET (30%)
###########################################################################

# First we create Data Partition Index
set.seed(1111)
indexsample <- createDataPartition(y = finaldataset$RatingClass, times = 1, p = 0.7, list = FALSE)

# Next, we create Training set
trainingset <- finaldataset[indexsample, ]

# Now, we create the Validation set
validationset <- finaldataset[-indexsample, ]

# Finally, we remove unwanted data
rm(datachocolate,indexsample)



#########################################################################
# TRAINING AND VALIDATION
#########################################################################

# 011-01 Configure the number of K-folds for cross validation (Repeated CV)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#########################################################################
# METHOD 1
# SUPPORT VECTOR MACHINE
#########################################################################

method1 <- "SVM"
method1d <- "Support Vector Machine"

# Train on training set
set.seed(1111)
method1train <- train(RatingClass ~ ., data = trainingset, trControl = control, method = "svmRadial")

# Results on the training set
method1results <- method1train$results

method1results %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"),font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure on the training set
accuracy_method1train <- max(method1train$results["Accuracy"])

# Prediction on training set
predict_method1 <- predict(method1train, newdata = validationset)

# Confusion Matrix
confusionmatrix_method1 <- confusionMatrix(predict_method1, validationset$RatingClass)

# Results of final model on Validation set
predictresults_method1 <- confusionmatrix_method1$overall

predictresults_method1 %>%
kable(col.names = c("Measure Value")) %>%
kable_styling(bootstrap_options = ("bordered"),font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure from the Model on Validation set
predictaccuracy_method1 <- predictresults_method1["Accuracy"]

# We create a table to record our approaches and the measure
finalresult_method1 <- tibble(ModelID = method1,
                         ModelMethod = method1d,
                         AccuracyOnTraining = accuracy_method1train,
                         AccuracyOnValidation = predictaccuracy_method1)

# Next, we create a table to record the results
summaryresult <- finalresult_method1

# Finally, we display the summary
summaryresult %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"),font_size = 20, position = "center", full_width = FALSE) %>%
column_spec(1, width = "5em") %>%
column_spec(2, width = "20em") %>%
column_spec(4, bold = TRUE)



#########################################################################
# METHOD 2
# RANDOM FOREST 
#########################################################################

method2 <- "RF"
method2d <- "Random Forest"

# Train on training set
set.seed(1111)
method2train <- train(RatingClass ~ ., data = trainingset, trControl = control, method = "rf")

# Results on the training set
method2results <- method2train$results

method2results %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure on the training set
accuracy_method2train <- max(method2train$results["Accuracy"])


# Prediction on training set
predict_method2 <- predict(method2train, newdata = validationset)

# Confusion Matrix
confusionmatrix_method2 <- confusionMatrix(predict_method2, validationset$RatingClass)

# Results of final model on Validation set
predictresults_method2 <- confusionmatrix_method2$overall

predictresults_method2 %>%
kable(col.names = c("Measure Value")) %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)


# Best Accuracy Measure from the Model on Validation set
predictaccuracy_method2 <- predictresults_method2["Accuracy"]

# We create a table to record our approaches and the measure
finalresult_method2 <- tibble(ModelID = method2,
                         ModelMethod = method2d,
                         AccuracyOnTraining = accuracy_method2train,
                         AccuracyOnValidation = predictaccuracy_method2)

# Next, we create a table to record the results
summaryresult <- bind_rows(summaryresult, finalresult_method2)

# Finally, we display the summary
summaryresult %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE) %>%
column_spec(1, width = "5em") %>%
column_spec(2, width = "20em") %>%
column_spec(4, bold = TRUE)




#########################################################################
# METHOD 3
# LEARNING VECTOR QUANTIZATION (LVQ)
#########################################################################

method3 <- "LVQ"
method3d <- "Learning Vector Quantization"

# Train on training set
set.seed(1111)
method3train <- train(RatingClass ~ ., data = trainingset, trControl = control, method = "lvq")

# Results on the training set
method3results <- method3train$results

method3results %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure on the training set
accuracy_method3train <- max(method3train$results["Accuracy"])


# Prediction on training set
predict_method3 <- predict(method3train, newdata = validationset)

# Confusion Matrix
confusionmatrix_method3 <- confusionMatrix(predict_method3, validationset$RatingClass)

# Results of final model on Validation set
predictresults_method3 <- confusionmatrix_method3$overall

predictresults_method3 %>%
kable(col.names = c("Measure Value")) %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure from the Model on Validation set
predictaccuracy_method3 <- predictresults_method3["Accuracy"]

# We create a table to record our approaches and the measure
finalresult_method3 <- tibble(ModelID = method3,
                         ModelMethod = method3d,
                         AccuracyOnTraining = accuracy_method3train,
                         AccuracyOnValidation = predictaccuracy_method3)

# Next, we create a table to record the results
summaryresult <- bind_rows(summaryresult, finalresult_method3)

# Finally, we display the summary
summaryresult %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)%>%
column_spec(1, width = "5em") %>%
column_spec(2, width = "20em") %>%
column_spec(4, bold = TRUE)




#########################################################################
# METHOD 4
# STOCHASTIC GRADIENT BOOSTING MACHINE (GBM)
#########################################################################

method4 <- "GBM"
method4d <- "Stochastic Gradient Boosting Machine"

# Train on training set
set.seed(1111)
method4train <- train(RatingClass ~ ., data = trainingset, trControl = control, method = "gbm", verbose = FALSE)

# Results on the training set
method4results <- method4train$results

method4results %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure on the training set
accuracy_method4train <- max(method4train$results["Accuracy"])

# Prediction on training set
predict_method4 <- predict(method4train, newdata = validationset)

# Confusion Matrix
confusionmatrix_method4 <- confusionMatrix(predict_method4, validationset$RatingClass)

# Results of final model on Validation set
predictresults_method4 <- confusionmatrix_method4$overall

predictresults_method4 %>%
kable(col.names = c("Measure Value")) %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE)

# Best Accuracy Measure from the Model on Validation set
predictaccuracy_method4 <- predictresults_method4["Accuracy"]

# We create a table to record our approaches and the measure
finalresult_method4 <- tibble(ModelID = method4,
                         ModelMethod = method4d,
                         AccuracyOnTraining = accuracy_method4train,
                         AccuracyOnValidation = predictaccuracy_method4)

# Next, we create a table to record the results
summaryresult <- bind_rows(summaryresult,finalresult_method4)

# Finally, we display the summary
summaryresult %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE) %>%
column_spec(1, width = "5em") %>%
column_spec(2, width = "20em") %>%
column_spec(4, bold = TRUE)




###############################################
# RESULTS SUMMARY
###############################################

# Result Summary
summaryresult %>%
arrange(desc(AccuracyOnValidation), desc(AccuracyOnTraining)) %>%
kable() %>%
kable_styling(bootstrap_options = ("bordered"), font_size = 20, position = "center", full_width = FALSE) %>%
column_spec(1, width = "5em") %>%
column_spec(2, width = "20em") %>%
column_spec(4, bold = TRUE)





