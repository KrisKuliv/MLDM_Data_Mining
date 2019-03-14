################################################################################
############# Header of code file. Adding all needed libraries #################
################################################################################

library(readr)
library(dplyr)
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("plotly", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
################################################################################
######################## Import educational dataset  ###########################
################################################################################

education_UKR_2012_2018 <- read_csv("education_UKR_2012-2018.csv")
class(education_UKR_2012_2018) #check the class of data we get
education_UKR_2012_2018 = as.data.frame(education_UKR_2012_2018) #make all data to be a data.frame
class(education_UKR_2012_2018)


################################################################################
######################### Deal with missing values  ############################
################################################################################
numb_rows <- nrow(education_UKR_2012_2018)
numb_complete_rows <- sum(complete.cases(education_UKR_2012_2018))
numb_rows # there are 3318 rows in the dataset
numb_complete_rows # but only 47 rows with no NA values!
1 - numb_complete_rows/numb_rows # so, if we are going to drop all missing values we'll loose 98% of information, that is not possible
numb_cols <-ncol(education_UKR_2012_2018)
numb_cols
################################################################################
############################ Data understanding  ###############################
################################################################################

# first I want to understand what are the unique statistical units of each of 30 columns that I can analyse. 
list_of_unique_values<- list(1:numb_cols)
list_of_unique_values
for (name in 1:numb_cols) {
  unique_value<-education_UKR_2012_2018[name]%>%
     unique()
  list_of_unique_values[[name]]<- unique_value
}

View(list_of_unique_values)

# After checking the unique values we can conclude that columns "Level of educational attainment", 
# "School subject", "Teaching experience", "Type of contract", "Reference area" and "Time period" are redundant, so we can drop them

education_UKR_2012_2018 <- select (education_UKR_2012_2018,-c(`Level of educational attainment`, 
                                                              `School subject`, 
                                                              `Teaching experience`, 
                                                              `Type of contract`,
                                                              `Reference area`, 
                                                              `Time Period`))
View(education_UKR_2012_2018)

################################################################################
#### Question 1: Discover attendance patterns for urban and rural locations ####
################################################################################

# we want get information that is assosiated with Rural and urban location grouped by wealth level
urban_rural_area <- filter(education_UKR_2012_2018, ( 
                                                  ((`Location` == "RUR:Rural") | (`Location` == "URB:Urban"))
                                                  #  & (`Unit of measure` == "PT:Percentage") 
                                                  #  & (`Sex` !="_T:Total")
                                                  # & (`Grade` !="_T:Total")
                                                  # & (`Wealth quintile` !="_T:Total")
                                                   )
                         )
View(urban_rural_area)

##################################################
########### Males in Rural area   ################
##################################################

# Here I select data about males in Rural area
rural_area_male <-urban_rural_area %>%
  filter(`Sex`=="M:Male")%>%
  filter(`Statistical unit`=="NAR:Net attendance rate")%>%
  filter(str_detect(`Unit of measure`, "Percentage"))%>%
  filter(str_detect(`Location`, "Rural"))%>%
  select_if(~ length(unique(.)) > 1) 

# Create a general variable  - concatanation of education and wealth
rural_area_male$`Educ_wealth_male` <- paste(rural_area_male$`Level of education`,"_",rural_area_male$`Wealth quintile`)

# delete variables education and wealth
rural_area_male <- select(rural_area_male,-c(1,2))

# Order variables
rural_area_male <- rural_area_male[order(rural_area_male$`Educ_wealth_male`),]

#Swap data
rural_area_male <-rural_area_male[ ,c(2,1)]

##################################################
########### Females in Rural area   ################
##################################################

# Here I select data about females in Rural area
rural_area_female <-urban_rural_area %>%
  filter(`Sex`=="F:Female")%>%
  filter(`Statistical unit`=="NAR:Net attendance rate")%>%
  filter(str_detect(`Unit of measure`, "Percentage"))%>%
  filter(str_detect(`Location`, "Rural"))%>%
  select_if(~ length(unique(.)) > 1) 

# Create a general variable  - concatanation of education and wealth
rural_area_female$`Educ_wealth_female` <- paste(rural_area_female$`Level of education`,"_",rural_area_female$`Wealth quintile`)

# delete variables education and wealth
rural_area_female <-select(rural_area_female,-c(1,2))

# Order variables
rural_area_female <- rural_area_female[order(rural_area_female$`Educ_wealth_female`),]

#Swap data
rural_area_female <-rural_area_female[,c(2,1)]
rural_area_female

##################################################
########### Males in Urban area   ################
##################################################

# Here I select data about males in Rural area
urban_area_male <-urban_rural_area %>%
  filter(`Sex`=="M:Male")%>%
  filter(`Statistical unit`=="NAR:Net attendance rate")%>%
  filter(str_detect(`Unit of measure`, "Percentage"))%>%
  filter(str_detect(`Location`, "Urban"))%>%
  select_if(~ length(unique(.)) > 1) 


# Create a general variable  - concatanation of education and wealth
urban_area_male$`Educ_wealth_male` <- paste(urban_area_male$`Level of education`,"_",urban_area_male$`Wealth quintile`)

# delete variables education and wealth
urban_area_male <- select(urban_area_male,-c(1,2))

# Order variables
urban_area_male <- urban_area_male[order(urban_area_male$`Educ_wealth_male`),]

#Swap data
urban_area_male <-urban_area_male[,c(2,1)]
urban_area_male

##################################################
########### Females in Urban area   ################
##################################################

# Here I select data about females in Rural area
urban_area_female <-urban_rural_area %>%
  filter(`Sex`=="F:Female")%>%
  filter(`Statistical unit`=="NAR:Net attendance rate")%>%
  filter(str_detect(`Unit of measure`, "Percentage"))%>%
  filter(str_detect(`Location`, "Urban"))%>%
  select_if(~ length(unique(.)) > 1) 

# Create a general variable  - concatanation of education and wealth
urban_area_female$`Educ_wealth_female` <- paste(urban_area_female$`Level of education`,"_",urban_area_female$`Wealth quintile`)

# delete variables education and wealth
urban_area_female <-select(urban_area_female,-c(1,2))

# Order variables
urban_area_female <- urban_area_female[order(urban_area_female$`Educ_wealth_female`),]

#Swap data
urban_area_female <-urban_area_female[,c(2,1)]
urban_area_female

# Create new data frame that combine male and female in urban and rural data 
urban_rural_area_male_female <-urban_area_male
urban_rural_area_male_female <-cbind(urban_rural_area_male_female, rural_area_male[,2])
urban_rural_area_male_female <-cbind(urban_rural_area_male_female, urban_area_female[,2])
urban_rural_area_male_female <-cbind(urban_rural_area_male_female, rural_area_female[,2])

urban_rural_area_male_female

colnames(urban_rural_area_male_female) <- c("Educ_wealth", "2013-male-urban", "2013-male-rural","2013-female-urban" , "2013-female-rural")
View(urban_rural_area_male_female)

# Plot male and female in urban and rural data 
p <- plot_ly(urban_rural_area_male_female, x = ~`Educ_wealth`, y = ~`2013-male-urban`, type = 'bar', name = 'Males in urban area') %>%
  add_trace(y = ~`2013-male-rural`, name = 'Males in rural area') %>%
  #add_trace(y = ~`2013-female-urban`, name = 'Females in urban area') %>%
  #add_trace(y = ~`2013-female-rural`, name = 'Females in rural area') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p




