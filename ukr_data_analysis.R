################################################################################
############# Header of code file. Adding all needed libraries #################
################################################################################

library(readr)
library(dplyr)

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

urban_rural_area %>%
  filter(str_detect(`Statistical unit`, "attendance"))%>%
  select_if(~ length(unique(.)) > 1) 



View(urban_rural_area)


