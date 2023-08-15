library("tidyverse")
library("shiny")
library("sf")
library(tigris)
library(DT)

collegegrads <- read_csv("C:/Users/Matthew Afoakwah/Downloads/collegecompletion/cc_state_sector_grads.csv")

#each column name
colnames(collegegrads)

#each columns data type
sapply(collegegrads, class)

#create new table with certain columns
colgrads <- collegegrads %>%
  select(stateid, state_abbr,control,year,gender,race, cohort, grad_cohort)

colgrads %>%
  datatable()


sapply(colgrads, unique)

colgrads$race <- recode(colgrads$race, 'A' = 'Asian', 'Ai' = 'American Indian', 'B' = 'Black', 'H' = 'Hispanic', 'W' = 'White', 'X' = 'Pacific Islander' )

#renaming the 1st column
colnames(colgrads)[1] <- "State"

US <- states(cb = TRUE, class = "sf")

unique(colgrads$cohort)


ggplot()+
  geom_sf(data = US) +
  coord_sf()

x <- colgrads %>%
  group_by(State, year, cohort)%>%
  summarise(dif = n())


Gradmap <- ggplot(data = colgrads, aes( x = State, y = race, color = State)) + geom_bar(stat = "identity") + coord_quickmap()

Gradmap                                          

  
boxplot(data = colgrads, aes(x = State, y = grad_cohort))        


library(maps)
us <- map_data("state")
us
USMap <- ggplot() + geom_polygon(data = us, aes( x = long, y = lat, group = group), color = "black", fill = "white" )

ggplot(data = USMap) + geom_contour(data = colgrads, aes(x = State, y = grad_cohort, color = State)) 

library(usmap)
states <- colgrads$State_abbr
colgrads$fips <- fips(colgrads$State)
US<- plot_usmap(data = colgrads, values = 'grad_cohort')


grads_by_state <- colgrads %>%
  select(State, state_abbr, grad_cohort)%>%
  group_by(State)%>%
  summarise(Gaduates = sum(grad_cohort))

grads_by_state$fips <- fips(grads_by_state$State)
M <- plot_usmap(data = grads_by_state, regions = "states" ,values = 'Gaduates')
a