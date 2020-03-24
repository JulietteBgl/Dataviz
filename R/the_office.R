
# Libraries ---------------------------------------------------------------

library("schrute")
library("tidytext")
library("data.table") # dataset manipulation

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-03-17')
office_ratings <- tuesdata$office_ratings

office <- theoffice
setDT(office)

reg_cast <- c("Michael","Dwight","Jim","Pam","Ryan","Andy","Robert","Stanley",'Kevin','Meredith',
              "Angela", "Oscar","Phyllis","Roy","Jan","Kelly","Toby","Creed","Darryl","Erin","Gabe",
              "Holly","Nellie","Clark","Pete")

gender <- c(rep("man",3), "woman", rep("man",5), rep("woman",2), "man", "woman", "man", rep("woman",2),
            rep("man",3), "woman", "man", rep("woman",2),rep("man",2))

char <- data.table(character = reg_cast, gender)

office_filtered <- office[character %in% reg_cast]
length(reg_cast) == length(unique(office_filtered$character))

office_filtered <- merge(office_filtered, 
      char, 
      all.x = TRUE)


