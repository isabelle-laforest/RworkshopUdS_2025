###creating different graphs in GGplot2###

#The purpose  of this section is to learn how to 
#build graphs in GGplot 2. We will start by loading 
#the necessary data and libraries

###########
###Setup###
###########

#Install Packages if you have not already
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readxl")

#Load libraries once packages are installed 
library("tidyverse")
library("ggplot2")
library("readxl") 

#Set your working directory if you have not yet done so
setwd("~/your_file_name")

#Load the data 
tomato <- read_excel("Rocio_tomato_modified.xlsx")

#################################
###View, edit, select the data###
#################################

#before building graphs it is important to first understand the data
#lets take a look at the data and see if names need to be changed and 
#if there is missing data 

#check what format your data is in and change it to a data frame for graphing
tomato<-as.data.frame(tomato)

#use the glimpse function to understand the format the data is in
glimpse(tomato)

#it looks like we have  names, values, and some formatting that needs to be changed  

#Lets start with column names. create list of column names
names(tomato)

#there are names that should be changed, some are misspelled
#and some are in a format r has trouble with. specifically 
#it is important to not use spaces in your data

#rename column names#
#we can rename the data by piping and using the "rename" function
#for names that have spaces you can use  `name space` to show 
#it is one unit.

tomato <- tomato %>%
  rename(total_area = `total area`) 

tomato <- tomato %>%
  rename(chlorotic_area = `chlorotic area`,
         Treatment = `Treatement`
  ) 

#look at column names and see if you like your corrections
names(tomato)

#Now lets take a look at individual column values
tomato$Treatment

#For large data sets, looking at every value is not always useful. 
#lets just look at all the unique values in the column. we can do 
# this in tidyverse by using the distinct function take a look at 
#all the character columns 

tomato %>%
  distinct(Infection) 

#it looks like there are spelling errors in the columns. we can rename them using 
#two functions mutate and str_replace
tomato %>%
  mutate(Infection = str_replace(Infection, "pathogn", "pathogen")) 


#take another look at distinct values to make sure you are happy with it. 
tomato %>%
  distinct(Infection) 

#Lets Use the glimpse function again 
glimpse(tomato)

#it looks like the rep column is considered to be numeric (dbl). lets change 
#this to character (chr). we can do this using mutate

tomato %>%
  mutate(rep = as.character(rep))

#Sometimes you may only want to make a graph using a smaller part of the 
#your data. you can filter your data and make a new table with only the values 
#needed. For example. lets make a new table the only contains rep 1 and 3
#to do this we can use the filter function. remember "==" means equal and "!="
#means dose not equal 

tomato_filtered <- tomato %>% 
  filter(Treatment == "mock")

df_filtered <- tomato %>%
  filter(rep != "2" & rep != "4") 

#############################
###Create Plots in GGplot2###
#############################

#Change data to dataframe
tomato<-as.data.frame(tomato)

#make sure columns with numerical values are considered numeric

#In this intro to GGplot2 we will look at mapping, layers, and scales. you
#map variables to graphics and you add on layers to the map to adjust graphics, 
#you can then add in scales to further manipulate your graphic
#We will map or select the data using aes(). Then we will select the type of graph we
#want using using "geom_" for example: geom_histogram; geom_point; geom_boxplot  
#geom_col; geom__line). We will make 4 graphs, for each one we will learn a new syntax, 
#slowly adding layers to the graph

###Make a simple histogram
#use the function geom_histogram
ggplot(tomato, aes(x = chlorotic_area)) + 
  geom_histogram()

### Make a scaterplot, 
#Use the function geom_point
#selecting color using color = in geom_point()

ggplot(tomato, aes(x = total_area, y = chlorotic_area)) + 
  geom_point(color = "blue") 

#Make a boxplot, 
#Use the function geom_boxplot
#selecting color using "color =" and "fill =" in geom_boxplot()
#Add labels and titles using labs(title="name", x ="name", y="name") 


ggplot(tomato, aes(x = Treatment, y = total_area)) +
  geom_boxplot(color="green", fill = "blue") + 
  labs(title = "Boxplot", x = "Treatment", y = "Total Area") 

#Make a Barplot 
#Use the function geom_col()
#selecting color using "fill =" aes()  
#Add labels and titles using labs(title="name", x ="name", y="name") 
#adding groups using fill=


ggplot(tomato, aes(x = Treatment, y = total_area, fill = Infection)) +
  geom_col() + 
  labs(title = "Boxplot", x = "Treatment", y = "Total Area") 

#Make the graph again, but this time lets place the bars next to each other, 
#geom_col(position = "dodge")

ggplot(tomato, aes(x = Treatment, y = total_area, fill = Infection)) +
  geom_col(position = "dodge") + 
  labs(title = "Boxplot", x = "Treatment", y = "Total Area") 


###lineplot, 
#geom__line is the function used to make line graphs. 
#lets make the x axis the rownames of our table
#select color using "color =" in aes ()
#create groups using "group =" in aes()
#Add labels and titles using labs(title="name", x ="name", y="name") 

ggplot(tomato, aes(x = rownames(tomato), y = total_area, group = Treatment, color = Treatment )) + 
  geom_line() + 
  labs(title = "Lineplot", x = "Treatment", y = "Total Area") 

#That graph dose not look good, lets change the order of the x axis using scale_x_discrete. we want R to consider 
#rownames as numeric for this so lets tell R that this data should be numeric

ggplot(tomato, aes(x = rownames(tomato), y = total_area, group = Treatment, color = Treatment)) +
  geom_line() +
  scale_x_discrete(limits = sort(as.numeric(rownames(tomato)))) +  
  labs(title = "Lineplot with Reordered Row Names", x = "Row Names", y = "Total Area")

###Jitter
#in this final section we will add all data points to a box plot using geom_jitter

#Boxplot using geom_jitter, 
#see what happens when you place jitter before and after geom_boxplot   

ggplot(tomato, aes(x = Treatment, y = total_area, fill=Infection)) +
  geom_boxplot() + 
  geom_jitter(color="blue") +  
  labs(title = "Boxplot", x = "Treatment", y = "Total Area") 

###Challenge###
#Try to create all 4 graphs using different values for x, y and group
#Select a new dataset or use your own data and create a graph














