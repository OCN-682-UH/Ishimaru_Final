#MBIO 612: Final Project Test

library(shiny) #to create the shiny app
library(rsconnect) #to publish the shiny app
library(bslib) #to create the pages in shiny 
library(tidyverse) #for general data manipulation 
library(stringr) #to fix capitalization 
library(here) #for unbreakable file paths
library(leaflet) #to create interactive map
library(gganimate) #to create animated plots
library(rcartocolor) #for a fun + colorblind friendly palette 
library(lubridate) #for working with dates
library(kableExtra) #for creating nice tables

# Read-in Data
raw_data <- read.csv(here("final_proj", "data", "manta-tow-by-reef.csv")) #read in data from AIMS
data_dict <- read.csv(here("final_proj", "data", "data_dict.csv")) #read in data dictionary 

# Data Manipulation
cots_data <- raw_data %>% #pull in raw data
  mutate(Year= year(ymd(SAMPLE_DATE))) %>% #create column containing year that survey was conducted
  mutate(MEAN_SOFT_CORAL= ifelse(is.na(MEAN_SOFT_CORAL), 0, MEAN_SOFT_CORAL)) %>% #make soft coral with values of NA 0 (for calculating total live coral)
  mutate(live_coral_tot= MEAN_LIVE_CORAL + MEAN_SOFT_CORAL) %>% #calculate total live coral
  rename(Site= SECTOR, Reef= REEF_NAME) %>% #alter names (make them look nice)
  mutate(Reef= str_to_title(Reef)) %>% #fix values within reef column (reef names) so only the first letter is capitalized
  mutate(Site= recode(Site, #fix site names so the whole name is displayed instead of the abbreviation 
                      "CA"= "Cairns",
                      "CB"= "Capricorn-Bunker",
                      "CG"= "Cape Grenville",
                      "CL"= "Cooktown-Lizard Island",
                      "CU"= "Cape Upstart",
                      "IN"= "Innisfail",
                      "PC"= "Princess Charlotte Bay",
                      "PO"= "Pompey",
                      "SW"= "Swain",
                      "TO"= "Townsville",
                      "TS"= "Torres Strait",
                      "WH"= "Whitsunday"))
  
pal <- carto_pal(11, "Safe") #create color palette for leaflet map and animated bar plot

sites <- cots_data %>% #pull in data, sites makes a dataframe with one instance per site and will be used to create the leaflet map
  group_by(Site, Reef) %>% #group data by site
  filter(row_number()==1) #one row per site (for labeling leaflet map)

coral_data <- cots_data %>%
  select(Site, Year, live_coral_tot) %>%
  group_by(Site, Year) %>%
  summarise(live_coral_tot= mean(live_coral_tot))

# Build Shiny App!
 ui <- page_navbar( #create navigation bar (thats different pages)
   
   title= "MBIO 612: Final Independent Project", #add title in navigation bar
   
   bg= "#B6D0E2", #color navigation bar
   
   inverse= T, #make navigation bar at the top of the screen
   
   nav_panel(title= "Introduction", p( #first page, add title on navigation bar
     uiOutput("title"), #add page title
     uiOutput("author"), #add authorship
     imageOutput("gbr_pic"), #add great barrier reef image (image is stored in repository folder!)
     uiOutput("pic_capt"),#add image source
     uiOutput("intro_body"), #add small description 
     uiOutput("aims_link"), #add link to the data source (AIMS)
     )),
   
   nav_panel(title= "Data", p( #second page, add title on navigation bar
     uiOutput("data_title"), #add title for data dictionary
     htmlOutput("data_dict"), #add data dictionary
   )),
    
   nav_panel(title= "Objective 1", p( #third page, add title on navigation bar
     uiOutput("site_map"), #add page title
     leafletOutput("map"), #add leaflet map
     uiOutput("o1_body"), #add small description 
     )),
   
   nav_panel(title= "Objective 2", p( #fourth page, add title on navigation bar
     imageOutput("bar"), #add animated bar plot
     uiOutput("o2_body"), #add small description
     )),
   
   nav_panel(title= "Objective 3", p( #fifth page, add title on navigation bar
     selectInput(inputId= "site", #site name for the input
                 label= "Select a Site", #label for the input
                 choices= unique(cots_data$Site)), #drop down with site choices
     
     selectInput(inputId= "reef", #reef name for the input
                 label= "Select a Reef", #label for the input
                 choices= unique(cots_data$Reef)), #drop down with reef choices (will later change to be based on 1st site input)
     
     fluidRow( #make the table and animated line plot be formatted next to eachother
       column(6, htmlOutput("reef_table")), #add  a kable table
       column(6, imageOutput("line")) #add a animated line plot 
     ),
     
     uiOutput("o3_body") #add small description
     
   ))
   
)

    server <- function(input, output, session) {
      output$title <- renderUI({ #add intro page title
        HTML(paste("<h2> Investigating Changes in Coral Presence within the Great Barrier Reef", #title in header 2
              sep= ""))
      })
      
      output$author <- renderUI({ #add authorship
        HTML(paste("<p> By: Shelbie Ishimaru", #add my name in regular text
                   sep= ""))
      })
      
      output$gbr_pic <- renderImage({ #add great barrier reef image
        list(src= here("final_proj", "gbr.png"), #find the file
             width= "100%", #width of displayed image
             height= 400) #height of displayed image
      },
      deleteFile= F #don't delete the image from folder, so it can keep showing up!
      )
      
      output$pic_capt <- renderUI({ #add image source
        HTML(paste("<p> Image Source: ", #regular text
                   "<em>Troy Mayne/WWF Australia. 2024.", #italicize the author
                   sep= ""))
      })
      
      output$intro_body <- renderUI({ #add intro information! 
        HTML(paste("Within this analysis I will be visualizing these three objectives: <br>", #description of goals
                   "1. The study's survey sites across the Great Barrier Reef <br>",
                   "2. Percent live coral cover in the Great Barrier Reef between 1992 - 2022 <br>",
                   "3. Percent live and dead coral cover seen in each reef by year <br>",
                   "<br>", #br= line break to edit spacing
                   "All data analysed was collected from the Australian Institute of Marine Science's Long-Term Monitoring Project. <br>", #data source acknowledgement 
                   "<p>Data Soruce: ",
                   "<em> Australian Institute of Marine Science (AIMS). (2015). AIMS Long-term Monitoring Program: Crown-of-thorns starfish and benthos Manta Tow Data (Great Barrier Reef). https://doi.org/10.25845/5c09b0abf315a.", #data source citation
                   sep= ""))
      })
      
      output$aims_link <- renderUI({ #add link to website where I got the data
        tagList(a("Click here to learn more about the project and see annual reports!", href= "https://apps.aims.gov.au/metadata/view/5bb9a340-4ade-11dc-8f56-00008a07204e")) #add clickable link
      })
      
      output$data_title <- renderUI({ #add image source
        HTML(paste("<strong> Data Dictionary", #regular text
                   sep= ""))
      })
      
      output$data_dict <- renderText({ #add data dictionary
        kbl(data_dict) %>% #create kable
          kable_minimal() %>% #select type
          kable_styling(full_width = FALSE, row_label_position = "c") #make it thin
        })
      
      output$site_map <- renderUI({ #add second page title in bold
        HTML(paste("<strong> Site Map",
                   sep= ""))
      })
      
      map_pal <- colorFactor(palette= pal, domain= sites$Site) #create map color palette to color circles by site
      output$map <- renderLeaflet({ #add leaflet map
        leaflet() %>% #call leaflet
        addTiles() %>% #add map tiles
        setView(lat= -17, lng= 150, zoom= 4) %>% #set our initial view
        addCircleMarkers(data= sites, #add sites dataframe to create points of each site
                         lat= ~LATITUDE, #lat of each site
                         lng= ~LONGITUDE, #long of each site
                         radius= 3, #make each circle this size
                         color= ~map_pal(Site), #make the circle colored by site 
                         fillOpacity= 0.5, #controls the circles transparency 
                         popup= ~paste("Site: ", Site, "<br>",  #add site name when you click on a circle
                                       "Reef:", Reef)) #add reef name when you click on a circle
        })
      
      output$o1_body <- renderUI({ #add info on what I accomplished in objective 1
        HTML(paste("<p> Here's a leaflet map of all reefs surveyed colored by specific site within the study. ", #regular text
                   "You can click on each dot to display the specifc site and reef name, then zoom in to see surrounding areas of each site. ",
                   "Within each site towed divers surveyed specific reefs to obtain percent live coral cover, percent dead coral cover, and many more variables.",
                   sep= ""))
      })
      
      output$bar <- renderImage({ #add animated bar plot
        coral_map <- ggplot(data= coral_data, aes(x= Site, y= live_coral_tot, fill= Site)) + #initialize plot
          geom_bar(stat= "identity") + #display bars based on our dataframe needs
          transition_states(Year) + #animate by year column 
          scale_fill_manual(values= pal) + #make 11 different colored bars based on a colorblind friendly palette 
          labs(title= "Live Coral By Site in the Great Barrier Reef", #add plot title
               subtitle= "Year= {closest_state}", #add subtitle that informs viewers of the current year
               x= "Site", #add x-axis title
               y= "Mean Live Coral Cover (%)", #add y-axis title
               caption= "Source: Australian Institute of Marine Science (AIMS). 2015.") + #add caption to credit data source
          theme_bw() + #nice simple theme
          theme(plot.title= element_text(hjust= 0.5, face= "bold", size= 45), #change text size and bold title
                plot.subtitle= element_text(size= 20), #change subtitle text size
                axis.text= element_text(size= 18), #change axis values size
                axis.text.x= element_text(angle= 45, vjust= 1, hjust= 1), #make x-axis labels (site names) angled for better viewing
                axis.title= element_text(size= 25), #change axis title size
                plot.caption= element_text(size= 12), #change caption text size and bold title
                plot.caption.position = "plot", #make the caption in the bottom right corner
                legend.position= "none") #remove legend
        
        anim_save("coral_plot.gif", animate(coral_map, nframes= 500, height= 800, width= 1000)) #slow animation speed and save animation
        list(src = "coral_plot.gif", contentType = "image/gif") #help us get animation displayed in shiny
      },
      deleteFile= T #delete this every run
      )
      
      output$o2_body <- renderUI({ #add info on what I accomplished in objective 2
        HTML(paste("<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>","<br>", #had issue with spacing (the text was showing up mid animation?) so this was a quick/simple way to alter spacing
                   "<p> Figure 1. An animated plot that shows live coral percent cover from each site from 1992 - 2022. ", #regular text
                   "The raw data from AIMS included mean live coral and mean live soft coral for every reef within each site and year. ",
                   "To calculate mean percent live coral I first summed the mean live coral and mean live soft coral by reef and year, then I found the mean of each site by year. ",
                   "When bars are not visible in the animation that means that surveys were not conducted on any reef within that site for that specific year. ",
                   "Although this shows broad site changes over time, it's extremely general. Averaging over multiple reefs within one site can hide significant changes that occur within one reef site.",
                   sep= ""))
      })
      
      observe({ #edit our reef options in objective 3
        updateSelectInput(session, "reef", choices= cots_data[cots_data$Site== input$site, "Reef"]) #change drop down list of reef choices based on what site was selected!
      })
      
      reef_data <- reactive({ #reactive because it changed based on user input
        cots_data %>% #call cots data
          select(Site, Reef, Year, live_coral_tot, MEAN_DEAD_CORAL) %>% #select columns important for our analysis
          group_by(Site, Reef, Year) %>% #group by for analysis
          summarise("Dead Coral"= mean(MEAN_DEAD_CORAL), #calculate the mean percent dead coral by reef
                    "Live Coral"= mean(live_coral_tot)) %>% #calculate the mean percent live coral by reef and year 
          pivot_longer(cols= c("Dead Coral", "Live Coral"), #pivot data to aid in plotting
                       names_to= "Status", #status column= if the coral is alive or dead
                       values_to= "Cover") %>% #cover column= mean percent cover for the reef and year
          filter(Site== input$site, Reef== input$reef) #filter our dataframe so it only includes the site and reef the user selected
      })
      
      output$reef_table <- renderText({ #create nice table
        kbl(reef_data()) %>% #create kable
          kable_classic_2(lightable_options = "striped") %>% #select type
          kable_styling(full_width = FALSE, row_label_position = "c") #make it thin
      })
      
      output$line <- renderImage({ #create animated line plot
        coral_status <- reef_data() %>% #pull in our reef data that was edited based on user input
          ggplot(aes(x= Year, y= Cover, color= Status)) + #initialize plot
          geom_line() + #make it a line plot
          geom_point() + #add points for animation
          transition_reveal(Year) + #animate by year (transition reveal shows the new year while keeping old yeaers displayed)
          labs(title= "Percent Live vs. Dead Coral Cover", #add plot title
               x= "Year", #add x-axis title
               y= "Mean Coral Cover (%)", #add y-axis title
               color= "Status", #add legend title
               caption= "Source: Australian Institute of Marine Science (AIMS). 2015.") + #add caption to credit data source
          theme_bw() + #nice simple theme
          theme(plot.title= element_text(hjust= 0.5, face= "bold", size= 25), #change text size and bold title
                axis.text= element_text(size= 13), #change axis values size
                axis.title= element_text(size= 16), #change axis title size
                plot.caption= element_text(size= 10), #change caption text size and bold title
                plot.caption.position = "plot", #make the caption in the bottom right corner
                legend.title= element_text(size= 16), #change legend title size
                legend.text= element_text(size= 13)) #change legend text size
        anim_save("coral_status.gif", animate(coral_status, nframes= 500)) #slow animation speed
        list(src = "coral_status.gif", contentType = "image/gif") #help us get animation displayed in shiny
      },
      deleteFile= T) #delete this every run
      
      output$o3_body <- renderUI({ #add info on what I accomplished in objective 3
        HTML(paste("<p> On your left is a table with the percent mean live and dead coral cover by year for a specific reef within a site. ", #regular text
                   "The animated line plot on your right visualizes the table and shows how percent mean coral cover changed overtime. ",
                   "By looking at a specific reef we can obtain more information about reef health when compared to broadly analyzing by site.",
                   sep= ""))
      })
      
}

# Run the Shiny App!
shinyApp(ui = ui, server = server) #run shiny app!
