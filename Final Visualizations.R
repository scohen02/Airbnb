require(shiny)
require(treemap)
require(circlepackeR)
require(data.tree)
require(dplyr)
require(statebins)
require(ggplot2)
require(dplyr)
require(leaflet)
require(geofacet)
require(rgdal)
require(geojsonio)
require(RColorBrewer)
require(sf)
require(geofacet)
require(ggridges)
require(Hmisc)
require(tidyverse)

usa_airbnb = read.csv("/Users/sophiecohen/Desktop/Data Visualization/Final Project/5 Final Visualizations and Paper/Final Project/usa2.csv")
world = read.csv("/Users/sophiecohen/Desktop/Data Visualization/Final Project/5 Final Visualizations and Paper/Final Project/world.csv")

world$country[world$country == "England"] = "United Kingdom"
world$country[world$country == "UK"] = "United Kingdom"
world$country[world$country == "USA"] = "United States of America"

world$reviewed[world$number_of_reviews < 1 ] = "None"
world$reviewed[1 <= world$number_of_reviews &  world$number_of_reviews < 5 ] = "Few"
world$reviewed[5 <= world$number_of_reviews & world$number_of_reviews < 27 ] = "Many"
world$reviewed[world$number_of_reviews >= 27 ] = "Highly"

world$available[world$availability_365 == 1 ] = "1 Day"
world$available[world$availability_365 == 2 ] = "2 Days"
world$available[world$availability_365 == 3 ] = "3 Days"
world$available[world$availability_365 == 4 ] = "4 Days"
world$available[world$availability_365 == 5 ] = "5 Days"
world$available[world$availability_365 == 6 ] = "6 Days"
world$available[world$availability_365 == 7 ] = "One Week"
world$available[world$availability_365 == 14 ] = "Two Weeks"
world$available[28 <= world$availability_365 & world$availability_365 <= 31 ] = "One Month"
world$available[178 <= world$availability_365 & world$availability_365 <= 186 ] = "Six Months"
world$available[world$availability_365 >= 365 ] = "Year+"
world = world %>% 
  drop_na(available)

world$min_nights[world$minimum_nights == 1 ] = "1 Day"
world$min_nights[world$minimum_nights == 2 ] = "2 Days"
world$min_nights[world$minimum_nights == 3 ] = "3 Days"
world$min_nights[world$minimum_nights == 4 ] = "4 Days"
world$min_nights[world$minimum_nights == 5 ] = "5 Days"
world$min_nights[world$minimum_nights == 6 ] = "6 Days"
world$min_nights[world$minimum_nights == 7 ] = "One Week"
world$min_nights[world$minimum_nights == 14 ] = "Two Weeks"
world$min_nights[28 <= world$minimum_nights & world$minimum_nights <= 31 ] = "One Month"
world$min_nights[178 <= world$minimum_nights & world$minimum_nights <= 186 ] = "Six Months"
world$min_nights[world$minimum_nights >= 365 ] = "Year+"
world = world %>% 
  drop_na(min_nights)


runApp(list(
  ui = fluidPage(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    titlePanel("Airbnb"),
    
    sidebarPanel(bootstrapPage(
      radioButtons(inputId="geography",
                   label="Geography Level",
                   choices=c("World","Continent","Country", "State", "City")),
      uiOutput('columns')
    )),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel(title="Price",
                 textOutput(outputId="price_choices"),
                 conditionalPanel(
                   condition = "input$geography == 'World'",
                   circlepackeROutput(outputId="world_plot1a"),
                   leafletOutput(outputId="world_plot1b"),
                   textOutput(outputId="table1"),
                   tableOutput(outputId="world_table1"),
                   textOutput(outputId="table2"),
                   tableOutput(outputId="world_table2")),
                 conditionalPanel(
                   condition = "input$geography == 'State''",
                   plotOutput(outputId="state_plot1"),
                   textOutput(outputId="table3"),
                   tableOutput(outputId="state_table1"),
                   textOutput(outputId="table4"),
                   tableOutput(outputId="state_table2"))),
        tabPanel(title="Room Type",
                 conditionalPanel(
                   condition = "input$geography == 'World'",
                   plotOutput(outputId="world_plot2")),
                 conditionalPanel(
                   condition = "input$geography == 'Continent'",
                   plotOutput(outputId="continent_plot2")),
                 conditionalPanel(
                   condition = "input$geography == 'Country''",
                   plotOutput(outputId="country_plot2")),
                 conditionalPanel(
                   condition = "input$geography == 'State''",
                   plotOutput(outputId="state_plot2")),
                 conditionalPanel(
                   condition = "input$geography == 'City'",
                   plotOutput(outputId="city_plot2"))),
        tabPanel(title="Availability",
                 textOutput(outputId="availability_choices"),
                 plotOutput(outputId="city_plot3")),
        tabPanel(title="Minimum Nights",
                 textOutput(outputId="night_choices"),
                 plotOutput(outputId="city_plot4")),
        tabPanel(title="Reviews",
                 conditionalPanel(
                   condition = "input$geography == 'World'",
                   plotOutput(outputId="world_plot5")),
                 conditionalPanel(
                   condition = "input$geography == 'Continent'",
                   plotOutput(outputId="continent_plot5")),
                 conditionalPanel(
                   condition = "input$geography == 'Country''",
                   plotOutput(outputId="country_plot5")),
                 conditionalPanel(
                   condition = "input$geography == 'State''",
                   plotOutput(outputId="state_plot5")),
                 conditionalPanel(
                   condition = "input$geography == 'City'",
                   plotOutput(outputId="city_plot5")))
    )
  )),
  
  
  server = function(input, output){
    
    output$columns = renderUI ({
      
      if(input$geography == 'Continent') {
        selectInput('columns_continent', 'Continents', sort(unique(world$continent)))
      }
      else if(input$geography == 'Country') {
        selectInput('columns_country', 'Countries', sort(unique(world$country)))
      } 
      else if(input$geography == 'State') {
        selectInput('columns_state', 'States', sort(unique(world$state)))
      } 
      else if(input$geography == 'City') {
        selectInput('columns_city', 'Cities', sort(unique(world$city)))
      }
    })
    
    output$price_choices <- renderText({
      "Only choose between 'World' and 'State'"
    })
    
    output$world_plot1a <- renderCirclepackeR({
      if(input$geography=="World"){
        world_sub = world %>%
          group_by(country, continent) %>%
          summarise(count = n()) %>%
          group_by(country, continent) %>%
          summarise(listings = sum(count)) %>%
          mutate(pathString=paste("World", continent,
                                  country, sep="/"))
        listings = as.Node(world_sub)
        circlepackeR(listings, size = "listings")
      }})
    
    output$world_plot1b <- renderLeaflet({
      if(input$geography=="World"){
        country_shapes <- geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json",
                                       what="sp")
        
        country_shapes <- st_as_sf(country_shapes)
        
        country_shapes = country_shapes %>%
          mutate(country=name) 
        
        world["country"][world["country"] == "USA"] = "United States of America"
        world["country"][world["country"] == c("England", "UK")] = "United Kingdom"
        
        world_sub2 = world %>%
          group_by(country) %>%
          summarise(AvgPrice = mean(price))
        
        
        world_ToMap <- left_join(country_shapes, world_sub2,
                                 by="country")  
        
        pal <- colorQuantile("RdPu",domain=NULL, n =5)  
        
        leaflet(data = world_ToMap) %>%
          addTiles("CartoDB.Positron") %>%
          addPolygons(fillColor = ~pal(AvgPrice),
                      fillOpacity = 0.8,
                      color = "black",
                      weight = 1,
                      popup=~paste("Country:", country,
                                   "<br>Average Price:", AvgPrice)) %>%
          addLegend("bottomleft",
                    colors=brewer.pal(5,"RdPu"),
                    labels=c("lowest","","","","highest"),
                    title="Average Price")
      }})
    
    output$table1 <- renderText({
      if(input$geography=="World"){
      "Top 5 Countries with Highest Average Price:"
    }})
    
    output$world_table1 <- renderTable({
      if(input$geography=="World"){
        world_sub2 = world %>%
          group_by(country) %>%
          rename(Country = country) %>%
          summarise(AvgPrice = mean(price)) %>%
          arrange(desc(AvgPrice)) %>%
          rename('Average Price ($)' = AvgPrice) %>%
          head(5)
      }})
    
    output$table2 <- renderText({
      if(input$geography=="World"){
      "Top 5 Countries with Lowest Average Price:"
    }})
    
    output$world_table2 <- renderTable({
      if(input$geography=="World"){
        world_sub2 = world %>%
          group_by(country) %>%
          rename(Country = country) %>%
          summarise(AvgPrice = mean(price)) %>%
          arrange(AvgPrice) %>%
          rename('Average Price ($)' = AvgPrice) %>%
          head(5)
      }})
    
    output$state_plot1 <- renderPlot ({
      if(input$geography=="State"){
        airbnb_sub = usa_airbnb %>%
          mutate(state=as.character(state)) %>%
          filter(state != "Washington DC") %>%
          group_by(state) %>%
          summarise(AvgPrice = mean(price))
        
        ggplot()+
          geom_statebins(aes(state=state.name), fill="gray") +
          geom_statebins(data=airbnb_sub, aes(state=state,
                                              fill=AvgPrice))+
          scale_fill_distiller("Average Price", palette="BuPu", direction=1)+
          theme_void()
      }})
    
    output$table3 <- renderText({
      if(input$geography=="State"){
      "Top 5 States with Highest Average Price:"
    }})
    
    output$state_table1 <- renderTable({
      if(input$geography=="State"){
        airbnb_sub = usa_airbnb %>%
          mutate(state=as.character(state)) %>%
          filter(state != "Washington DC") %>%
          group_by(state) %>%
          rename(State = state) %>%
          summarise(AvgPrice = mean(price)) %>%
          arrange(desc(AvgPrice)) %>%
          rename('Average Price ($)' = AvgPrice) %>%
          head(5)
      }})
    
    output$table4 <- renderText({
      if(input$geography=="State"){
      "Top 5 States with Lowest Average Price:"
    }})
    
    output$state_table2 <- renderTable({
      if(input$geography=="State"){
        airbnb_sub = usa_airbnb %>%
          mutate(state=as.character(state)) %>%
          filter(state != "Washington DC") %>%
          group_by(state) %>%
          rename(State = state) %>%
          summarise(AvgPrice = mean(price)) %>%
          arrange(AvgPrice) %>%
          rename('Average Price ($)' = AvgPrice) %>%
          head(5)
      }})
    
    output$world_plot2 <- renderPlot({
      if(input$geography=="World"){
        ggplot(data=world) +
          stat_summary(aes(x=room_type, y=price, fill=room_type), fun="mean", geom="bar") +
          scale_fill_brewer("Room Type", palette="YlGnBu") +
          xlab("Room Type") +
          ylab("Average Price") +
          ggtitle("Relationship between Room Type and Average Price for Airbnbs in the World")

      }})

      output$continent_plot2 <- renderPlot({
        if(input$geography=="Continent"){
          sub = world %>%
            filter(continent == input$columns_continent)

          ggplot(data=sub) +
            stat_summary(aes(x=room_type, y=price, fill=room_type), fun="mean", geom="bar") +
            scale_fill_brewer("Room Type", palette="YlGnBu") +
            xlab("Room Type") +
            ylab("Average Price") +
            ggtitle(paste("Relationship between Room Type and Average Price for Airbnbs in", input$columns_continent))

        }})

    output$country_plot2 <- renderPlot({
      if(input$geography=="Country"){
        sub = world %>%
          filter(country == input$columns_country)

        ggplot(data=sub) +
          stat_summary(aes(x=room_type, y=price, fill=room_type), fun="mean", geom="bar") +
          scale_fill_brewer("Room Type", palette="YlGnBu") +
          xlab("Room Type") +
          ylab("Average Price") +
          ggtitle(paste("Relationship between Room Type and Average Price for Airbnbs in", input$columns_country))

      }})

    output$state_plot2 <- renderPlot({
      if(input$geography=="State"){
        sub = world %>%
          filter(state == input$columns_state)

        ggplot(data=sub) +
          stat_summary(aes(x=room_type, y=price, fill=room_type), fun="mean", geom="bar") +
          scale_fill_brewer("Room Type", palette="YlGnBu") +
          xlab("Room Type") +
          ylab("Average Price") +
          ggtitle(paste("Relationship between Room Type and Average Price for Airbnbs in", input$columns_state))
      }})

    output$city_plot2 <- renderPlot({
      if(input$geography=="City"){
        sub = world %>%
          filter(city == input$columns_city)

        ggplot(data=sub) +
          stat_summary(aes(x=room_type, y=price, fill=room_type), fun="mean", geom="bar") +
          scale_fill_brewer("Room Type", palette="YlGnBu") +
          xlab("Room Type") +
          ylab("Average Price") +
          ggtitle(paste("Relationship between Room Type and Average Price for Airbnbs in", input$columns_city))

      }})
    
    output$availability_choices <- renderText({
      "Only choose 'City'"
    })
    
    output$city_plot3 <- renderPlot({
      if(input$geography=="City"){
      sub = world %>%
        filter(city == input$columns_city) 
      
      ggplot(data=sub) +
        geom_density_ridges(aes(x=price, y=factor(available, levels=c("1 Day", "2 Days", "3 Days", "4 Days", "5 Days", "6 Days", "One Week", "Two Weeks", "One Month", "Six Months", "Year+")), fill=available), scale=1, bandwidth=24.1)+
        xlab("Price") +
        ylab("Number of Available Days") +
        scale_fill_brewer("", palette="Set3")+
        ggtitle(paste("Relationship between Availability and Price in", input$columns_city)) +
        theme(legend.position = "none")
      
    }})
    
    output$night_choices <- renderText({
      "Only choose 'City'"
    })
    
    output$city_plot4 <- renderPlot({
      if(input$geography=="City"){
      sub = world %>%
        filter(city == input$columns_city)
      
      ggplot(data=sub)+
        stat_summary(aes(x=factor(min_nights, levels=c("1 Day", "2 Days", "3 Days", "4 Days", "5 Days", "6 Days", "One Week", "Two Weeks", "One Month", "Six Months", "Year+")), y=price, fill=min_nights), fun="mean", geom="bar") +
        theme(legend.position = "none") +
        xlab("Number of Minimum Nights")+
        ylab("Average Price") +
        scale_fill_brewer("", palette="Paired")+
        ggtitle(paste("Relationship between Number of Minimum Nights and Price in", input$columns_city))+
        theme(axis.text.x = element_text(angle = 90))
    }})
    
    output$world_plot5 <- renderPlot({
      if(input$geography=="World"){
      
      world = world %>%
        group_by(continent, reviewed) %>%
        summarise(AvgPrice = mean(price))
      
      ggplot(data=world)+
        geom_tile(aes(x=factor(reviewed, levels=c("None", "Few", "Many", "Highly")), y=continent, fill=AvgPrice))+
        scale_fill_distiller("Average Price", palette="PiYG")+
        ylab("Continent")+
        xlab("Reviews") +
        ggtitle("Relationship between Number of Reviews and Average Price in the World")
      
    }})
    
    output$continent_plot5 <- renderPlot({
      if(input$geography=="Continent"){
      sub = world %>%
        filter(continent == input$columns_continent)
      
      sub$reviewed[sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] ] = "None"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] <= sub$number_of_reviews &  sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] ] = "Few"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] <= sub$number_of_reviews & sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Many"
      sub$reviewed[sub$number_of_reviews >= quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Highly"
      
      sub = sub %>%
        group_by(country, reviewed) %>%
        summarise(AvgPrice = mean(price))
      
      ggplot(data=sub)+
        geom_tile(aes(x=factor(reviewed, levels=c("None", "Few", "Many", "Highly")), y=country, fill=AvgPrice))+
        scale_fill_distiller("Average Price", palette="PiYG")+
        ylab("Country")+
        xlab("Reviews") +
        ggtitle(paste("Relationship between Number of Reviews and Average Price in", input$columns_continent))
      
    }})
    
    output$country_plot5 <- renderPlot({
      if(input$geography=="Country"){
      sub = world %>%
        filter(country == input$columns_country)
      
      sub$reviewed[sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] ] = "None"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] <= sub$number_of_reviews &  sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] ] = "Few"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] <= sub$number_of_reviews & sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Many"
      sub$reviewed[sub$number_of_reviews >= quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Highly"
      
      sub = sub %>%
        group_by(city, reviewed) %>%
        summarise(AvgPrice = mean(price))
      
      ggplot(data=sub)+
        geom_tile(aes(x=factor(reviewed, levels=c("None", "Few", "Many", "Highly")), y=city, fill=AvgPrice))+
        scale_fill_distiller("Average Price", palette="PiYG")+
        ylab("City")+
        xlab("Reviews") +
        ggtitle(paste("Relationship between Number of Reviews and Average Price in", input$columns_country))
      
    }})
    
    output$state_plot5 <- renderPlot({
      if(input$geography=="State"){
      sub = world %>%
        filter(state == input$columns_state)
      
      sub$reviewed[sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] ] = "None"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] <= sub$number_of_reviews &  sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] ] = "Few"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] <= sub$number_of_reviews & sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Many"
      sub$reviewed[sub$number_of_reviews >= quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Highly"
      
      sub = sub %>%
        group_by(city, reviewed) %>%
        summarise(AvgPrice = mean(price))
      
      ggplot(data=sub)+
        geom_tile(aes(x=factor(reviewed, levels=c("None", "Few", "Many", "Highly")), y=city, fill=AvgPrice))+
        scale_fill_distiller("Average Price", palette="PiYG")+
        ylab("City")+
        xlab("Reviews") +
        ggtitle(paste("Relationship between Number of Reviews and Average Price in", input$columns_state))
    }})
    
    output$city_plot5 <- renderPlot({
      if(input$geography=="City"){
      sub = world %>%
        filter(city == input$columns_city)
      
      sub$reviewed[sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] ] = "None"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[1] <= sub$number_of_reviews &  sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] ] = "Few"
      sub$reviewed[quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[2] <= sub$number_of_reviews & sub$number_of_reviews < quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Many"
      sub$reviewed[sub$number_of_reviews >= quantile(sub$number_of_reviews, prob=c(.25,.5,.75))[3] ] = "Highly"
      
      sub = sub %>%
        group_by(neighbourhood, reviewed) %>%
        summarise(AvgPrice = mean(price))
      
      ggplot(data=sub)+
        geom_tile(aes(x=factor(reviewed, levels=c("None", "Few", "Many", "Highly")), y=neighbourhood, fill=AvgPrice))+
        scale_fill_distiller("Average Price", palette="PiYG")+
        ylab("Neighborhood")+
        xlab("Reviews") +
        ggtitle(paste("Relationship between Number of Reviews and Average Price in", input$columns_city))
    }})
  } 
))


