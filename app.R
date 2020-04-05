library(readr)
library(dplyr)
library(highcharter)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(scales)
library(countrycode)
library(DT)
library(echarts4r)
library(viridis)
library(shinycustomloader)

# Read raw data file
data <- read_csv('suicide_data.csv')
source('style.R')
source('funs.R')
map_data <- download_map_data("custom/world-continents")
GIF <- "ambulance.gif"


# Filter out countries having no data
x <- data %>% group_by(country) %>% summarise(Total=sum(`suicides/100k pop`)) %>% filter(Total==0) %>% distinct(country) %>% pull()

# Cleaned Data
data <- data %>% filter(!country %in% x)

# Continent
data$continent <- countrycode(sourcevar = data$country,
                              origin = "country.name",
                              destination = "continent")


data$count_2 <-countrycode(data$country, 
                           origin = 'country.name', 
                           destination = 'genc2c')

data$flags <- paste0("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
                     tolower(data$count_2), ".svg")

dff <- data %>%
    group_by(year) %>%
    summarise(s1 = (sum(suicides_no)/sum(population))*100000)

countries <- c('Overall', unique(data$country))
flags <- c('https://upload.wikimedia.org/wikipedia/commons/c/c4/Globe_icon.svg',
           unique(data$flags))


#### ==== UI ====####
app <- shinyApp(
    ui <- dashboardPagePlus(
        skin = "red",
        collapse_sidebar = TRUE,
        dashboardHeader(
            title = 
                span(class = "logo-lg", "Suicide Analysis"),
            
              tags$li(
                class = "dropdown", div(p("Country"), 
                style= "padding-top: 3px; 
                  color: white; font-weight: bold; 
                  font-size: 22px; margin-right: -40px;
                  font-family: 'Inter UI',sans-serif;")
              ),
            
                tags$li(class = "dropdown", 
                        div(pickerInput("country", "",
                                        choices = countries,
                                        selected = 'Overall',
                                        choicesOpt = list(content =  
                                                            mapply(countries, flags, FUN = function(country, flagUrl) {
                                                              HTML(paste(
                                                                tags$img(src=flagUrl, width=20, height=15),
                                                                country
                                                              ))
                                                            },
                                                            SIMPLIFY = FALSE, USE.NAMES = FALSE)),
                                        options = pickerOptions(liveSearch =TRUE)), 
                        style= "width: 60%; margin-left: auto; margin-right: auto; 
                                margin-bottom:-10px; padding-top: 10px; margin-top: -30px;"
                        ))

            ),
            
        dashboardSidebar(
            sidebarMenu(
                menuItem(
                    text = "HOME",
                    tabName = "home",
                    icon = icon("home")
                ),
                menuItem(
                    text = "MAP",
                    tabName = "map",
                    icon = icon("map")
                )
            )
        ),
        dashboardBody(
            mystyle(),
            useShinyjs(),
            tabItems(
                tabItem(
                    tabName = "home",
                    fluidRow(
                        uiOutput('suicide'),
                        uiOutput('suicide2'),
                        uiOutput('suicide3'),
                        uiOutput('suicide4')),
                        
                    fluidRow(
                        box(withLoader(echarts4rOutput("gender"), type="image", loader=GIF)),
                        box(withLoader(highchartOutput("line_gender"), type="image", loader=GIF))
                        ),
                    fluidRow(
                        box(withLoader(echarts4rOutput("age"), type="image", loader=GIF)),                    
                        box(withLoader(highchartOutput("line_age"), type="image", loader=GIF))
                    )

                ),
                tabItem(
                    tabName = "map",
                      tabBox(
                        id = "tabset1",
                        width = 12,
                        tabPanel("Continent View", withLoader(highchartOutput("map3", height = "700px"), type="image", loader=GIF)),
                        tabPanel("Country View", withLoader(highchartOutput("map2", height = "700px"), type="image", loader=GIF))
                    )) 
            )
        )
    ),
    
    #### ==== SERVER ====####
    
    server <- function(input, output, session) {
        
      # Header Title
      observe({
        
        shinyjs::html("pageHeader", 
                      HTML(paste(
                        tags$img(src=flags[which(countries== input$country)], 
                                 width=50, height=40),
                        "<span class='main-color'>",input$country,"</span>",
                        "Suicide Data Dashboard")
                      ))
        
      })
      
        output$suicide <- renderUI( {
            
          if(input$country=='Overall') {
            y <- scales::number(mean(dff$s1), accuracy = 0.01)
          }
          
          else {
            ov <- data %>%
              filter(country == input$country) %>% 
              group_by(year) %>%
              summarise(s1 = (sum(suicides_no)/sum(population))*100000)
              
            y <- scales::number(mean(ov$s1), accuracy = 0.01)
            
          }
          
            ivalueBox(
                value = y, 
                title = "Avg. Suicides per 100K", 
                icon = icon("user-md")
            )
        
        } )
        
        output$suicide4 <- renderUI( {
            
          if(input$country=='Overall') {
            y <- dff %>% filter(s1 == max(dff$s1)) %>% pull(year)
          }
          
          else {
            y <- data %>%
              filter(country == input$country) %>% 
              group_by(year) %>%
              summarise(s1 = (sum(suicides_no)/sum(population))*100000) %>% 
              filter(s1 == max(s1)) %>% pull(year)
            }
          
          
            
            ivalueBox(
                value = y, 
                title = "Year Has Max Suicide Rate", 
                icon = icon("calendar")
            )
            
        } )
        
        output$suicide2 <- renderUI( {
            
            ct <- data %>%
                filter(year >= 2010) %>% 
                group_by(country) %>%
                mutate(y= min_rank(desc(year))) %>% 
                filter(y == 1) %>% 
                summarise(s1 = (sum(suicides_no)/sum(population))*100000) %>% 
                mutate(v = min_rank(desc(s1))) %>% 
                filter(v == 1) %>% 
                pull(country)
            
            
            ivalueBox(
                value = ct, 
                title = "Country Has Max Suicide Rate", 
                icon = icon("flag")
            )
            
        } )
        
        output$suicide3 <- renderUI( {
            
            ct <- data %>%
                filter(year >= 2010) %>% 
                group_by(continent) %>%
                mutate(y= min_rank(desc(year))) %>% 
                filter(y == 1) %>% 
                summarise(s1 = (sum(suicides_no)/sum(population))*100000) %>% 
                mutate(v = min_rank(desc(s1))) %>% 
                filter(v == 1) %>% 
                pull(continent)
            
            ivalueBox(
                value = ct, 
                title = "Continent Has Max Suicide Rate", 
                icon = icon("globe"), 
            )
            
        } )
        
        
        output$line <- renderHighchart({ 
          
            highchart() %>% 
            hc_add_series(dff, hcaes(x = year, 
                                                y = s1,
                                                color = s1), type = "line") %>%
            hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", 
                       pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y:,.2f}</b>")) %>%
            hc_title(text = "Worldwide suicides by year",
                         style = list(
                           fontFamily = "Bangers",
                           color = "#666",
                           fontWeight = "normal",
                           fontSize = "36px"
                         )
                     ) %>% 
            hc_xAxis(title = list(text = "Year")) %>%
            hc_yAxis(title = list(text = "Suicides per 100K people"),allowDecimals = FALSE) %>%
            hc_legend(enabled = FALSE) %>% 
            hc_exporting(enabled = TRUE)    
        })
        
        output$line_gender <- renderHighchart({
            
          if(input$country=='Overall') {
            sex_tibble <- data %>%
              group_by(year, sex) %>%
              summarise(s1 = (sum(suicides_no)/sum(population))*100000)
          }
          else {
            sex_tibble <- data %>%
              filter(country == input$country) %>% 
              group_by(year, sex) %>%
              summarise(s1 = (sum(suicides_no)/sum(population))*100000)
            
          }

            # Pick color for gender.
            sex_color <- c("#EE6AA7", "#87CEEB") # baby blue & pink
            
            # Create line plot.
            highchart() %>% 
                hc_add_series(sex_tibble, hcaes(x = year, y = s1, group = sex), 
                              type = "line", color = sex_color, size = 300) %>%
                hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Gender: <b>{point.sex}</b><br>", "Suicides: <b>{point.y:,.2f}</b>")) %>%
                hc_title(text = "Worldwide suicides by Gender",
                         style = list(
                             fontFamily = "Bangers",
                             color = "#666",
                             fontWeight = "normal",
                             fontSize = "36px"
                         )) %>% 
                hc_xAxis(title = list(text = "Year")) %>%
                hc_yAxis(title = list(text = "Suicides per 100K people"), allowDecimals = FALSE) %>% 
                hc_exporting(enabled = TRUE)
            
        })
        
        
    output$map2 <- renderHighchart({
      
      country_tibble <- data %>%
        select(country, suicides_no, population) %>%
        group_by(country) %>%
        summarize(suc = round((sum(suicides_no)/sum(population))*100000, 2))
      
      highchart() %>%
        hc_add_series_map(worldgeojson, country_tibble, value = "suc", 
                          joinBy = c('name','country'))  %>% 
        hc_colorAxis(stops = color_stops()) %>% 
        hc_title(text = "Suicides by Country") %>% 
        hc_tooltip(borderWidth = 1.5, headerFormat = "", valueSuffix = " suicides (per 100K people)")

    })
    
    output$map3 <- renderHighchart({
      
      continent_tibble <- data %>%
        select(continent, suicides_no, population) %>%
        group_by(continent) %>%
        summarize(suc = round((sum(suicides_no)/sum(population))*100000, 2)) %>%
        arrange(suc)
      
      highchart() %>%
        hc_add_series_map(map_data, continent_tibble, value = "suc", joinBy = c('name','continent'), name = "Suicides (per 100K people)")  %>% 
        hc_add_series(continent_tibble, hcaes(x = continent, y = suc, color = suc), type = "pie", name = 'Suicides (per 100K people)')  %>% 
        hc_colorAxis(stops = color_stops()) %>% 
        hc_title(text = "Suicides by Continent") %>% 
        hc_tooltip(borderWidth = 1.5, valueSuffix = '') %>%
        hc_plotOptions(
          pie = list(center = c('10%', '80%'), size = 130, dataLabels = list(enabled = FALSE)))

    })
        

    output$gender <- renderEcharts4r({
      
      if(input$country=='Overall') {
      gender <- data %>%
        group_by(sex) %>%
        summarise(s1 = (sum(suicides_no)/sum(population))*100000) %>% 
        arrange(desc(sex)) %>% 
        mutate(value = round(100*(s1/sum(s1)),0))
      
      gender = cbind(gender,
                     path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13',
                              'path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z'))
      }
      
      else {
        
        gender <- data %>%
          filter(country == input$country) %>% 
          group_by(sex) %>%
          summarise(s1 = (sum(suicides_no)/sum(population))*100000) %>% 
          arrange(desc(sex)) %>% 
          mutate(value = round(100*(s1/sum(s1)),0))
        
        gender = cbind(gender,
                       path = c('path://M18.2629891,11.7131596 L6.8091608,11.7131596 C1.6685112,11.7131596 0,13.032145 0,18.6237673 L0,34.9928467 C0,38.1719847 4.28388932,38.1719847 4.28388932,34.9928467 L4.65591984,20.0216948 L5.74941883,20.0216948 L5.74941883,61.000787 C5.74941883,65.2508314 11.5891201,65.1268798 11.5891201,61.000787 L11.9611506,37.2137775 L13.1110872,37.2137775 L13.4831177,61.000787 C13.4831177,65.1268798 19.3114787,65.2508314 19.3114787,61.000787 L19.3114787,20.0216948 L20.4162301,20.0216948 L20.7882606,34.9928467 C20.7882606,38.1719847 25.0721499,38.1719847 25.0721499,34.9928467 L25.0721499,18.6237673 C25.0721499,13.032145 23.4038145,11.7131596 18.2629891,11.7131596 M12.5361629,1.11022302e-13 C15.4784742,1.11022302e-13 17.8684539,2.38997966 17.8684539,5.33237894 C17.8684539,8.27469031 15.4784742,10.66467 12.5361629,10.66467 C9.59376358,10.66467 7.20378392,8.27469031 7.20378392,5.33237894 C7.20378392,2.38997966 9.59376358,1.11022302e-13 12.5361629,1.11022302e-13',
                                'path://M28.9624207,31.5315864 L24.4142575,16.4793596 C23.5227152,13.8063773 20.8817445,11.7111088 17.0107398,11.7111088 L12.112691,11.7111088 C8.24168636,11.7111088 5.60080331,13.8064652 4.70917331,16.4793596 L0.149791395,31.5315864 C-0.786976655,34.7595013 2.9373074,35.9147532 3.9192135,32.890727 L8.72689855,19.1296485 L9.2799493,19.1296485 C9.2799493,19.1296485 2.95992025,43.7750224 2.70031069,44.6924335 C2.56498417,45.1567684 2.74553639,45.4852068 3.24205501,45.4852068 L8.704461,45.4852068 L8.704461,61.6700801 C8.704461,64.9659872 13.625035,64.9659872 13.625035,61.6700801 L13.625035,45.360657 L15.5097899,45.360657 L15.4984835,61.6700801 C15.4984835,64.9659872 20.4191451,64.9659872 20.4191451,61.6700801 L20.4191451,45.4852068 L25.8814635,45.4852068 C26.3667633,45.4852068 26.5586219,45.1567684 26.4345142,44.6924335 C26.1636859,43.7750224 19.8436568,19.1296485 19.8436568,19.1296485 L20.3966199,19.1296485 L25.2043926,32.890727 C26.1862111,35.9147532 29.9105828,34.7595013 28.9625083,31.5315864 L28.9624207,31.5315864 Z M14.5617154,0 C17.4960397,0 19.8773132,2.3898427 19.8773132,5.33453001 C19.8773132,8.27930527 17.4960397,10.66906 14.5617154,10.66906 C11.6274788,10.66906 9.24611767,8.27930527 9.24611767,5.33453001 C9.24611767,2.3898427 11.6274788,0 14.5617154,0 L14.5617154,0 Z'))
        
        
      }
      

        gender %>% 
            e_charts(sex) %>% 
            e_x_axis(splitLine=list(show = FALSE), 
                     axisTick=list(show=FALSE),
                     axisLine=list(show=FALSE),
                     axisLabel= list(show=FALSE)) %>%
            e_y_axis(max=100, 
                     splitLine=list(show = FALSE),
                     axisTick=list(show=FALSE),
                     axisLine=list(show=FALSE),
                     axisLabel=list(show=FALSE)) %>%
            e_color(color = c('#87CEEB','#eee')) %>%
            e_pictorial(value, symbol = path,z=10, name= 'realValue', 
                        symbolBoundingData= 100, symbolClip= TRUE) %>% 
            e_pictorial(value, symbol = path, name= 'background', 
                        symbolBoundingData= 100) %>% 
            e_labels(position = "bottom", offset= c(0, 10), 
                     textStyle =list(fontSize= 42,
                                     fontFamily= 'Bangers', 
                                     color= '#87CEEB'),
                     formatter="{@[1]}% {@[0]}") %>%
            e_legend(show = FALSE) %>%
            e_title("Worldwide suicides by Gender", left='center', padding=10,
                    textStyle = list(
                    fontSize = 36,
                    fontWeight = 'normal',
                    color = '#666',
                    fontFamily= 'Bangers')) %>% 
            e_toolbox_feature(feature = c("saveAsImage"), 
                              title = "Save",
                              icon = 'image://data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAMAAAAJbSJIAAAARVBMVEX///8AAAANDQ2GhoZeXl4qKirPz8+KiorU1NS6urpqamqlpaWdnZ2WlpaRkZEREREvLy/CwsK0tLStra0fHx8aGhohISGb2hODAAABAklEQVR4nO3dy3HDMAxFUUSObMuW/I3Tf6mpIQsMBtQ5Fby7JTlSBAAAAAAAAAAAAABAL+s8jWveIravsd3iXj0h2RTVC9Ip7E9hfwr7U9ifwv4U9qewv5irFyR7xFI9Idkz4ng6jOt0rj4GAwAAAAAAYCjL5Xtcl1fEu/oKM9k1fqonJPvs4KVC9YB0CvtT2J/C/hT2p7A/hf3toPBTvSDZb1yrJyR7R7yGPqdZqo/BAAAAAAAAGMp56G8MHSOe1VeYyZZ4VE9INu/gpUL1gHQK+1PYn8L+FPansD+F/e2gcKpekOwet+oJydaIbeh/dq3Vx2AAAAAAAAAAAAAAAP/0B2wtQ4n+W64CAAAAAElFTkSuQmCC')
        

    })
    
    
    output$age <- renderEcharts4r({
        
      
      if(input$country=='Overall') {
        
        age <- data %>%
            group_by(age) %>%
            summarise(s1 = round((sum(suicides_no)/sum(population))*100000, 2)) %>% 
            mutate(value = round(100*(s1/sum(s1)),0))
        
        
        age$age <- factor(age$age, levels = c( "5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years" ))
        age <- age[order(age$age,age$age),]
        age$path = 'image://data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFoAAABaCAYAAAA4qEECAAADYElEQVR4nO2dz0sUYRjHP7tIdAmxQ1LdlhCKMohAIsgiyEuHjkUEFQTlpejS/xCCBB06RBGBBKIG4cGyH0qHBKE9eKyFqBQPRQeNCt06vGNY7bq7szPfeZLnAwuzM+/zgw/DDvMu70wOIVveLscJOwycA44A24CfwAfgKXAbeFVvovlC/o/vuVwuTj+x0FWiYdGbgXvA8RrjHgAXgIVaCbMU3SKr1BhtwEtgZx1jTwI7gG7ga5pNNUO+9pBMuEN9klfYD9xMqZdEsCj6AHAiRtxZYFeyrSSHRdGnYsblCD8jJrEoek8TsbsT6yJhLIrelFFsqlgUPZtRbKpYFP2kidjxxLpIGIuiB4AvMeLmgJGEe0kMi6I/AVdjxPVSx91hVlgUDXAXuEaY16jFMnAJeJhqR01iVTTAdeAYUFxjzBRwCLgl6agJrM51rDAO7AP2EmbxthPO8vfAc2Ams84axLpoCGKLrH1mm8eC6KPAGaAL2Fpj7AZgY7T9DfhRY/wc4eflPmH+OjOynI8uEGbpukXlJ4Dz84V8aWWHcj46q4thFzCNTjJRren2UrlLWPM3WYjuAMYIk/tq2oCx9lK5Q11YLboFGARaxXVX0woMtpfK0uuTWvRFoFNcsxKdhF5kqEX3iuuthbQXtehG/gdMG2kvlm/B1xUuWoSLFmFF9CRwg2TnM4pRzskEc8bGiugR4ArhNjkpJqKcJv51sSJ63eOiRbhoES5ahIsW4aJFuGgRLlqEixbhokW4aBEuWoSLFuGiRbhoES5ahIsW4aJFuGgRLlqEWvTHKvs/p1izWu5qvaSCWvTlCvtmgeEUaw5TeUVtpV5SQy16COgBRoHXhMWb3aS7PnAhqjEQ1RwFeuYL+aEUa/5DFmtYHkefOEwQVmcBvKD+FQNvgNN/P+pHiV8MRbhoES5ahIsW4aJFuGgRLlqEixbhokW4aBEuWoSLFuGiRbhoES5ahIsW4aJFuGgRLlqEixbhokVYEx3nudGKXE1jTfS6xUWLcNEiXLQIFy3CRYtw0SJctAgXLcJFi3DRIv430eUq2+axJvp7jePPqmzHySXFmuhHwFKVYzNA/6rv/VR/s9BSlMsM1kTPEN4DPkU4I8vAO6APOAgsrhq7GO3ri8aUo5ipKIep1zv9AtipgOACGIrLAAAAAElFTkSuQmCC'
        
      } else {
        
        age <- data %>%
          filter(country == input$country) %>% 
          group_by(age) %>%
          summarise(s1 = round((sum(suicides_no)/sum(population))*100000, 2)) %>% 
          mutate(value = round(100*(s1/sum(s1)),0))
        
        
        age$age <- factor(age$age, levels = c( "5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years" ))
        age <- age[order(age$age,age$age),]
        age$path = 'image://data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAFoAAABaCAYAAAA4qEECAAADYElEQVR4nO2dz0sUYRjHP7tIdAmxQ1LdlhCKMohAIsgiyEuHjkUEFQTlpejS/xCCBB06RBGBBKIG4cGyH0qHBKE9eKyFqBQPRQeNCt06vGNY7bq7szPfeZLnAwuzM+/zgw/DDvMu70wOIVveLscJOwycA44A24CfwAfgKXAbeFVvovlC/o/vuVwuTj+x0FWiYdGbgXvA8RrjHgAXgIVaCbMU3SKr1BhtwEtgZx1jTwI7gG7ga5pNNUO+9pBMuEN9klfYD9xMqZdEsCj6AHAiRtxZYFeyrSSHRdGnYsblCD8jJrEoek8TsbsT6yJhLIrelFFsqlgUPZtRbKpYFP2kidjxxLpIGIuiB4AvMeLmgJGEe0kMi6I/AVdjxPVSx91hVlgUDXAXuEaY16jFMnAJeJhqR01iVTTAdeAYUFxjzBRwCLgl6agJrM51rDAO7AP2EmbxthPO8vfAc2Ams84axLpoCGKLrH1mm8eC6KPAGaAL2Fpj7AZgY7T9DfhRY/wc4eflPmH+OjOynI8uEGbpukXlJ4Dz84V8aWWHcj46q4thFzCNTjJRren2UrlLWPM3WYjuAMYIk/tq2oCx9lK5Q11YLboFGARaxXVX0woMtpfK0uuTWvRFoFNcsxKdhF5kqEX3iuuthbQXtehG/gdMG2kvlm/B1xUuWoSLFmFF9CRwg2TnM4pRzskEc8bGiugR4ArhNjkpJqKcJv51sSJ63eOiRbhoES5ahIsW4aJFuGgRLlqEixbhokW4aBEuWoSLFuGiRbhoES5ahIsW4aJFuGgRLlqEWvTHKvs/p1izWu5qvaSCWvTlCvtmgeEUaw5TeUVtpV5SQy16COgBRoHXhMWb3aS7PnAhqjEQ1RwFeuYL+aEUa/5DFmtYHkefOEwQVmcBvKD+FQNvgNN/P+pHiV8MRbhoES5ahIsW4aJFuGgRLlqEixbhokW4aBEuWoSLFuGiRbhoES5ahIsW4aJFuGgRLlqEixbhokVYEx3nudGKXE1jTfS6xUWLcNEiXLQIFy3CRYtw0SJctAgXLcJFi3DRIv430eUq2+axJvp7jePPqmzHySXFmuhHwFKVYzNA/6rv/VR/s9BSlMsM1kTPEN4DPkU4I8vAO6APOAgsrhq7GO3ri8aUo5ipKIep1zv9AtipgOACGIrLAAAAAElFTkSuQmCC'
        
        
      }
        
        age %>% 
            e_charts(age, width='400px', height = '400px') %>% 
            e_pictorial(value, symbol = path, 
                        symbolRepeat = TRUE, z = -1,
                        symbolSize = c(20, 20)) %>% 
            e_title("Worldwide suicides by Age", left='center', padding=10,
                    textStyle = list(
                        fontSize = 36,
                        color = "#666",
                        fontFamily= 'Bangers',
                        fontWeight = "normal")) %>% 
            e_flip_coords() %>%
            e_legend(show = FALSE) %>%
            e_x_axis(show=FALSE, splitLine=list(show = FALSE)) %>%
            e_y_axis(axisTick=list(show=FALSE), axisLine=list(show=FALSE),
                     splitLine=list(show = FALSE),
                     axisLabel = list(
                       fontSize = 16,
                       color = "#999",
                       fontFamily= 'Racing Sans One',
                       fontWeight = "normal")) %>%
            e_labels(fontSize = 20, 
                     fontFamily= "Racing Sans One",
                     color = "#999",
                     fontWeight ='normal', 
                     position = "right", offset=c(10, 0),
                     formatter="{@[0]}%") %>% 
            e_toolbox_feature(feature = c("saveAsImage"),
                              title = "Save",
                              icon = 'image://data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOEAAADhCAMAAAAJbSJIAAAARVBMVEX///8AAAANDQ2GhoZeXl4qKirPz8+KiorU1NS6urpqamqlpaWdnZ2WlpaRkZEREREvLy/CwsK0tLStra0fHx8aGhohISGb2hODAAABAklEQVR4nO3dy3HDMAxFUUSObMuW/I3Tf6mpIQsMBtQ5Fby7JTlSBAAAAAAAAAAAAABAL+s8jWveIravsd3iXj0h2RTVC9Ip7E9hfwr7U9ifwv4U9qewv5irFyR7xFI9Idkz4ng6jOt0rj4GAwAAAAAAYCjL5Xtcl1fEu/oKM9k1fqonJPvs4KVC9YB0CvtT2J/C/hT2p7A/hf3toPBTvSDZb1yrJyR7R7yGPqdZqo/BAAAAAAAAGMp56G8MHSOe1VeYyZZ4VE9INu/gpUL1gHQK+1PYn8L+FPansD+F/e2gcKpekOwet+oJydaIbeh/dq3Vx2AAAAAAAAAAAAAAAP/0B2wtQ4n+W64CAAAAAElFTkSuQmCC') %>% 
            e_grid(
                left = '20%',
                bottom = 30)

        
    })
    
    output$line_age <- renderHighchart({
    
      if(input$country=='Overall') {
        
        age_tibble <- data %>%
            group_by(year, age) %>%
            summarise(s1 = round((sum(suicides_no)/sum(population))*100000, 2))
        
        age_tibble$age <- factor(age_tibble$age, levels = c( "5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years" ))
        age_tibble <- age_tibble[order(age_tibble$age,age_tibble$age),]
        
      }
      
      else {
        
        age_tibble <- data %>%
          filter(country == input$country) %>% 
          group_by(year, age) %>%
          summarise(s1 = round((sum(suicides_no)/sum(population))*100000, 2))
        
        age_tibble$age <- factor(age_tibble$age, levels = c( "5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years" ))
        age_tibble <- age_tibble[order(age_tibble$age,age_tibble$age),] 
        
      }
        
        
        # Pick color for graph. 
        age_color <- rev(viridis::magma(6))
        
        # Create line plot.
        highchart() %>% 
            hc_add_series(age_tibble, hcaes(x = year, y = s1, group = age), type = "line", color = age_color) %>%
            hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "", pointFormat = paste("Year: <b>{point.x}</b> <br>","Age: <b>{point.age}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
          hc_title(text = "Worldwide suicides by Age",
                   style = list(
                     fontFamily = "Bangers",
                     color = "#666",
                     fontWeight = "normal",
                     fontSize = "36px"
                   )) %>% 
            hc_xAxis(title = list(text = "Year")) %>%
            hc_yAxis(title = list(text = "Suicides per 100K people"),
                     allowDecimals = FALSE) %>% 
            hc_exporting(enabled = TRUE)
        
    })
        
     }
)

#### ==== END ====####
