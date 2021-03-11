# Load libraries ----------------------------------------------------------
library(shiny)
library(shinyjs)
library(TESAcarbon)
library(maps)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
    titlePanel("TESA Carbon - Carbon footprint estimator for meetings and courses"),
    navlistPanel(
        widths=c(3,9),
        tabPanel(
            "Single traveller",
            h3("Calculate the carbon footprint for a single traveller"),
            fluidRow(
                column(width=6,
                    h4("Travel"),
                    helpText("For all of the fields below, please include return trip distance."),
                    flowLayout(
                        textInput("plane.distance", 
                            "Total distance travelled by airplane (in kilometers)", 
                            # value="Distance in kilometers",
                            placeholder="Enter a number, exclude 'km'"),
                        textInput("bustrain.distance", 
                            "Total distance travelled by bus or train (in kilometers)", 
                            # value="Distance in kilometers",
                            placeholder="Enter a number, exclude 'km'"),
                        textInput("car.distance", 
                            "Total distance travelled by car (in kilometers)", 
                            # value="Distance in kilometers",
                            placeholder="Enter a number, exclude 'km'"),
                        numericInput("number.car.sharing", 
                            "How many passengers were in your car?", 
                            # value="Distance in kilometers",
                            value=1,
                            min=1,
                            max=10,
                            step=1
                        )
                    )
                ),
                column(width=6,
                    h4("Hotel nights and meals"),
                    fluidRow(
                        textInput("hotel.nights",
                            "How many nights in a hotel will you stay?",
                            placeholder="Enter a number"
                        ),
                        textInput("meals", 
                            "How many restaurant meals?",
                            placeholder="Enter a number"
                        ),
                        hr(),
                        actionButton("calc", "Estimate carbon footprint", width="50%"),
                        tags$head(
                            tags$style(HTML('#calc{background-color:DodgerBlue;
                                color: white}'))
                        )
                    )
                )
            ),
            fluidRow(
                hr(),
                textOutput("cf.plane"),
                textOutput("cf.bustrain"),
                textOutput("cf.car"),
                textOutput("cf.hotel"),
                textOutput("cf.meal"),
                br(),
                textOutput("cf"),
                tags$head(tags$style("#cf{color: DodgerBlue;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                    )
                )
            )
        ),
        tabPanel(
            "Multiple travellers",
            column(width=6,
                helpText("Here is some example ICES data. You can download a template, fill this in with your own travellers' data, and re-upload."),
                downloadButton("downloadTemplate", "Download")
            ),
            column(width=6,
                fileInput("uploadedCSV", "Choose a CSV file for upload (maximum 5MB)", multiple = FALSE, accept = c("test/csv", "text/comma-separated-values", "text/plain", ".csv"))
            ),
            fluidRow(
                p("The first five rows of your uploaded data are shown below."),
                column(width=10,
                    tableOutput("uploaded")
                ),
                actionButton("runMultipleTravellers"),
                uiOutput("bad_destination"),
                uiOutput("mult_destination"),
                uiOutput("origin_check")
            ),
#            hr(),
#            textOutput("multi.cf"),
            hr(),
            plotOutput(outputId = "mappedOutput")
        )
    )
)

# Server ------------------------------------------------------------------
server <- function(input, output){

    # Local copy/mod of distance lookup:
    distance.lookup.fmod= function(input.data){
        origin.vector= input.data$origin
        destination.vector= input.data$destination
        countries= unique(input.data$origin.country)
        datacitycountry= paste0(input.data$origin, input.data$origin.country)
        # lookup distances between locations
        cities= as.data.table(world.cities)[country.etc %in% countries]
          #error check cities
          if(any(!(origin.vector %in% cities$name)))
            stop(paste(origin.vector[!(origin.vector %in% cities$name)], ": this origin city country combination does not have an entry in the world cities database. Options:
              (1) Names are generally English but not always, e.g. use Goteborg and not Gothenburg
              (2) Do not use accents
              (3) Centres with populations < 1000 are often not in the database. Try a close larger centre
              (4) Try fuzzy matching your centre with the first three letters in quotation marks and in sentence case, e.g.
                  world.cities[grep('Got', world.cities$name),]
                  and use the city name in the 'name' and 'country.etc' column in your input data.sheet"))
        cities$citycountry= paste0(cities$name,cities$country.etc)
        cities= cities[citycountry %in% datacitycountry]
        # To-from matrix 
        city.matrix= CJ(name=cities$name, name1=cities$name,unique=TRUE)
        tmp= merge(
            x=city.matrix, 
            y=cities[,.(name,lat,long)], 
            by.x = "name", 
            by.y = "name", 
            allow.cartesian=TRUE)
        city.position= merge(
            x=tmp, 
            y=cities[,.(name,lat,long)], 
            by.x = "name1", 
            by.y = "name", 
            allow.cartesian=TRUE)
        city.position= data.table(
            origin=city.position$name1, 
            destination=city.position$name, 
            long.origin=city.position$long.y,
            lat.origin=city.position$lat.y, 
            long.destination= city.position$long.x, 
            lat.destination= city.position$lat.x)
        city.position$distance= distGeo(city.position[,.(long.origin,lat.origin)],
                                        city.position[,.(long.destination,lat.destination)])/1000

        ### This here is useful for later manipulation: 

        city.position= city.position[origin %in% unique(origin.vector) | destination %in% unique(destination.vector)]
        city.position$city.combo= paste0(city.position$origin,city.position$destination)
        distances=vector(length=length(origin.vector))
        for (i in 1:length(origin.vector)){
          distances[i]= city.position[city.position$origin==origin.vector[i] &
                    city.position$destination==destination.vector[i],]$distance
        }
        distances= round(distances,0)*input.data$flying

        #lat and long of cities
        origin.locations= cities[ name %in% unique(origin.vector)]
        destination.locations= cities[ name %in% unique(destination.vector)]
        localisation= list(distance= distances, origin.locations= origin.locations,
          destination.locations= destination.locations)
        localisation
    }

    # Local copy of the carbon footprint calculator:
    carbon.footprint.fmod= function(input, localization, Title.name="Carbon footprint", list.out=T){
      ## localisation= distance.lookup.f(input)
      localisation$origin.locations$capital=0
      localisation$destination.locations$capital=1
      cities= localisation$origin[!(name %in% localisation$destination.locations$name)]
      cities= rbind(localisation$destination,cities)
      input$plane.distance= localisation$distance
      input$C= C.f(hotel.nights=input$hotel.nights,
        plane.distance=input$plane.distance*2,
        bustrain.distance= input$bustrain.distance*2,
        car.distance= input$car.distance*2,
        number.car.sharing=input$car.sharing,
        meals=input$meals)
      total.per.activity= round(tapply(input$C,input$activity.name,sum),3)
      participants.per.activity= tapply(input$C,input$activity.name,length)
      per.capita.per.activity=round(total.per.activity/participants.per.activity,3)
      mean.per.capita= round(mean(per.capita.per.activity),3)
      total.C= round(sum(input$C),3)
      tab1= data.frame(Activity=names(total.per.activity),
        Total=total.per.activity,
        Participation=participants.per.activity,
        C.per.person=per.capita.per.activity)
      location= input[,.(unique(destination),unique(activity.type)),activity.name]
      tab1$location= location$V1[match(tab1$Activity,location$activity.name)]
      tab1$type= location$V2[match(tab1$Activity,location$activity.name)]
      tab= list(carbon= tab1, countries= unique(input$origin.country),locations= cities)
      tab

      mapbar= function(C.emissions){
        carbon= ggplot(tab$carbon,aes(y=Total,x=Activity))+
          geom_col(show.legend = F,size=.5) +
          ylim(0,max(tab$carbon$Total)*1.2)+
          annotate("text",x=1:nrow(tab$carbon),y=rep(max(tab$carbon$Total)*1.15,rep=nrow(tab$carbon)),label=tab$carbon$location,cex=2) +
          annotate("text",x=1:nrow(tab$carbon),y=rep(max(tab$carbon$Total)*1.05,rep=nrow(tab$carbon)),label=tab$carbon$type,cex=2) +
          annotate("text",x=1:nrow(tab$carbon),y=rep(max(tab$carbon$Total)/4,rep=nrow(tab$carbon)),
            label=paste0("per capita=",round(tab$carbon$C.per.person,2)),cex=3,col="orange")+
          theme_classic()+
          aes(stringr::str_wrap(Activity, 15))+
          ylab("Carbon emissions (t)")+
          xlab(NULL)+
          coord_flip()
    
        map=ggplot(as.data.table(map_data("world")), aes(x = long, y = lat, group = group)) +
          geom_polygon(fill="lightgray", colour = "white") +
          geom_point(data = tab$locations,aes(long, lat, group = name)) +
          geom_point(data = tab$locations[capital==1],aes(long, lat, group = name),col='red',pch=21,size=2,stroke=2) +
          geom_text_repel(data = tab$locations,aes(long, lat,label = name,group = name),color = 'black', size  = 3,
            box.padding = 0.7, point.padding = 0.5) +
          theme_void()
    
        plots= plot_grid(map,carbon,nrow=2)
        title <- ggdraw() +
          draw_label(paste0(Title.name, ', Total C = ',round(sum(tab$carbon$Total),1),' t'), fontface = 'bold', x = 0, hjust = 0) +
          theme(plot.margin = margin(0, 0, 0, 7))
          plot_grid(title, plots, ncol = 1,rel_heights = c(0.1, 1))
      }
      print(mapbar(tab))
      if (list.out) tab
    } 

    # First panel: individual calculation
    observeEvent(input$calc, {
        # Do some simple replacements to remove "km" and "kilometers" instances
        if(is.na(input$plane.distance)){
            updateTextInput(session, "plane.distance", value=0)
        }
        if(is.na(input$bustrain.distance)){
            updateTextInput(session, "bustrain.distance", value=0)
        }
        if(is.na(input$car.distance)){
            updateTextInput(session, "car.distance", value=0)
        }
        if(is.na(input$hotel.nights)){
            updateTextInput(session, "hotel.nights", value=0)
        }
        if(is.na(input$meals)){
            updateTextInput(session, "meals", value=0)
        }

        plane.distance <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$plane.distance))
        bustrain.distance <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$bustrain.distance)) # as.numeric(gsub("kilometers", "", x=gsub(as.character("km", "", x=input$bustrain.distance))))
        car.distance <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$car.distance)) # as.numeric(gsub("kilometers", "", x=gsub(as.character("km", "", x=input$car.distance))))
        hotel.nights <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$hotel.nights)) # as.numeric(as.character(gsub("nights", "", x=input$hotel.hights)))
        meals <- as.numeric(gsub(".*?([0-9]+).*", "\\1", input$meals))# as.numeric(as.character(gsub("meals", "", x=input$meals)))
        
        # print(paste0("Plane: ", plane.distance))
        # print(paste0("Bus: ", bustrain.distance))
        # print(paste0("Car: ", car.distance))
        # print(paste0("Hotel: ", hotel.nights))
        # print(paste0("Meals: ", meals))

        calculated_cf <- TESAcarbon::C.f(
            hotel.nights=ifelse(is.na(hotel.nights), 0, hotel.nights),
            plane.distance=ifelse(is.na(plane.distance), 0, plane.distance),
            bustrain.distance=ifelse(is.na(bustrain.distance), 0, bustrain.distance),
            car.distance=ifelse(is.na(car.distance), 0, car.distance),
            number.car.sharing=input$number.car.sharing,
            meals=ifelse(is.na(meals), 0, meals)
        )
        output$cf.hotel <- renderText(paste0("Carbon footprint for hotel: ", 
            signif(TESAcarbon::carbon.params$C.hotel[1] * hotel.nights + TESAcarbon::carbon.params$C.hotel[2], 3),
            " tonnes"))
        output$cf.plane <- renderText(paste0("Carbon footprint for airplane travel: ", 
            signif(TESAcarbon::carbon.params$C.plane[1] * plane.distance + TESAcarbon::carbon.params$C.plane[2], 3),
            " tonnes"))
        output$cf.bustrain <- renderText(paste0("Carbon footprint for bus and train travel: ", 
            signif(TESAcarbon::carbon.params$C.bustrain[1] * bustrain.distance + TESAcarbon::carbon.params$C.bustrain[2], 3),
            " tonnes"))
        output$cf.car <- renderText(paste0("Carbon footprint for car travel (per passenger): ", 
            signif((TESAcarbon::carbon.params$C.car[1] * car.distance + TESAcarbon::carbon.params$C.car[2]) / input$number.car.sharing, 3),
            " tonnes"))
        output$cf.meal <- renderText(paste0("Carbon footprint for meals: ", 
            signif((TESAcarbon::carbon.params$C.meal[1] * meals + TESAcarbon::carbon.params$C.meal[2]) * TESAcarbon::carbon.params$C.meal.discount, 3), 
            " tonnes"))
        output$cf <- renderText(paste0("Total estimated carbon footprint: ", signif(calculated_cf, 3), " tonnes"))
    })
    # Second, CSV reader and multi-person app

    # Provide ICES data as a template
    template_df <<- data.frame(TESAcarbon::ICES)# [1:20,]
    output$downloadTemplate <- downloadHandler(
        # Returns a string which indicates what name to use when saving the file
        filename = "TESACarbonCalculator_multiple_traveller_template.csv",
        content = function(file) {
            write.table(template_df, file, sep = ",", row.names = FALSE)
        }
    )
    upload.df <<- reactiveValues()
    # File upload 
    output$uploaded <- renderTable({
        req(input$uploadedCSV)
        df <- read.csv(input$uploadedCSV$datapath,
            header=TRUE,
            sep=","
        )
        assign('upload.df', df, env=.GlobalEnv)
        cat(names(upload.df))
        cat(class(upload.df))
        return(upload.df[1:5,])
    })

    bad_destinations <<- vector()
    multiple_destination <<- vector()
    shinyjs::disable("runMultipleTravellers")
    observeEvent(upload.df(), {
        if(!is.null(nrow(upload.df()))){
            shinyjs::enable("runMultipleTravellers")
            # First, ensure that the destinations are real cities
            origin.vector <- upload.df()$origin
            destination.vector <- upload.df()$destination
            countries <- unique(upload.df()$origin.country)
            datacitycountry <- paste0(upload.df()$origin, upload.df()$origin.country)
            # lookup distances between locations
            cities <- as.data.table(maps::world.cities)[country.etc %in% countries]
            for(dest in destination.vector){
                destination_present <- which(maps::world.cities == dest)
                if(length(destination_present) == 0){
                    bad_destination <- append(bad_destination, dest)
                } else if(length(destination_present > 1)){
                    multiple_destination <- append(multiple_destination, dest)
                }
            }
            if(length(multiple_destination) > 0){
                output$mult_destination <- renderUI({
                    lapply(1:length(multiple_destination), function(val) {
                        test <- data.frame(world.cities[which(maps::world.cities == multiple_destination[dest]),c("name", "country.etc"))
                        test$choices <- paste0(test$name, ", ", test$country.etc, " (",test$lat, ", ", test$long, ")")
                        choice_list <- list(rownames(test))
                        names(choice_list) <- test$choices
                        fluidRow(column(12,
                            selectInput(paste0("dest_", val), paste0("The destination city ", mult_destination[val], " matches multiple cities. Please select the correct one:"), choices=choice_list)
                        ))
                    })
                })
                # upload.df()$destination[which(upload.df()$destination == dest)] <- 
            }
            if(length(bad_destination) > 0){
                output$bad_destination <- renderUI({
                    lapply(1:length(bad_destination), function(val) {
                        fluidRow(column(12,textOutput(paste0("Destination ", bad_destination[val], " matches no known cities. Please verify that you are using English spelling. If the city has less than 1000 people, it is unlikely to be in the database. If this is the case, please choose a larger city that is close by."))))
                    })
                })
            }
            }
        }
    })
    # 
    output$mappedOutput <- renderPlot({
        
        # ...
        req(input$uploadedCSV)
        TESAcarbon::carbon.footprint.f(data.table(upload.df), "", list.out=F)
    })

    observeEvent(input$uploadedCSV, {
      origins <- unique(upload.df$origin)
      for(o in 1:length(origins)){
          #### This is probably important
      }
      for(d in 1:length())
      output$city_problems <- renderUI({
          lapply()
        # h6("What if there are city name problems?")
      })
    })
}

shiny::shinyApp(ui = ui, server = server)