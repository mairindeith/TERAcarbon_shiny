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
                p("The first five rows of your uploaded data are shown below"),
                column(width=10,
                    tableOutput("uploaded")
                ),
                uiOutput("city_problems")
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
        return(upload.df[1:3,])
    })
#    output$multi.cf <- renderText({
#    })
#    )
    output$mappedOutput <- renderPlot({
        # ...
        req(input$uploadedCSV)
        TESAcarbon::carbon.footprint.f(data.table(upload.df), "", list.out=F)
    })

    output$city_problems <- renderUI({
        h6("What if there are city name problems?")
    })
}

shiny::shinyApp(ui = ui, server = server)