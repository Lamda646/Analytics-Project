
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load the Airbnb data (assuming it's stored in a CSV file)
load('C:..../Airbnb/AirBnB.Rdata')




# Define the UI
ui <- fluidPage(
  titlePanel("Explore Paris AirBnB Data"),
  
    tabsetPanel(
      tabPanel("Relation between Price and Apartment Features", 
          sidebarLayout(
             sidebarPanel(
                selectInput(inputId = "feature", label = "Apartment Features:",
                             choices = c("bedrooms", "bathrooms", "accommodates", "beds","guests_included", "weekly_price", "monthly_price"),
                             selected = "bedrooms")
               
                         ),
             mainPanel(
                  plotOutput(outputId = "scatter_plot")
                      ))
   
      ),
 
      tabPanel( "Number of Apartments per owner",
             dataTableOutput(outputId = "owners_table")),
    
      tabPanel("Renting Price per City Quarter",
             dataTableOutput(outputId = "price_table")),
      
      tabPanel("Visiting frequency per Quarter",
               plotOutput(outputId= "visit_frequency_plot"))
  )
)




# Define the server logic
server <- function(input, output) {
  # Create the scatter plot
  output$scatter_plot <- renderPlot({
    price_range <- cut(apartment_data$price, breaks = c(0, 100, 200, 300, 400,500,600,700,800,900,1000, Inf), #breaking the prices into range
                       labels = c("0-100", "101-200", "201-300", "301-400", "401-500", "501-600", "601-700", "701-800", "801-900", "901-1000", "1001+"),
                       include.lowest = TRUE,
                       right = FALSE)
    
    
    
    
    ggplot(apartment_data, aes_string(x = price_range  , y = input$feature , color = input$feature)) +
      geom_point() +
      labs(x = "Price Range"  , y = input$feature  ) +
      scale_color_gradient(low = "blue", high = "red") + 
      theme_minimal()
  })


# Number of apartments per owner 
  #A reactive expression is an R expression that uses widget input and returns a value
   apartment_count <- reactive({
     table(apartment_data$host_id) #Here apartment_count counts the number of apartments per host_id
   })
   
  # renderDataTable returns a Data Frame 

   output$owners_table <-renderDataTable({
     owner_counts <- data.frame(
       Owner_ID = names(apartment_count()), # Here we are taking the names or the ID of the apartment_count()
       "Host Name" = apartment_data$host_name[match(names(apartment_count()), apartment_data$host_id)], # here we are taking the host_name corresponding to the host_id
       "Apartment Count" = as.vector(apartment_count())) # Here we are displaying the apartment_count() as a vector
   
        owner_counts 
   
   })
  
   output$price_table <- renderDataTable({
     price_per_arrondissement <- arrondiss_zipcode
       
   })

  
   
   # To plot visting frequency per quarter
   output$visit_frequency_plot <- renderPlot({
     
     
     ggplot(visit_freq, aes(x =quarter , y = visit_count)) +
       geom_bar(stat = "identity", fill = "skyblue") +
       xlab("Quarter") +
       ylab("Visit Frequency") +
       ggtitle("Visit Frequency per Quarter")
       
       
   })
   
   
}
# Run the application
shinyApp(ui = ui, server = server)



























