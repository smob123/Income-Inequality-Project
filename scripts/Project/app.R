#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

# read the translated data file
incomes <-
    read.csv("../../data/translated_data.csv", stringsAsFactors = FALSE)

# calculate incomes based on whether an indivudual is over or under 40 years old
over.40 <- incomes[incomes$agegrp >= 40,]
under.40 <- incomes[incomes$agegrp < 40,]

over.40.total <- sum(over.40[, "income"])
under.40.total <- sum(under.40[, "income"])

# add the result to a dataframe to plot it
younger.older.40.df <-
    data.frame(
        age_group = c("Over 40", "Under 40"),
        income_totals = c(over.40.total, under.40.total)
    )

# calculate total incomes based on age groups
get.income.by.age <- function() {
    ages <- c(15, 20, 25, 30, 35, 40, 45, 60, 65)
    a <- c()
    
    for (i in 1:length(ages)) {
        x <- incomes[incomes$agegrp == ages[i],]
        a[i] <- sum(x$income)
    }
    
    return(a)
}

a <- get.income.by.age()

# add the incomes by age into a dataframe to be plotted
incomes.by.age.df <-
    data.frame(agegrp = c(15, 20, 25, 30, 35, 40, 45, 60, 65),
               income_totals = a)


# calculate most common qualifications
qualifications.counts <- table(qualification = over.40$qualification)
most.common.qualifications <- data.frame(qualifications.counts)

# calculate most common occupations
occupations.counts <- table(occupation = over.40$occupation)
most.common.occupations <- data.frame(occupations.counts)

# get the maximum total income, and the age group that is associated with it
max.total.income <- max(incomes.by.age.df$income_totals)
max.total.age <-
    incomes.by.age.df[incomes.by.age.df$income_totals == max.total.income, ]
max.total.agegrp <- paste0(max.total.age, "-", max.total.age + 4)

# get the minimum total income, and the age group that is associated with it
min.total.income <- min(incomes.by.age.df$income_totals)
min.total.age <-
    incomes.by.age.df[incomes.by.age.df$income_totals == min.total.income, ]
min.total.agegrp <- paste0(min.total.age, "-", min.total.age + 4)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Income Distribution in New Zealand"),
    sidebarLayout(# side bar
        sidebarPanel(# minimum income total, and the age group that is associated with it
            div(
                p("Min Income Total:"),
                p(paste0(
                    min.total.income, " (", min.total.agegrp, ")"
                ))
            ),
            # maximum income total, and the age group that is associated with it
            div(
                p("Max Income Total:"),
                p(paste0(
                    max.total.income, " (", max.total.agegrp, ")"
                ))
            ),
            # mean of all total incomes
            div(
                p("Mean Income Total: "),
                p(mean(incomes.by.age.df$income_totals))
            )),
        # Show a plots inside of a tabset in a tab panel
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Younger and Older Than 40",
                    plotOutput("younger.older.40"),
                    verbatimTextOutput("younger.older.40.info")
                ),
                tabPanel(
                    "All Total Incomes",
                    plotOutput("incomes.by.age", click = "incomes.by.age.click"),
                    verbatimTextOutput("incomes.by.age.info")
                ),
                tabPanel(
                    "Common Qualifications",
                    plotOutput("most.common.qualifications"),
                    verbatimTextOutput("most.common.qualifications.info")
                ),
                tabPanel(
                    "Common Occupations",
                    plotOutput("most.common.occupations"),
                    verbatimTextOutput("most.common.occupations.info")
                )
            )
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$younger.older.40 <- renderPlot({
        # plot people who are older, or younger than 40 years old
        ggplot(younger.older.40.df,
               aes(
                   x = "",
                   y = income_totals,
                   fill = age_group
               )) +
            geom_bar(stat = "identity") +
            ylab("Sum of Incomes for Each Age Group") +
            coord_polar("y", start = 0)
    })
    
    output$younger.older.40.info <- renderPrint({
        print(younger.older.40.df)
    })
    
    # plot total incomes of all age groups
    output$incomes.by.age <- renderPlot({
        ggplot(incomes.by.age.df) +
            geom_point(aes(x = agegrp, y = income_totals), size = 3) +
            xlab("Age groups") +
            ylab("Sum of Incomes")
    })
    
    output$incomes.by.age.info <- renderPrint({
        # get the x coordinate, which should represent an age group
        xPos <- input$incomes.by.age.click$x
        
        # check that it is a valid number
        if (!(is.na(xPos) || is.null(xPos)) && is.numeric(xPos)) {
            # round the result down, and store it as the row number
            rowNum <- floor(xPos)
            
            # get the data of the age group
            rowData <-
                incomes.by.age.df[incomes.by.age.df$agegrp == rowNum, ]
            
            # check if there is a value in rowData
            if (length(rowData$agegrp) < 1) {
                # print an "error" message
                print("Nothing to show")
            } else {
                # otherwise get the age group from the row
                if (rowData$agegrp < 65) {
                    agegrp <- paste0("Age Group: ",
                                     rowData$agegrp,
                                     "-",
                                     rowData$agegrp + 4)
                } else {
                    agegrp <- paste0("Age Group: ", "65+")
                }
                
                # get the total of incomes of thatg age group
                total.incomes <-
                    paste0("Total incomes: ", rowData$income_totals)
                
                # get the mean of incomes
                mean.incomes <- paste0("Mean of Incomes: ",
                                       mean(rowData$income_totals))
                
                # print all the previus variabkes in seperate lines
                cat(paste0(
                    agegrp,
                    "\n",
                    total.incomes,
                    "\n",
                    mean.incomes
                ))
            }
        }
    })
    
    # plot most common qualifications for people who are 40 and above
    output$most.common.qualifications <- renderPlot({
        ggplot(most.common.qualifications) +
            geom_bar(aes(
                x = qualification,
                y = Freq,
                fill = qualification
            ), stat = "identity")
    })
    
    # print the information of the dataframe
    output$most.common.qualifications.info <- renderPrint({
        print(most.common.qualifications)
    })
    
    # plot most common occupations for people who are 40 and above
    output$most.common.occupations <- renderPlot({
        ggplot(most.common.occupations) +
            geom_bar(aes(
                x = occupation,
                y = Freq,
                fill = occupation
            ), stat = "identity") +
            coord_flip()
    })
    
    # print the information of the dataframe
    output$most.common.occupations.info <- renderPrint({
        print(most.common.occupations)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
