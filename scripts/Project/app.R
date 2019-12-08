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
over.40 <- incomes[incomes$agegrp >= 40, ]
under.40 <- incomes[incomes$agegrp < 40, ]

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
    ages <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
    totals <- c()
    
    for (i in 1:length(ages)) {
        x <- incomes[incomes$agegrp == ages[i], ]
        totals[i] <- sum(x$income)
    }
    
    return(totals)
}

totals <- get.income.by.age()

# add the incomes by age into a dataframe to be plotted
incomes.by.age.df <-
    data.frame(agegrp = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
               income_totals = totals)


# calculate qualification frequencies
qualifications.counts <-
    table(qualification = over.40$qualification)
qualification.frequency <- data.frame(qualifications.counts)


# calculate the qualification frequencies for people who earn at least $1
over.40.filter.one_dollar <-
    over.40[over.40$income >= 1,]

qualifications.counts.one_dollar <-
    table(qualification = over.40.filter.one_dollar$qualification)

qualification.frequency.one_dollar <-
    data.frame(qualifications.counts.one_dollar)


# calculate the common qualifications for the top 1% of 40+ year olds
over.40.ordered <- order(over.40)
over.40.ordered <- over.40[over.40.ordered,]

one.percent <- ceiling(nrow(over.40.ordered) * 0.1)

over.40.ordered <- over.40.ordered[1:one.percent,]

qualification.frequency.top_one_percent <-
    over.40[over.40.ordered$income >= 1, "qualification"]

qualifications.counts.top_one_percent <-
    table(qualification = qualification.frequency.top_one_percent)

qualification.frequency.top_one_percent <-
    data.frame(qualifications.counts.top_one_percent)


# calculate occupation frequencies
occupations.counts <- table(occupation = over.40$occupation)
occupation.frequencies <- data.frame(occupations.counts)


# calculate occupation frequency for everyone who earns at least $1
occupation.counts.one_dollar <-
    table(occupation = over.40.filter.one_dollar$occupation)
occupation.frequency.one_dollar <-
    data.frame(occupation.counts.one_dollar)

occupation.frequency.top_one_percent <-
    over.40[over.40.ordered$income >= 1, "occupation"]

occupation.counts.top_one_percent <-
    table(occupation = occupation.frequency.top_one_percent)

occupation.frequency.top_one_percent <-
    data.frame(occupation.counts.top_one_percent)


# get the maximum total income, and the age group that is associated with it
max.total.income <- max(incomes.by.age.df$income_totals)
max.total.age <-
    incomes.by.age.df[incomes.by.age.df$income_totals == max.total.income,]
max.total.agegrp <- paste0(max.total.age, "-", max.total.age + 4)


# get the minimum total income, and the age group that is associated with it
min.total.income <- min(incomes.by.age.df$income_totals)
min.total.age <-
    incomes.by.age.df[incomes.by.age.df$income_totals == min.total.income,]
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
                p("Mean of all Income Totals: "),
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
                    "Qualification Frequencies",
                    selectInput(
                        "common.qualifications.filter",
                        "Filter:",
                        c(
                            "none" = "all",
                            "Earn at least $1" = "$1 or more",
                            "Top 1%" = "top 1%"
                        ),
                        selected = "all"
                    ),
                    plotOutput("qualification.frequency"),
                    verbatimTextOutput("qualification.frequency.info")
                ),
                tabPanel(
                    "Occupation Frequencies",
                    selectInput(
                        "common.occupations.filter",
                        "Filter:",
                        c(
                            "none" = "all",
                            "Earn at least $1" = "$1 or more",
                            "Top 1%" = "top 1%"
                        ),
                        selected = "all"
                    ),
                    plotOutput("occupation.frequencies"),
                    verbatimTextOutput("occupation.frequencies.info")
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
                incomes.by.age.df[incomes.by.age.df$agegrp == rowNum,]
            
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
                
                # get the list of incomes related to the
                # selected age group from the original set
                age.group <- incomes[incomes$agegrp == rowNum, ]
                
                # get the mean of incomes
                mean.incomes <- paste0("Mean of Incomes: ",
                                       round(mean(age.group$income), digits = 2))
                
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
    
    # plot qualification frequencies for people who are 40 and above
    output$qualification.frequency <- renderPlot({
        plot.filter <- input$common.qualifications.filter
        
        if (plot.filter == "all") {
            mcqPlot <- qualification.frequency
        } else if (plot.filter == "$1 or more") {
            mcqPlot <- qualification.frequency.one_dollar
        } else if (plot.filter == "top 1%") {
            mcqPlot <- qualification.frequency.top_one_percent
        }
        
        ggplot(mcqPlot) +
            geom_bar(aes(
                x = qualification,
                y = Freq,
                fill = qualification
            ),
            stat = "identity")
    })
    
    # print the information of the dataframe
    output$qualification.frequency.info <- renderPrint({
        plot.filter <- input$common.qualifications.filter
        
        if (plot.filter == "all") {
            mcqPlot <- qualification.frequency
        } else if (plot.filter == "$1 or more") {
            mcqPlot <- qualification.frequency.one_dollar
        } else if (plot.filter == "top 1%") {
            mcqPlot <- qualification.frequency.top_one_percent
        }
        
        print(mcqPlot)
    })
    
    # plot occupation frequencies for people who are 40 and above
    output$occupation.frequencies <- renderPlot({
        plot.filter <- input$common.occupations.filter
        
        if (plot.filter == "all") {
            mcoPlot <- occupation.frequencies
        } else if (plot.filter == "$1 or more") {
            mcoPlot <- occupation.frequency.one_dollar
        } else if (plot.filter == "top 1%") {
            mcoPlot <- occupation.frequency.top_one_percent
        }
        
        ggplot(mcoPlot) +
            geom_bar(aes(
                x = occupation,
                y = Freq,
                fill = occupation
            ), stat = "identity") +
            coord_flip()
    })
    
    # print the information of the dataframe
    output$occupation.frequencies.info <- renderPrint({
        plot.filter <- input$common.occupations.filter
        
        if (plot.filter == "all") {
            mcoPlot <- occupation.frequencies
        } else if (plot.filter == "$1 or more") {
            mcoPlot <- occupation.frequency.one_dollar
        } else if (plot.filter == "top 1%") {
            mcoPlot <- occupation.frequency.top_one_percent
        }
        
        print(mcoPlot)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
