#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# make sure to install the following packages before running the app:
# install.packages("dplyr")
# install.packages("ggplot2")

library(shiny)
library(dplyr)
library(ggplot2)

# read the translated data file
incomes <-
    read.csv("../../data/translated_data.csv", stringsAsFactors = FALSE)

# get the number of people in the dataset
num.of.rows <- nrow(incomes)

# get the number of people in each age group
sample.sizes <- table(incomes$agegrp)

# fix column names to display age ranges properly
names(sample.sizes) <-
    paste0(names(sample.sizes), "-", as.numeric(names(sample.sizes)) + 4)
names(sample.sizes)[11] <- "65+"

# store the result in a dataframe, and name the columns properly
sample.sizes.df <- as.data.frame(sample.sizes)
colnames(sample.sizes.df) <- c("Age Group", "Sample Size")

# calculate average incomes based on age groups
get.income.by.age <- function() {
    ages <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
    averages <- c()
    
    for (i in 1:length(ages)) {
        x <- incomes[incomes$agegrp == ages[i],]
        averages[i] <- round(mean(x$income), digits = 2)
    }
    
    return(averages)
}

averages <- get.income.by.age()

# add the income averages by age into a dataframe
incomes.by.age.df <-
    data.frame(agegrp = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
               income_averages = averages)
# get the maximum average income, and the age group that is associated with it
max.average.income <- max(incomes.by.age.df$income_averages)
max.average.age <-
    incomes.by.age.df[incomes.by.age.df$income_averages == max.average.income, "agegrp"]
max.average.agegrp <-
    paste0(max.average.age, "-", max.average.age + 4)


# get the minimum average income, and the age group that is associated with it
min.average.income <- min(incomes.by.age.df$income_averages)
min.average.age <-
    incomes.by.age.df[incomes.by.age.df$income_averages == min.average.income, "agegrp"]
min.average.agegrp <-
    paste0(min.average.age, "-", min.average.age + 4)


# get the summary of the data to print it on the sidebar
data.summary.df <-
    data.frame(
        "Age Group" = c(max.average.agegrp, min.average.agegrp),
        "Average" = c(max.average.income, min.average.income)
    )

# calculate incomes based on whether an indivudual is over or under 40 years old
over.40 <- incomes[incomes$agegrp >= 40,]
under.40 <- incomes[incomes$agegrp < 40,]

over.40.average <- round(mean(over.40[, "income"]), digits = 2)
under.40.average <- round(mean(under.40[, "income"]), digits = 2)

# add the result to a dataframe
younger.older.40.df <-
    data.frame(
        age_group = c("Over 40", "Under 40"),
        income_averages = c(over.40.average, under.40.average)
    )

# get the number of people who have an occupation for both age groups
over.40.have.occupation <-
    over.40[over.40$occupation != "No Occupation", ]

under.40.have.occupation <-
    under.40[under.40$occupation != "No Occupation", ]


# get the list of people over 40 who earn at least $1 a week
over.40.filter.one_dollar <-
    over.40.have.occupation[over.40.have.occupation$income >= 1, ]

# get the list of people under 40 who earn at least $1 a week
under.40.filter.one_dollar <-
    under.40.have.occupation[under.40.have.occupation$income >= 1, ]

# get the list of the top 1% of 40+ year olds
over.40.ordered <-
    order(over.40.have.occupation$income, decreasing = TRUE)
over.40.ordered <- over.40.have.occupation[over.40.ordered, ]

one.percent <- ceiling(nrow(over.40.ordered) * 0.1)

over.40.top_one_percent <- over.40.ordered[1:one.percent, ]

# get the list of the top 1% of people under 40 years old
under.40.ordered <-
    order(under.40.have.occupation$income, decreasing = TRUE)
under.40.ordered <- over.40.have.occupation[under.40.ordered, ]

one.percent <- ceiling(nrow(under.40.ordered) * 0.1)

under.40.top_one_percent <- under.40.ordered[1:one.percent, ]

# calculates the frequencies of unique values in a dataframe's column
calculate.frequencies <- function(dataset, var) {
    # store the column's data in a time to calculate the frquencies of each value
    counts <-
        table(dataset[, var], dnn = c(var))
    
    # convert the table to a dataframe and return it
    return(data.frame(counts))
}

# calculate occupation frequencies
over.40.occupation.frequencies <-
    calculate.frequencies(over.40.have.occupation, "occupation")
under.40.occupation.frequencies <-
    calculate.frequencies(under.40.have.occupation, "occupation")


# calculate occupation frequency for everyone who earns at least $1
over.40.occupation.frequency.one_dollar <-
    calculate.frequencies(over.40.filter.one_dollar, "occupation")

under.40.occupation.frequency.one_dollar <-
    calculate.frequencies(under.40.filter.one_dollar, "occupation")

# calculate occupation frequency for the top 1%
over.40.occupation.frequency.top_one_percent <-
    calculate.frequencies(over.40.top_one_percent, "occupation")

under.40.occupation.frequency.top_one_percent <-
    calculate.frequencies(under.40.top_one_percent, "occupation")

# calculate average incomes based on the top 1% of each age group
get.top.one.percent.income.by.age <- function() {
    ages <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
    averages <- c()
    
    for (i in 1:length(ages)) {
        age.group <- incomes[incomes$agegrp == ages[i],]
        # get the list of the top 1% of 40+ year olds
        age.group.ordered <-
            order(age.group$income, decreasing = TRUE)
        age.group.ordered <- age.group[age.group.ordered, ]
        
        one.percent <- ceiling(nrow(age.group.ordered) * 0.1)
        
        age.group.top_one_percent <-
            age.group.ordered[1:one.percent, ]
        averages[i] <-
            round(mean(age.group.top_one_percent$income), digits = 2)
    }
    
    return(averages)
}
top.one.percents <- get.top.one.percent.income.by.age()

# add the income averages of top 1% by age into a dataframe
incomes.by.age.top.one.percent.df <-
    data.frame(agegrp = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
               income_averages = top.one.percents)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Income Inequality Distribution in New Zealand"),
    sidebarLayout(
        # side bar
        sidebarPanel(
            # summary of the dataset
            div(tags$b("Number of the People in the Dataset:"),
                p(num.of.rows)),
            div(tags$b("Sample Sizes")),
            tableOutput("sample.sizes"),
            div(tags$b("Mean of all Income Averages:"),
                p(round(
                    mean(incomes.by.age.df$income_averages), digits = 2
                ))),
            # minimum income average, and the age group that is associated with it
            div(
                style = "margin-top:20px;",
                tags$b("Maximum/Minimum Income Averages:"),
                tableOutput("data.summary")
            )
        ),
        # Show a plots inside of a tabset in a tab panel
        mainPanel(tabsetPanel(
            tabPanel(
                "Younger and Older Than 40",
                plotOutput("younger.older.40"),
                verbatimTextOutput("younger.older.40.info")
            ),
            tabPanel(
                "Occupations",
                # frequency of hours worked per week
                h3("Wrok Hours Per Week"),
                selectInput(
                    "hours.per.week.filter",
                    "Filter:",
                    c(
                        "none" = "all",
                        "Earn at least $1" = "$1 or more",
                        "Top 1%" = "top 1%"
                    ),
                    selected = "all"
                ),
                plotOutput("hours.per.week"),
                # frequencies of various occupations
                h3("Most Common Occupations"),
                fluidRow(# the first column contains the age group filter
                    column(
                        5,
                        radioButtons(
                            "occupations.age.groups.filter",
                            label = "age.group",
                            choices = c("40+", "under40"),
                            selected = "40+"
                        )
                    ),
                    # the second column contains income filter
                    column(
                        5,
                        selectInput(
                            "common.occupations.filter",
                            "Filter:",
                            c(
                                "none" = "all",
                                "Earn at least $1" = "$1 or more",
                                "Top 1%" = "top 1%"
                            ),
                            selected = "all"
                        )
                    )),
                plotOutput("occupation.frequencies"),
                verbatimTextOutput("occupation.frequencies.info")
            ),
            # plot of age group specific income averages
            tabPanel(
                "Average Incomes of the Top 1%",
                plotOutput("incomes.by.age", click = "incomes.by.age.click"),
                verbatimTextOutput("incomes.by.age.info")
            )
        ))
    )
)

# Define server logic required draw the plots
server <- function(input, output) {
    # print the sample sizes' dataframe in the sidebar
    output$sample.sizes <- renderTable({
        return(sample.sizes.df)
    })
    
    # print the summary of the max/min avergaes in the sidebar
    output$data.summary <- renderTable({
        return(data.summary.df)
    })
    
    # plot the average number of hours per week
    output$hours.per.week <- renderPlot({
        # get the chosen filter
        plot.filter <- input$hours.per.week.filter
        
        # filter the data based on the chosen filter
        if (plot.filter == "all") {
            hours.per.week <- data.frame(
                "age.group" = c("40+", "Under 40"),
                "hours" = c(
                    mean(over.40.have.occupation$hours),
                    mean(under.40.have.occupation$hours)
                )
            )
        } else if (plot.filter == "$1 or more") {
            hours.per.week <- data.frame(
                "age.group" = c("40+", "Under 40"),
                "hours" = c(
                    mean(over.40.filter.one_dollar$hours),
                    mean(under.40.filter.one_dollar$hours)
                )
            )
        } else if (plot.filter == "top 1%") {
            hours.per.week <- data.frame(
                "age.group" = c("40+", "Under 40"),
                "hours" = c(
                    mean(over.40.top_one_percent$hours),
                    mean(under.40.top_one_percent$hours)
                )
            )
        }
        
        ggplot(hours.per.week) +
            geom_bar(aes(
                x = age.group,
                y = hours,
                fill = age.group
            ), stat = "identity")
    })
    
    # compare average incomes for people who are older, or younger than 40 years old
    output$younger.older.40 <- renderPlot({
        ggplot(younger.older.40.df,
               aes(
                   x = "",
                   y = income_averages,
                   fill = age_group
               )) +
            geom_bar(stat = "identity") +
            ylab("Average of Incomes for Each Age Group") +
            coord_polar("y", start = 0)
    })
    
    # print a summary of the plot above
    output$younger.older.40.info <- renderPrint({
        print(younger.older.40.df)
    })
    
    
    # plot occupation frequencies for people who are 40 and above
    output$occupation.frequencies <- renderPlot({
        agegrp.filter <- input$occupations.age.groups.filter
        plot.filter <- input$common.occupations.filter
        
        if (plot.filter == "all") {
            if (agegrp.filter == "40+") {
                mcoPlot <- over.40.occupation.frequencies
            } else if (agegrp.filter == "under40") {
                mcoPlot <- under.40.occupation.frequencies
            }
        } else if (plot.filter == "$1 or more") {
            if (agegrp.filter == "40+") {
                mcoPlot <- over.40.occupation.frequency.one_dollar
            } else if (agegrp.filter == "under40") {
                mcoPlot <- under.40.occupation.frequency.one_dollar
            }
        } else if (plot.filter == "top 1%") {
            if (agegrp.filter == "40+") {
                mcoPlot <- over.40.occupation.frequency.top_one_percent
            } else if (agegrp.filter == "under40") {
                mcoPlot <- under.40.occupation.frequency.top_one_percent
            }
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
        agegrp.filter <- input$occupations.age.groups.filter
        plot.filter <- input$common.occupations.filter
        
        if (plot.filter == "all") {
            if (agegrp.filter == "40+") {
                mcoPlot <- over.40.occupation.frequencies
            } else if (agegrp.filter == "under40") {
                mcoPlot <- under.40.occupation.frequencies
            }
        } else if (plot.filter == "$1 or more") {
            if (agegrp.filter == "40+") {
                mcoPlot <- over.40.occupation.frequency.one_dollar
            } else if (agegrp.filter == "under40") {
                mcoPlot <- under.40.occupation.frequency.one_dollar
            }
        } else if (plot.filter == "top 1%") {
            if (agegrp.filter == "40+") {
                mcoPlot <- over.40.occupation.frequency.top_one_percent
            } else if (agegrp.filter == "under40") {
                mcoPlot <- under.40.occupation.frequency.top_one_percent
            }
        }
        
        print(mcoPlot)
    })
    
    # plot average incomes of all age groups
    output$incomes.by.age <- renderPlot({
        ggplot(incomes.by.age.top.one.percent.df) +
            geom_point(aes(x = agegrp, y = income_averages), size = 3) +
            xlab("Age groups") +
            ylab("Sum of Incomes")
    })
    
    # plot the income averages for all age groups
    output$incomes.by.age.info <- renderPrint({
        # get the x coordinate, which should represent an age group
        xPos <- input$incomes.by.age.click$x
        
        # check that it is a valid number
        if (!(is.na(xPos) || is.null(xPos)) && is.numeric(xPos)) {
            # round the result down, and store it as the row number
            rowNum <- floor(xPos)
            
            # get the data of the age group
            rowData <-
                incomes.by.age.top.one.percent.df[incomes.by.age.top.one.percent.df$agegrp == rowNum, ]
            
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
                
                # get the average of incomes of thatg age group
                average.incomes <-
                    paste0("Average income: ", rowData$income_averages)
                
                # get the list of incomes related to the
                # selected age group from the original set
                age.group <- incomes[incomes$agegrp == rowNum,]
                
                # print all the previus variabkes in seperate lines
                cat(paste0(agegrp,
                           "\n",
                           average.incomes))
            }
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
