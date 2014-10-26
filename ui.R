library(shiny)

# Define UI for Edit distance application which is used for:
#a) finding the longest substring match for an input string(target) inside a given (source) string. 
#Eg: Given a source string "Networks" and a target string "neowork", it matches "ne" and "work" 
# and outputs the one which is longest i.e "work".
## b) Shows which operations (insert,delete, substitute,no change), are required to convert
# a source string to target string.
shinyUI(pageWithSidebar(
    # Application title
    headerPanel("Longest Substring Match via Edit Distance(Levenstein)"),
    # Sidebar with text controls to select a source and target string
    
    sidebarPanel(
        h3("Enter your input here:"),
        textInput("src", "String to search within (source)", "networks"),        
        textInput("target", "String to search for (target)", "neowork"),
        
        h6("a) Finds the longest substring match for an input string(target) inside a given (source) string. Eg: Given a source string 'Networks' and a target string 'neowork', it matches 'ne' and 'work' and outputs the one which is longest i.e 'work'."),
        h6("b) Shows which operations (insert,delete, substitute,no change), are required to convert a source string to target string."),
        h6("Inspired by: http://www.let.rug.nl/kleiweg/lev/")
        ) , 
    # Shows the 3 outputs
    mainPanel(
        # Output of the cost table which is calculated using the Edit distance algo
        h3 ("Cost Table"),
        tableOutput("costTable"),
        # Output of the longest substring match
        h3("a) Longest Matching Substring"),
        verbatimTextOutput("longest"),
        # Output of the sequence required to convert the source string to target string
        h3("b) Source to target conversion sequence"),
        verbatimTextOutput("sequence")
    )
))