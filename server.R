library(shiny)
library(pander)
library(markdown)
library(stringr)

# The following code implements the Edit distance algorithm for:
## a) finding the longest substring match for an input string(target) inside a given (source) string. 
#Eg: Given a source string "Networks" and a target string "neowork", it matches "ne" and "work" 
# and outputs the one which is longest i.e "work".
## b) Shows which operations (insert,delete, substitute,no change), are required to convert
# a source string to target string.

# Utility function for uniquifying same column or row names. Eg: If "neowork" is the row, the dataframe 
# cannot have 2 twos with name "o"...so we unquifying by additing a unique incrementing integer like "o3", "o5"
rename <- function(a) {
    b = c()
    for (i in 1:length(a)){
        b[i] <- paste0(a[i],i)
    }
    b
}
# Utility function to reverse the affect of rename (above) before returning the output i.e chop of the uniquifying
# integers
getName <- function(a) {
    unlist(lapply(strsplit(a,""),function(x) {x[[1]]}))
}

# Function which calculates the Edit distance with cost of 1 for insertion or deletion and cost of 2 for substitution
# Core logic
editDistance <- function(s,t) {    
    
    # s is the source string and t is target string
    df <- data.frame(matrix(NA, nrow = nchar(s)+1, ncol = nchar(t)+1))
    v <- c("-",strsplit(t,"")[[1]])
    names(df) <- rename(v) # Uniquify the column names
    v <- c("-",strsplit(s,"")[[1]])
    row.names(df) <- rename(v) # Uniquify the row names
    df["-1",] <- c(0:nchar(t)) # Column name for blank string
    df[,"-1"] <- c(0:nchar(s)) # Row name for blank string
    
    # Utility function
    dropl <- function(a){
        substr(a, 1, nchar(a)-1)
    }
    
    #Start filling in the cost in each of the cells as per Levenstein min cost
    for(i in 2:(nchar(s)+1)) {
        for(j in 2:(nchar(t)+1)){
            if (dropl(rownames(df)[i]) == dropl(names(df)[j])) {
                cost = 0 # Cost of 0 for a match
            }else{
                cost = 2 # Cost of 2 for substitution
            }
            # Fill in the cell by taking the min cost from top,left and diagonal(upper left) cell, as customary in
            # Edit distance (Levenstein)
            df[i,j] <- as.integer(min(c(df[i-1,j-1]+cost,df[i-1,j]+1,df[i,j-1]+1))) 
        }
    }
    
    
    # Code to backtrack starts here. Backtracking helps to find the exact cost and trace with min cost required to
    # convert source string to target string. Also helps to track all substring matches, from which we choose the 
    # longest match
    i = nchar(s)+1
    j = nchar(t)+1
    ops=""
    coord <- list()
    match <- c()
    ml <- c()
    cons = TRUE
    while (i > 1 || j > 1) {
        a=b=c=Inf
        if (i > 1 && j > 1){
            a = df[i-1,j-1]
        }
        if (i > 1){
            b = df[i-1,j]
        }
        if (j > 1){
            c = df[i,j-1]
        }
        
        parent <- min(c(a,b,c))
        ii = FALSE
        jj = FALSE

        coord[[length(coord)+1]] <- list(c(i,j))
        if (j > 1 && parent == df[i,j-1]) {
            ops = paste0(ops,'I')
            jj = TRUE
            cons = FALSE
        }else if (i > 1 && parent == df[i-1,j]) {
            ops = paste0(ops,'D')
            ii = TRUE
            cons = FALSE
        }else if (dropl(rownames(df)[i]) == dropl(names(df)[j])) {
            ops = paste0(ops,'M')
            if (cons) {
                match[length(match)+1] <- dropl(rownames(df)[i])
            }else {
                if(length(match) > 0){
                    ml[length(ml)+1] <- paste(rev(match),collapse=''); 
                }
                match <- c()
                match[length(match)+1] <- dropl(rownames(df)[i])
            }
            ii=TRUE
            jj=TRUE
            cons = TRUE
        }else {
            ops = paste0(ops,'S')
            ii= TRUE
            jj= TRUE    
            cons = FALSE
        }
        if (i==1 && j==1)
            break;
        if (ii && i > 1){
            i=i-1
        }
        if (jj && j > 1){
            j=j-1
        }
    }
    if(length(match) > 0){
        ml[length(ml)+1] <- paste(rev(match),collapse=''); 
    }
    
    #print(coord)
    subseq <- paste(rev(substring(ops,1:nchar(ops),1:nchar(ops))),collapse="")
    names(df) <- getName(names(df))
    row.names(df) <- getName(row.names(df))

    # Return a list which has:
    # 1. The resultant data frame with the frid filled
    # 2. The longest substring match found
    # 3. The exact steps to convert source string to target string
    ll <- unlist(lapply(ml,function(x){nchar(x)}))
    
    list(df,ml[which(ll==max(ll))], subseq)
}

# Define server logic required for the shiny app
shinyServer(function(input, output) {
    # We don't want to rerun the algo for each of the outputs
    dataset <- reactive({
        editDistance(input$src,input$target)
    })
    
    # Output the table
    output$costTable <- renderTable({
        dataset()[[1]]
       # HTML(markdownToHTML(
    #    text=pandoc.table.return(
     #           dataset()[[1]], 
     #          style="rmarkdown", split.tables=Inf
     #       ), 
     #       fragment.only=TRUE
      #  ) 
        
    #)
    }) 
    
    # Output the longest substring match found using this algo
    output$longest <- renderPrint({
        dataset()[[2]]
    })
    
    # Output the exact min cost sequence required to change source string to target string
    output$sequence <- renderPrint({
        dataset()[[3]]
    })
})