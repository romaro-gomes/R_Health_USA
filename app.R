library(pacman)
pacman::p_load(tidyverse,here,shiny,shinydashboard,glue)

#EDA --------------------------------------------------------------------------

measures = read_csv(here('data_tables/measures.csv'))
#measures

national_results = read_csv(here('data_tables/national_results.csv'))
#national_results

questions = read_csv(here('data_tables/questions.csv'))
#questions

reports = read_csv(here('data_tables/reports.csv'))
#reports

responses = read_csv(here('data_tables/responses.csv'))
#responses

state_results = read_csv(here('data_tables/state_results.csv'))
#state_results

states = read_csv(here('data_tables/states.csv'))
#states


normalize_columns = function(data){
    colnames(data)=lapply(colnames(data),function(x) stringr::str_replace_all(string=x,pattern="-", repl=" "))
    colnames(data)=lapply(colnames(data),function(x) stringr::str_replace_all(string=x,pattern=" ", repl="_")) |>
        tolower() |>
        iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
    
    new_data = data
    
    return(new_data)
}

states=normalize_columns(states)
state_results= normalize_columns(state_results)
responses=normalize_columns(responses)
reports=normalize_columns(reports)
questions=normalize_columns(questions)
national_results=normalize_columns(national_results)
measures = normalize_columns(measures)
state_results$measure_id[1] 

?selectizeInput

data= left_join(state_results,states)
data
#App--------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title ="HCAHPS survey"),
    
    
    dashboardSidebar(disable=TRUE),
    
    dashboardBody(
        fixedRow(
            
            box(align="center",
                plotOutput('viz'), width = 12
            )
        ),
        
        
        fluidRow(
                column(width= 4,
                    selectizeInput(
                        "measure",
                        label = "Measure",
                        choices =state_results$measure_id,
                        multiple = FALSE,
                        selected =state_results$measure_id[1] 
                    )),
                
                    column(width = 4,
                    selectizeInput(
                        "period",
                        label = "Period of Studied",
                        choices =state_results$release_period,
                        multiple = FALSE,
                        selected =state_results$release_period[1] 
                    )),
                    column(width=4,
                    selectizeInput(
                        "region",
                        label = "Region Analysis",
                        choices =states$region,
                        multiple = FALSE,
                        selected = states$region[1]
                    ))
      
))
)


server <- function(input, output) {
       
   d = reactive({ data |>
        filter(release_period==input$period,measure_id =='H_CLEAN_HSP',region==input$region) |> 
        rename('sometimes_or_never'=bottom_box_percentage,'usually'=middle_box_percentage,"always"=top_box_percentage) |> 
        pivot_longer(cols=c(sometimes_or_never,usually,always),names_to ='rank',values_to = 'percentage') })
    
    output$viz = switch(input$measure,
            'H_CLEAN_HSP' =         
            renderPlot({
            ggplot(d(),aes(x=state,y=percentage, fill=rank)) +
            geom_bar(position="stack", stat="identity") +
            labs(title=glue('Measure {input$measure} in period of {input$period} from state in {input$region} region'))

    })
}


shinyApp(ui = ui, server = server)
