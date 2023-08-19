# Libraries
library(pacman)
pacman::p_load(tidyverse,here)

measures = read_csv(here('data_tables/measures.csv'))
measures

national_results = read_csv(here('data_tables/national_results.csv'))
national_results

questions = read_csv(here('data_tables/questions.csv'))
questions

reports = read_csv(here('data_tables/reports.csv'))
reports

responses = read_csv(here('data_tables/responses.csv'))
responses

state_results = read_csv(here('data_tables/state_results.csv'))
state_results

states = read_csv(here('data_tables/states.csv'))
states

#-----------------------------------------

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



#-----------------------------------------
measures |>  View()
states |> View()


left_join(state_results,states) |>
  filter(release_period=='07_2023',measure_id =='H_CLEAN_HSP',region=='East South Central') |> 
  rename('sometimes_or_never'=bottom_box_percentage,'usually'=middle_box_percentage,"always"=top_box_percentage) |> 
  pivot_longer(cols=c(sometimes_or_never,usually,always),names_to ='rank',values_to = 'percentage') |> 
  ggplot(aes(x=state,y=percentage, fill=rank)) +
  geom_bar(position="stack", stat="identity") 

left_join(state_results,states) |>
  filter(release_period=='07_2023',measure_id =='H_CLEAN_HSP',region=='East South Central') |> 
  rename('no'=bottom_box_percentage,'regular'=middle_box_percentage,"yes"=top_box_percentage) |> 
  pivot_longer(cols=c(no,regular,yes),names_to ='rank',values_to = 'percentage') |> 
  ggplot(aes(x=state,y=percentage, fill=rank)) +
  geom_bar(position="stack", stat="identity") 

left_join(state_results,states) |>
  filter(release_period=='07_2023',measure_id =='H_CLEAN_HSP',region=='East South Central') |> 
  rename('zero_to_six'=bottom_box_percentage,"seven_or_eight"=middle_box_percentage,"nine_or_ten"=top_box_percentage) |> 
  pivot_longer(cols=c(zero_to_six,seven_or_eight,nine_or_ten),names_to ='rank',values_to = 'percentage') |> 
  ggplot(aes(x=state,y=percentage, fill=rank)) +
  geom_bar(position="stack", stat="identity")

left_join(state_results,states) |>
  filter(release_period=='07_2023',measure_id =='H_CLEAN_HSP',region=='East South Central') |> 
  rename('probably_no_or_definitely_no'=bottom_box_percentage,"probably_yes"=middle_box_percentage,"definitely_yes"=top_box_percentage) |> 
  pivot_longer(cols=c(probably_no_or_definitely_no,probably_yes,definitely_yes),names_to ='rank',values_to = 'percentage') |> 
  ggplot(aes(x=state,y=percentage, fill=rank)) +
  geom_bar(position="stack", stat="identity") 

left_join(state_results,states) |>
  filter(release_period=='07_2023',measure_id =='H_CLEAN_HSP',region=='East South Central') |> 
  rename('disagree_or_strongly_disagree'=bottom_box_percentage,"agree" =middle_box_percentage,"strongly_agree"=top_box_percentage) |> 
  pivot_longer(cols=c(disagree_or_strongly_disagree,agree,strongly_agree),names_to ='rank',values_to = 'percentage') |> 
  ggplot(aes(x=state,y=percentage, fill=rank)) +
  geom_bar(position="stack", stat="identity")

state_results$measure_id |> unique()

state_results |> filter(measure_id=='H_RECMND')
state_results |> filter(measure_id=='H_HSP_RATING') 


#--------------------------------------------------------------
responses$`response_rate_(%)` = as.numeric(responses$`response_rate_(%)`) 
responses |>
  filter(release_period=='07_2015', state=='AL') |>
  group_by(completed_surveys) |> 
  summarise(number_of_hospital=n(),
            hospital_response_data=sum(!is.na(`response_rate_(%)`)),
            mean_of_pacientes_responses_percentage=mean(`response_rate_(%)`)) |>
  mutate_if(is.numeric, ~replace_na(., 0))


#----------------------------------------------------------------------------------  
questions$bottom_box_answer |> unique()
questions$middle_box_answer |> unique()
questions$top_box_answer |> unique()
questions$measure_id
state_results

#---------------------------------------------------
