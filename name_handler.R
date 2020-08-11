exceptions <- list(
  'don'='doncic',
  'marc morris'='morris',
  'g hill'='hill'
)

handle_names <- function(dt){
  if ('player_code' %in% colnames(dt))
    dt <- dt %>%
      mutate(player_code=sub('^tmp_', '', player_code),
             player_code=sub('([ _])?(jr|sr|ii|iii|iv)(.)?$', '', player_code))
  if ('description' %in% colnames(dt))
    dt <- dt %>% 
      mutate(description=gsub(' (ii|iii|iv|jr.|sr.) ', ' ', description),
             description=gsub(' (ii|iii|iv|jr.|sr.)$', '', description),
             description=sub('marc morris', 'morris', description),
             description=sub(' g hill', ' hill', description),
             description=sub(' don ', ' doncic ', description))
  
  dt
}

.special_names <- function(x){
  
}