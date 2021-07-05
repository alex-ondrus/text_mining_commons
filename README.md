# text_mining_commons
Applying text mining methods to the Hansard for commons debates

## code

* `web_data_parsing.R` - loads libraries required and defines functions for accessing and parsing Hansard debate XML files from ourcommons.ca
* `mining_functions.R` - sources `web_data_parsing.R` and defines functions for applying techniques from [Tidy Text Mining](https://www.tidytextmining.com/)
* `twitter_interaction.R` - sources `mining_functions.R` as well as a `rtweet_auth.R`, a script that I did not include that creates an access token via `rtweet::create_token()`. Defines functions to generate tweets from the text mining functions in `mining_functions.R`

## data

* `CurrentMembers.csv` - contains a list of current members of the house from ourcommons.ca. Will need to be updated manually each election
