# Personality & Teamwork Quality Explorer

This project explores relationships between HEXACO personality traits
and teamwork quality using questionnaire data.

The Shiny app lets users visualize observed relationships and run
hypothetical simulations comparing three team-formation strategies:
random assignment, balancing personality scores across teams, and
grouping people with similar scores.

Simulated outcomes depend on user-selected assumptions. They do not
demonstrate that a grouping strategy improves teamwork in practice.

This project was developed for the Programming in Psychological Science
course at the University of Amsterdam.

## Dataset

https://osf.io/jb94w/overview                     <- explanation of variables 

https://itemresponsewarehouse.org/tables/zaehl2023_hexaco/    

the complete dataset is there: df <- read.csv2("https://osf.io/download/k4syv/")
 

HEXACO personality and teamwork dataset 

Source: Philipp Zähl (2023), supplementary research data 

It contains personality questionnaire responses from students in software teams, along with separate measures of teamwork quality, including cohesion, communication, coordination, commitment, support, and balance.  

Item Response Warehouse 


Useful links: 

- Personality data and codebook 

- Teamwork quality data 

- All related dataset tables 

Why it fits 

You can investigate whether personality traits are associated with teamwork quality, and which dimensions of teamwork show the strongest relationships.  

Important limitation 

The personality table has 54 respondents, with 60 questionnaire items per respondent. The accessible data description doesn't establish that you have enough team-level observations and performance outcomes to train a reliable team-performance predictor.
