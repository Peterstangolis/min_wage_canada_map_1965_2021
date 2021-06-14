
# ============    GETTING THE DATA FOR THE PROJECT     ============= #



## LOAD LIBRARIES
library(datapasta)
library(tidyverse)
library(lubridate)
library(timetk)
library(stringr)


##  LINK TO DATA USED IN PROJECT - DATA TO BE SCRAPED WITH {datapasta}
url <- "http://srv116.services.gc.ca/dimt-wid/sm-mw/rpt4.aspx?GoCTemplateCulture=en-CA"

## MANITOBA WAGES 1965-2021
manitoba_wages <- tibble::tribble(
  ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                 ~Note,
     "Manitoba",   "01-Oct-2021",      "$11.95",                                                    NA,
     "Manitoba",   "01-Oct-2020",      "$11.90",                                                    NA,
     "Manitoba",   "01-Oct-2019",      "$11.65",                                                    NA,
     "Manitoba",   "01-Oct-2018",      "$11.35",                                                    NA,
     "Manitoba",   "01-Oct-2017",      "$11.15",                                                    NA,
     "Manitoba",   "01-Oct-2015",      "$11.00",                                                    NA,
     "Manitoba",   "01-Oct-2014",      "$10.70",                                                    NA,
     "Manitoba",   "01-Oct-2013",      "$10.45",                                                    NA,
     "Manitoba",   "01-Oct-2012",      "$10.25",                                                    NA,
     "Manitoba",   "01-Oct-2011",      "$10.00",                                                    NA,
     "Manitoba",   "01-Oct-2010",       "$9.50",                                                    NA,
     "Manitoba",   "01-Oct-2009",       "$9.00",                                                    NA,
     "Manitoba",   "01-May-2009",       "$8.75",                                                    NA,
     "Manitoba",   "01-Apr-2008",       "$8.50",                                                    NA,
     "Manitoba",   "01-Apr-2007",       "$8.00",                                                    NA,
     "Manitoba",   "01-Apr-2006",       "$7.60",                                                    NA,
     "Manitoba",   "01-Apr-2005",       "$7.25",                                                    NA,
     "Manitoba",   "01-Apr-2004",       "$7.00",                                                    NA,
     "Manitoba",   "01-Apr-2003",       "$6.75",                                                    NA,
     "Manitoba",   "01-Apr-2002",       "$6.50",                                                    NA,
     "Manitoba",   "01-Apr-2001",       "$6.25",                                                    NA,
     "Manitoba",   "01-Apr-1999",       "$6.00",                                                    NA,
     "Manitoba",   "01-Jan-1996",       "$5.40",                                                    NA,
     "Manitoba",   "01-Jul-1995",       "$5.25",                                                    NA,
     "Manitoba",   "01-Mar-1991",       "$5.00",                                                    NA,
     "Manitoba",   "01-Sep-1987",       "$4.70",                                                    NA,
     "Manitoba",   "01-Apr-1987",       "$4.50",                                                    NA,
     "Manitoba",   "01-Jan-1985",       "$4.30",                                                    NA,
     "Manitoba",   "01-Jul-1982",       "$4.00",                                                    NA,
     "Manitoba",   "01-Sep-1981",       "$3.55",                                                    NA,
     "Manitoba",   "01-Mar-1981",       "$3.35",                                                    NA,
     "Manitoba",   "01-Jan-1980",       "$3.15",                                                    NA,
     "Manitoba",   "01-Jul-1979",       "$3.05",                                                    NA,
     "Manitoba",   "01-Sep-1976",       "$2.95",                                                    NA,
     "Manitoba",   "01-Oct-1975",       "$2.60",                                                    NA,
     "Manitoba",   "01-Jan-1975",       "$2.30",                                                    NA,
     "Manitoba",   "01-Jul-1974",       "$2.15",                                                    NA,
     "Manitoba",   "01-Oct-1973",       "$1.90",                                                    NA,
     "Manitoba",   "01-Oct-1972",       "$1.75",                                                    NA,
     "Manitoba",   "01-Nov-1971",       "$1.65",                                                    NA,
     "Manitoba",   "01-Oct-1970",       "$1.50",                                                    NA,
     "Manitoba",   "01-Dec-1969",       "$1.35",                                                    NA,
     "Manitoba",   "01-Dec-1968",       "$1.25",                                                    NA,
     "Manitoba",   "01-Aug-1968",       "$1.20",                                                    NA,
     "Manitoba",   "01-Apr-1968",       "$1.15",                                                    NA,
     "Manitoba",   "01-Dec-1967",       "$1.10",                                                    NA,
     "Manitoba",   "01-Dec-1966",       "$1.00",                                                    NA,
     "Manitoba",   "01-Jul-1966",       "$0.92",                                         "Urban areas",
     "Manitoba",   "01-Jul-1966",       "$0.90",                                         "Rural areas",
     "Manitoba",   "01-Dec-1965",       "$0.85",                                         "Urban areas",
     "Manitoba",   "01-Dec-1965",       "$0.80",                                         "Rural areas",
     "Manitoba",   "01-Jan-1965",       "$0.75",     "Urban areas. (Existing rate as of Jan. 1, 1965)",
     "Manitoba",   "01-Jan-1965",       "$0.70", "Non-urban areas (Existing rate as of Jan. 1, 1965.)"
  ) %>% 
  select(-Note)

  

## ONTARIO WAGES 1965-2021  
ontario_wages <- tibble::tribble(
                   ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                                          ~Note,
                       "Ontario",   "01-Oct-2021",      "$14.35",                                                                                             NA,
                       "Ontario",   "01-Oct-2020",      "$14.25",                                                                                             NA,
                       "Ontario",   "01-Jan-2018",      "$14.00",                                                                                             NA,
                       "Ontario",   "01-Oct-2017",      "$11.60", "On October 1 of each year, this rate increases based on changes to the Consumer Price Index.",
                       "Ontario",   "01-Oct-2016",      "$11.40", "On October 1 of each year, this rate increases based on changes to the Consumer Price Index.",
                       "Ontario",   "01-Oct-2015",      "$11.25",                                                                                             NA,
                       "Ontario",   "01-Jun-2014",      "$11.00",                                                                                             NA,
                       "Ontario",   "31-Mar-2010",      "$10.25",                                                                                             NA,
                       "Ontario",   "31-Mar-2009",       "$9.50",                                                                                             NA,
                       "Ontario",   "31-Mar-2008",       "$8.75",                                                                                             NA,
                       "Ontario",   "01-Feb-2007",       "$8.00",                                                                                             NA,
                       "Ontario",   "01-Feb-2006",       "$7.75",                                                                                             NA,
                       "Ontario",   "01-Feb-2005",       "$7.45",                                                                                             NA,
                       "Ontario",   "01-Feb-2004",       "$7.15",                                                                                             NA,
                       "Ontario",   "01-Jan-1995",       "$6.85",                                                                                             NA,
                       "Ontario",   "01-Jan-1994",       "$6.70",                                                                                             NA,
                       "Ontario",   "01-Nov-1992",       "$6.35",                                                                                             NA,
                       "Ontario",   "01-Nov-1991",       "$6.00",                                                                                             NA,
                       "Ontario",   "01-Oct-1990",       "$5.40",                                                                                             NA,
                       "Ontario",   "01-Oct-1989",       "$5.00",                                                                                             NA,
                       "Ontario",   "01-Oct-1988",       "$4.75",                                                                                             NA,
                       "Ontario",   "01-Oct-1987",       "$4.55",                                                                                             NA,
                       "Ontario",   "01-Oct-1986",       "$4.35",                                                                                             NA,
                       "Ontario",   "01-Oct-1984",       "$4.00",                                                                                             NA,
                       "Ontario",   "01-Mar-1984",       "$3.85",                                                                                             NA,
                       "Ontario",   "01-Oct-1981",       "$3.50",                                                                                             NA,
                       "Ontario",   "31-Mar-1981",       "$3.30",                                                                                             NA,
                       "Ontario",   "01-Jan-1979",       "$3.00",                                                                                             NA,
                       "Ontario",   "01-Aug-1978",       "$2.85",                                                                                             NA,
                       "Ontario",   "15-Mar-1976",       "$2.65",                                                                                             NA,
                       "Ontario",   "01-May-1975",       "$2.40",                                                                                             NA,
                       "Ontario",   "01-Oct-1974",       "$2.25",                                                                                             NA,
                       "Ontario",   "01-Jan-1974",       "$2.00",                                                                                             NA,
                       "Ontario",   "01-Feb-1973",       "$1.80",                                                                                             NA,
                       "Ontario",   "01-Apr-1971",       "$1.65",                                                                                             NA,
                       "Ontario",   "01-Oct-1970",       "$1.50",                                                                                             NA,
                       "Ontario",   "01-Jan-1969",       "$1.30",                                                                                             NA,
                       "Ontario",   "27-Dec-1965",       "$1.00",                                                                       "Northern Ontario Zone.",
                       "Ontario",   "30-Mar-1965",       "$1.00",                                                                 "Women. Southern Ontario Zone",
                       "Ontario",   "01-Jan-1965",       "$1.00",                                                                  "Men. Southern Ontario Zone.",
                       "Ontario",   "01-Jan-1965",       "$1.00",                            "Oshawa-Toronto-Hamilton area. Existing rate as of January 1, 1965",
                       "Ontario",   "01-Jan-1965",       "$0.95",                                                                "Women. Southern Ontario Zone.",
                       "Ontario",   "01-Jan-1965",       "$0.90",                                  "Northern Ontario Zone. Existing rate as of January 1, 1965."
                   ) %>% 
  select(-Note)

## ALBERTA WAGES 1965-2021
alberta_wages <- tibble::tribble(
                   ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                         ~Note,
                       "Alberta",   "01-Oct-2018",      "$15.00",                                                                            NA,
                       "Alberta",   "01-Oct-2017",      "$13.60",                                                                            NA,
                       "Alberta",   "01-Oct-2016",      "$12.20",                                                                            NA,
                       "Alberta",   "01-Oct-2015",      "$11.20",                                                                            NA,
                       "Alberta",   "01-Sep-2014",      "$10.20",                                                                            NA,
                       "Alberta",   "01-Sep-2013",       "$9.95",                                                                            NA,
                       "Alberta",   "01-Sep-2012",       "$9.75",                                                                            NA,
                       "Alberta",   "01-Sep-2011",       "$9.40",                                                                            NA,
                       "Alberta",   "01-Apr-2009",       "$8.80",                                                                            NA,
                       "Alberta",   "01-Apr-2008",       "$8.40",                                                                            NA,
                       "Alberta",   "01-Sep-2007",       "$8.00",                                                                            NA,
                       "Alberta",   "01-Sep-2005",       "$7.00",                                                                            NA,
                       "Alberta",   "01-Oct-1999",       "$5.90",                                                                            NA,
                       "Alberta",   "01-Apr-1999",       "$5.65",                                                                            NA,
                       "Alberta",   "01-Oct-1998",       "$5.40",                                                                            NA,
                       "Alberta",   "01-Apr-1992",       "$5.00",                                                                            NA,
                       "Alberta",   "01-Sep-1988",       "$4.50",                                                                            NA,
                       "Alberta",   "01-May-1981",       "$3.80",                                                                            NA,
                       "Alberta",   "01-May-1980",       "$3.50",                                                                            NA,
                       "Alberta",   "01-Mar-1977",       "$3.00",                                                                            NA,
                       "Alberta",   "01-Mar-1976",       "$2.75",                                                                            NA,
                       "Alberta",   "01-Jul-1975",       "$2.50",                                                                            NA,
                       "Alberta",   "01-Jan-1975",       "$2.25",                                                                            NA,
                       "Alberta",   "01-Apr-1974",       "$2.00",                                                                            NA,
                       "Alberta",   "01-Oct-1973",       "$1.90",                                                                            NA,
                       "Alberta",   "01-Jan-1973",       "$1.75",                                                                            NA,
                       "Alberta",   "01-Oct-1970",       "$1.55",                                                                            NA,
                       "Alberta",   "01-Apr-1970",       "$1.40",                                                                            NA,
                       "Alberta",   "01-Jan-1968",       "$1.25",                                                                            NA,
                       "Alberta",   "01-Aug-1967",       "$1.15",                                                                            NA,
                       "Alberta",   "01-Jul-1966",       "$1.00",                                  "Applicable in all regions of the province.",
                       "Alberta",   "01-Jan-1965",       "$1.00", "Urban centres over 5,000 population. (Existing rate as of January 1, 1965.)",
                       "Alberta",   "01-Jan-1965",       "$0.95",                      "Non-Urban areas. (Existing rate as of January 1, 1965)"
                   ) %>% 
  select(-Note)

## QUEBEC WAGES 1965-2021
quebec_waves <- tibble::tribble(
                  ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                                                               ~Note,
                       "Quebec",   "01-May-2021",      "$13.50",                                                                                                                  NA,
                       "Quebec",   "01-May-2020",      "$13.10",                                                                                                                  NA,
                       "Quebec",   "01-May-2019",      "$12.50",                                                                                                                  NA,
                       "Quebec",   "01-May-2018",      "$12.00",                                                                                                                  NA,
                       "Quebec",   "01-May-2017",      "$11.25",                                                                                                                  NA,
                       "Quebec",   "01-May-2016",      "$10.75",                                                                                                                  NA,
                       "Quebec",   "01-May-2015",      "$10.55",                                                                                                                  NA,
                       "Quebec",   "01-May-2014",      "$10.35",                                                                                                                  NA,
                       "Quebec",   "01-May-2013",      "$10.15",                                                                                                                  NA,
                       "Quebec",   "01-May-2012",       "$9.90",                                                                                                                  NA,
                       "Quebec",   "01-May-2011",       "$9.65",                                                                                                                  NA,
                       "Quebec",   "01-May-2010",       "$9.50",                                                                                                                  NA,
                       "Quebec",   "01-May-2008",       "$8.50",                                                                                                                  NA,
                       "Quebec",   "01-May-2007",       "$8.00",                                                                                                                  NA,
                       "Quebec",   "01-May-2006",       "$7.75",                                                                                                                  NA,
                       "Quebec",   "01-May-2005",       "$7.60",                                                                                                                  NA,
                       "Quebec",   "01-May-2004",       "$7.45",                                                                                                                  NA,
                       "Quebec",   "01-Feb-2003",       "$7.30",                                                                                                                  NA,
                       "Quebec",   "01-Oct-2002",       "$7.20",                                                                                                                  NA,
                       "Quebec",   "01-Feb-2001",       "$7.00",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1998",       "$6.90",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1997",       "$6.80",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1996",       "$6.70",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1995",       "$6.45",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1994",       "$6.00",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1993",       "$5.85",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1992",       "$5.70",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1991",       "$5.55",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1990",       "$5.30",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1989",       "$5.00",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1988",       "$4.75",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1987",       "$4.55",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1986",       "$4.35",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1981",       "$4.00",                                                                                                                  NA,
                       "Quebec",   "01-Apr-1981",       "$3.85",                                                                                                                  NA,
                       "Quebec",   "01-Apr-1980",       "$3.65",                                                                                                                  NA,
                       "Quebec",   "01-Apr-1979",       "$3.47",                                                                                                                  NA,
                       "Quebec",   "01-Oct-1978",       "$3.37",                                                                                                                  NA,
                       "Quebec",   "01-Jan-1978",       "$3.27",                                                                                                                  NA,
                       "Quebec",   "01-Jul-1977",       "$3.15",                                                                                                                  NA,
                       "Quebec",   "01-Jan-1977",       "$3.00",                                                                                                                  NA,
                       "Quebec",   "01-Jul-1976",       "$2.87",                                                                                                                  NA,
                       "Quebec",   "01-Dec-1975",       "$2.80",                                                                                                                  NA,
                       "Quebec",   "01-Jun-1975",       "$2.60",                                                                                                                  NA,
                       "Quebec",   "01-Nov-1974",       "$2.30",                                                                                                                  NA,
                       "Quebec",   "01-May-1974",       "$2.10",                                                                                                                  NA,
                       "Quebec",   "01-Nov-1973",       "$1.85",                                                                                                                  NA,
                       "Quebec",   "01-May-1973",       "$1.70",                                                                                                                  NA,
                       "Quebec",   "01-Nov-1972",       "$1.65",                                                                                                                  NA,
                       "Quebec",   "01-Aug-1972",       "$1.60",                                                                                                                  NA,
                       "Quebec",   "01-Nov-1971",       "$1.50",                                                                                                                  NA,
                       "Quebec",   "01-May-1971",       "$1.45",                                                                                                                  NA,
                       "Quebec",   "01-Nov-1970",       "$1.40", "Montreal metropolitan area. The rate outside the Montreal metropolitan area was set at $1.35 on November 1, 1970.",
                       "Quebec",   "01-May-1970",       "$1.35",      "Montreal metropolitan area. The rate outside the Montreal metropolitan area was set at $1.30 on May 1, 1970.",
                       "Quebec",   "01-Nov-1968",       "$1.25", "Montreal metropolitan area. The rate outside the Montreal metropolitan area was set at $1.15 on November 1, 1968.",
                       "Quebec",   "01-Apr-1967",       "$1.05",    "Montreal metropolitan area. The rate outside the Montreal metropolitan area was set at $1.00 on April 1, 1967.",
                       "Quebec",   "01-Nov-1966",       "$1.00", "Montreal metropolitan area. The rate outside the Montreal metropolitan area was set at $0.90 on November 1, 1966.",
                       "Quebec",   "01-Oct-1965",       "$0.85",  "Montreal metropolitan area. The rate outside the Montreal metropolitan area was set at $0.80 on October 1, 1965.",
                       "Quebec",   "01-Jan-1965",       "$0.70",         "Montreal metropolitan area. The rate outside the Montreal metropolitan area was $0.64 on January 1, 1965."
                  ) %>% 
  select(-Note)

## BRITISH COLUMBIA WAGES 1965-2021
bc_wages <-tibble::tribble(
                  ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                  ~Note,
             "British Columbia",   "01-Jun-2021",      "$15.20",                                     NA,
             "British Columbia",   "01-Jun-2020",      "$14.60",                                     NA,
             "British Columbia",   "01-Jun-2019",      "$13.85",                                     NA,
             "British Columbia",   "01-Jun-2018",      "$12.65",                                     NA,
             "British Columbia",   "15-Sep-2017",      "$11.35",                                     NA,
             "British Columbia",   "15-Sep-2016",      "$10.85",                                     NA,
             "British Columbia",   "15-Sep-2015",      "$10.45",                                     NA,
             "British Columbia",   "01-May-2012",      "$10.25",                                     NA,
             "British Columbia",   "01-Nov-2011",       "$9.50",                                     NA,
             "British Columbia",   "01-May-2011",       "$8.75",                                     NA,
             "British Columbia",   "01-Nov-2001",       "$8.00",                                     NA,
             "British Columbia",   "01-Nov-2000",       "$7.60",                                     NA,
             "British Columbia",   "01-Apr-1998",       "$7.15",                                     NA,
             "British Columbia",   "01-Oct-1995",       "$7.00",                                     NA,
             "British Columbia",   "01-Mar-1995",       "$6.50",                                     NA,
             "British Columbia",   "01-Apr-1993",       "$6.00",                                     NA,
             "British Columbia",   "01-Feb-1992",       "$5.50",                                     NA,
             "British Columbia",   "01-Apr-1990",       "$5.00",                                     NA,
             "British Columbia",   "01-Oct-1989",       "$4.75",                                     NA,
             "British Columbia",   "01-Jul-1988",       "$4.50",                                     NA,
             "British Columbia",   "01-Dec-1980",       "$3.65",                                     NA,
             "British Columbia",   "01-Jul-1980",       "$3.40",                                     NA,
             "British Columbia",   "01-Jan-1976",       "$3.00",                                     NA,
             "British Columbia",   "01-Dec-1975",       "$2.75",                                     NA,
             "British Columbia",   "03-Jun-1974",       "$2.50",                                     NA,
             "British Columbia",   "03-Dec-1973",       "$2.25",                                     NA,
             "British Columbia",   "04-Dec-1972",       "$2.00",                                     NA,
             "British Columbia",   "04-May-1970",       "$1.50",                                     NA,
             "British Columbia",   "01-Nov-1967",       "$1.25",                                     NA,
             "British Columbia",   "01-May-1967",       "$1.10",                                     NA,
             "British Columbia",   "01-Jan-1965",       "$1.00", "Existing rate as of January 1, 1965."
             ) %>% 
  select(-Note)

## NEW BRUNSWICK WAGES 1965-2021
nb_wages <- tibble::tribble(
                ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                 ~Note,
              "New Brunswick",   "01-Apr-2021",      "$11.75",                                                                    NA,
              "New Brunswick",   "01-Apr-2019",      "$11.50",                                                                    NA,
              "New Brunswick",   "01-Apr-2018",      "$11.25",                                                                    NA,
              "New Brunswick",   "01-Apr-2017",      "$11.00",                                                                    NA,
              "New Brunswick",   "01-Apr-2016",      "$10.65",                                                                    NA,
              "New Brunswick",   "31-Dec-2014",      "$10.30",                                                                    NA,
              "New Brunswick",   "01-Apr-2012",      "$10.00",                                                                    NA,
              "New Brunswick",   "01-Apr-2011",       "$9.50",                                                                    NA,
              "New Brunswick",   "01-Sep-2010",       "$9.00",                                                                    NA,
              "New Brunswick",   "01-Apr-2010",       "$8.50",                                                                    NA,
              "New Brunswick",   "01-Sep-2009",       "$8.25",                                                                    NA,
              "New Brunswick",   "15-Apr-2009",       "$8.00",                                                                    NA,
              "New Brunswick",   "31-Mar-2008",       "$7.75",                                                                    NA,
              "New Brunswick",   "01-Jul-2007",       "$7.25",                                                                    NA,
              "New Brunswick",   "05-Jan-2007",       "$7.00",                                                                    NA,
              "New Brunswick",   "01-Jul-2006",       "$6.70",                                                                    NA,
              "New Brunswick",   "01-Jan-2006",       "$6.50",                                                                    NA,
              "New Brunswick",   "01-Jan-2005",       "$6.30",                                                                    NA,
              "New Brunswick",   "01-Jan-2004",       "$6.20",                                                                    NA,
              "New Brunswick",   "01-Aug-2002",       "$6.00",                                                                    NA,
              "New Brunswick",   "01-Jul-2001",       "$5.90",                                                                    NA,
              "New Brunswick",   "01-Jan-2000",       "$5.75",                                                                    NA,
              "New Brunswick",   "01-Jul-1996",       "$5.50",                                                                    NA,
              "New Brunswick",   "01-Jan-1996",       "$5.25",                                                                    NA,
              "New Brunswick",   "01-Oct-1991",       "$5.00",                                                                    NA,
              "New Brunswick",   "01-Oct-1990",       "$4.75",                                                                    NA,
              "New Brunswick",   "01-Oct-1989",       "$4.50",                                                                    NA,
              "New Brunswick",   "01-Apr-1989",       "$4.25",                                                                    NA,
              "New Brunswick",   "15-Sep-1986",       "$4.00",                                                                    NA,
              "New Brunswick",   "01-Oct-1982",       "$3.80",                                                                    NA,
              "New Brunswick",   "01-Oct-1980",       "$3.35",                                                                    NA,
              "New Brunswick",   "01-Jul-1980",       "$3.05",                                                                    NA,
              "New Brunswick",   "01-Nov-1976",       "$2.80",                                                                    NA,
              "New Brunswick",   "01-Jun-1976",       "$2.55",                                                                    NA,
              "New Brunswick",   "01-Jul-1975",       "$2.30",                                                                    NA,
              "New Brunswick",   "01-Jan-1975",       "$2.15",                                                                    NA,
              "New Brunswick",   "01-Jul-1974",       "$1.90",                                                                    NA,
              "New Brunswick",   "01-Jan-1974",       "$1.75",                                                                    NA,
              "New Brunswick",   "01-Jan-1973",       "$1.50",                                                                    NA,
              "New Brunswick",   "01-Mar-1972",       "$1.40",                                                                    NA,
              "New Brunswick",   "01-Sep-1971",       "$1.25",                                                                    NA,
              "New Brunswick",   "01-Jan-1970",       "$1.15",                                                                    NA,
              "New Brunswick",   "01-Jan-1968",       "$1.00",                                                                    NA,
              "New Brunswick",   "01-Jul-1966",       "$0.90", "Wholesale, retail and manufacturing. Other rates for other sectors.",
              "New Brunswick",   "01-Jul-1965",       "$0.80", "Wholesale, retail and manufacturing. Other rates for other sectors.",
              "New Brunswick",   "01-Jan-1965",       "$0.75", "Wholesale, retail and manufacturing. Other rates for other sectors."
              ) %>% 
  select(-Note)

## NEWFOUNDLAND AND LABRADOR WAGES 1965-2021
nl_wages <- tibble::tribble(
                            ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                         ~Note,
              "Newfoundland and Labrador",   "01-Apr-2021",      "$12.50",                                            NA,
              "Newfoundland and Labrador",   "01-Oct-2020",      "$12.15",                                            NA,
              "Newfoundland and Labrador",   "01-Apr-2020",      "$11.65",                                            NA,
              "Newfoundland and Labrador",   "01-Apr-2018",      "$11.15",                                            NA,
              "Newfoundland and Labrador",   "01-Oct-2017",      "$11.00",                                            NA,
              "Newfoundland and Labrador",   "01-Apr-2017",      "$10.75",                                            NA,
              "Newfoundland and Labrador",   "01-Oct-2015",      "$10.50",                                            NA,
              "Newfoundland and Labrador",   "01-Oct-2014",      "$10.25",                                            NA,
              "Newfoundland and Labrador",   "01-Jul-2010",      "$10.00",                                            NA,
              "Newfoundland and Labrador",   "01-Jan-2010",       "$9.50",                                            NA,
              "Newfoundland and Labrador",   "01-Jul-2009",       "$9.00",                                            NA,
              "Newfoundland and Labrador",   "01-Jan-2009",       "$8.50",                                            NA,
              "Newfoundland and Labrador",   "01-Apr-2008",       "$8.00",                                            NA,
              "Newfoundland and Labrador",   "01-Oct-2007",       "$7.50",                                            NA,
              "Newfoundland and Labrador",   "01-Jan-2007",       "$7.00",                                            NA,
              "Newfoundland and Labrador",   "01-Jun-2006",       "$6.75",                                            NA,
              "Newfoundland and Labrador",   "01-Jan-2006",       "$6.50",                                            NA,
              "Newfoundland and Labrador",   "01-Jun-2005",       "$6.25",                                            NA,
              "Newfoundland and Labrador",   "01-Nov-2002",       "$6.00",                                            NA,
              "Newfoundland and Labrador",   "01-May-2002",       "$5.75",                                            NA,
              "Newfoundland and Labrador",   "01-Oct-1999",       "$5.50",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Apr-1997",       "$5.25",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Sep-1996",       "$5.00",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Apr-1991",       "$4.75",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Apr-1988",       "$4.25",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jan-1985",       "$4.00",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jan-1983",       "$3.75",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "31-Mar-1981",       "$3.45",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jul-1980",       "$3.15",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jun-1979",       "$2.80",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jan-1976",       "$2.50",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jan-1975",       "$2.20",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jul-1974",       "$2.00",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jan-1974",       "$1.80",          "Employees 16 years of age or older",
              "Newfoundland and Labrador",   "01-Jun-1972",       "$1.40",          "Employees 18 years of age or older",
              "Newfoundland and Labrador",   "01-Jul-1970",       "$1.25",                                         "Men",
              "Newfoundland and Labrador",   "01-Jul-1970",       "$1.00",                                       "Women",
              "Newfoundland and Labrador",   "01-May-1968",       "$1.10",                                         "Men",
              "Newfoundland and Labrador",   "01-May-1968",       "$0.85",                                       "Women",
              "Newfoundland and Labrador",   "01-Jan-1965",       "$0.70",   "Men. Existing rate as of January 1, 1965.",
              "Newfoundland and Labrador",   "01-Jan-1965",       "$0.50", "Women. Existing rate as of January 1, 1965."
              ) %>% 
  select(-Note)

## NORTHWEST TERRITORIES WAGES 1965-2021
nw_wages <- tibble::tribble(
                        ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                         ~Note,
              "Northwest Territories",   "01-Sep-2021",      "$15.20",                                                                            NA,
              "Northwest Territories",   "01-Apr-2018",      "$13.46",                                                                            NA,
              "Northwest Territories",   "01-Jun-2015",      "$12.50",                                                                            NA,
              "Northwest Territories",   "01-Apr-2011",      "$10.00",                                                                            NA,
              "Northwest Territories",   "01-Apr-2010",       "$9.00",                                                                            NA,
              "Northwest Territories",   "28-Dec-2003",       "$8.25",                      "Applies to all employees in the Northwest Territories.",
              "Northwest Territories",   "01-Apr-1991",       "$7.00", "Employees 16 years of age or older in areas distant from the highway system",
              "Northwest Territories",   "01-Apr-1986",       "$5.00",                                                                            NA,
              "Northwest Territories",   "01-Aug-1982",       "$4.25",                                                                            NA,
              "Northwest Territories",   "15-May-1980",       "$3.50",                                                                            NA,
              "Northwest Territories",   "01-Jun-1976",       "$3.00",                                                                            NA,
              "Northwest Territories",   "01-Apr-1974",       "$2.50",                                                                            NA,
              "Northwest Territories",   "01-Sep-1973",       "$2.00",                                                                            NA,
              "Northwest Territories",   "01-Sep-1970",       "$1.50",                                                                            NA,
              "Northwest Territories",   "01-Jul-1968",       "$1.25",                                                                            NA
              ) %>% 
  select(-Note)

## NOVA SCOTIA WAGES 1965-2021
ns_wages <- tibble::tribble(
              ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                                                                                                                                           ~Note,
              "Nova Scotia",   "01-Apr-2021",      "$12.95",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2020",      "$12.55",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2019",      "$11.55",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2018",      "$11.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2017",      "$10.85",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2016",      "$10.70", "On April 1 of each year, this rate is adjusted by the percentage change in the projected annual Consumer Price Index for Canada in the preceding calendar year, rounded to the nearest $0.05.",
              "Nova Scotia",   "01-Apr-2015",      "$10.60", "On April 1 of each year, this rate is adjusted by the percentage change in the projected annual Consumer Price Index for Canada in the preceding calendar year, rounded to the nearest $0.05.",
              "Nova Scotia",   "01-Apr-2014",      "$10.40", "On April 1 of each year, this rate is adjusted by the percentage change in the projected annual Consumer Price Index for Canada in the preceding calendar year, rounded to the nearest $0.05.",
              "Nova Scotia",   "01-Apr-2013",      "$10.30", "On April 1 of each year, this rate is adjusted by the percentage change in the projected annual Consumer Price Index for Canada in the preceding calendar year, rounded to the nearest $0.05.",
              "Nova Scotia",   "01-Apr-2012",      "$10.15",                                                 "On April 1 of each year, this rate is to increase to reflect changes in Statistics Canada's Low Income Cut-Off figures for the previous year.",
              "Nova Scotia",   "01-Oct-2011",      "$10.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-2010",       "$9.65",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2010",       "$9.20",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2009",       "$8.60",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-May-2008",       "$8.10",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-May-2007",       "$7.60",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2006",       "$7.15",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-2005",       "$6.80",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Apr-2004",       "$6.50",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-2003",       "$6.25",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-2002",       "$6.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-2001",       "$5.80",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-2000",       "$5.70",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1999",       "$5.60",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Feb-1997",       "$5.50",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1996",       "$5.35",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1993",       "$5.15",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1992",       "$5.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1991",       "$4.75",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1989",       "$4.50",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1985",       "$4.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1982",       "$3.75",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1981",       "$3.30",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1980",       "$3.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1977",       "$2.75",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1976",       "$2.50",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Mar-1975",       "$2.25",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jan-1975",       "$2.20",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Oct-1974",       "$2.00",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jul-1974",       "$1.80",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jul-1973",       "$1.65",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jul-1972",       "$1.55",                                                                                                                                                                                              NA,
              "Nova Scotia",   "01-Jul-1971",       "$1.35",                                                                     "Men. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst,Yarmouth, Antigonish and Port Hawksbury. $1.25 elsewhere.",
              "Nova Scotia",   "01-Jul-1971",       "$1.20",                                                                   "Women. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst,Yarmouth, Antigonish and Port Hawksbury. $1.10 elsewhere.",
              "Nova Scotia",   "01-Jan-1971",       "$1.30",                                                                     "Men. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst,Yarmouth, Antigonish and Port Hawksbury. $1.20 elsewhere.",
              "Nova Scotia",   "01-Jan-1971",       "$1.10",                                                                   "Women. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst,Yarmouth, Antigonish and Port Hawksbury. $1.00 elsewhere.",
              "Nova Scotia",   "01-Aug-1968",       "$1.25",                                                                                                "Men. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst and Yarmouth. $1.15 elsewhere.",
              "Nova Scotia",   "01-Aug-1968",       "$1.00",                                                                                              "Women. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst and Yarmouth. $0.90 elsewhere.",
              "Nova Scotia",   "01-Apr-1968",       "$1.15",                                                                                                "Men. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst and Yarmouth. $1.05 elsewhere.",
              "Nova Scotia",   "01-Apr-1968",       "$0.90",                                                                                              "Women. In Halifax, Dartmouth, New Glasgow, Sydney, Truro, Amherst and Yarmouth. $0.80 elsewhere.",
              "Nova Scotia",   "01-Jun-1966",       "$1.10",                                                                                    "Men. In Halifax, Dartmouth, New Glasgow and Sydney. $1.00 in Truro, Amherst and Yarmouth. $0.90 elsewhere.",
              "Nova Scotia",   "01-Jun-1966",       "$0.85",                                                                                  "Women. In Halifax, Dartmouth, New Glasgow and Sydney. $0.75 in Truro, Amherst and Yarmouth. $0.65 elsewhere.",
              "Nova Scotia",   "20-Feb-1965",       "$1.05",                                                                                    "Men. In Halifax, Dartmouth, New Glasgow and Sydney. $0.95 in Truro, Amherst and Yarmouth. $0.85 elsewhere.",
              "Nova Scotia",   "20-Feb-1965",       "$0.80",                                                                                  "Women. In Halifax, Dartmouth, New Glasgow and Sydney. $0.70 in Truro, Amherst and Yarmouth. $0.60 elsewhere.",
              "Nova Scotia",   "01-Jan-1965",      "$21.60",                     "Per week. For women only, in Halifax, Dartmouth, New Glasgow, and Sydney. $19.20 in Truro, Amherst and Yarmouth. $14.40 elsewhere. (Existing rates as of January 1, 1965)"
              ) %>% 
  select(-Note)

## NUNAVUT WAGES 1965-2021
nv_wages <- tibble::tribble(
                ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                         ~Note,
                    "Nunavut",   "01-Apr-2020",      "$16.00",                                        "Applies to all employees in Nunavut.",
                    "Nunavut",   "01-Apr-2016",      "$13.00",                                                                            NA,
                    "Nunavut",   "01-Jan-2011",      "$11.00",                                                                            NA,
                    "Nunavut",   "05-Sep-2008",      "$10.00",                                        "Applies to all employees in Nunavut.",
                    "Nunavut",   "03-Mar-2003",       "$8.50",                                        "Applies to all employees in Nunavut.",
                    "Nunavut",   "01-Apr-1999",       "$7.00", "Employees 16 years of age or older in areas distant from the highway system",
                    "Nunavut",   "01-Apr-1999",       "$6.50",                                         "Employees 16 years of age or older."
                ) %>% 
  select(-Note)

## PEI WAGES 1965-2021
pei_wages <- tibble::tribble(
                         ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                      ~Note,
                "Prince Edward Island",   "01-Apr-2021",      "$13.00",                                         NA,
                "Prince Edward Island",   "01-Apr-2020",      "$12.85",                                         NA,
                "Prince Edward Island",   "01-Apr-2019",      "$12.25",                                         NA,
                "Prince Edward Island",   "01-Apr-2018",      "$11.55",                                         NA,
                "Prince Edward Island",   "01-Apr-2017",      "$11.25",                                         NA,
                "Prince Edward Island",   "01-Oct-2016",      "$11.00",                                         NA,
                "Prince Edward Island",   "01-Jun-2016",      "$10.75",                                         NA,
                "Prince Edward Island",   "01-Jul-2015",      "$10.50",                                         NA,
                "Prince Edward Island",   "01-Oct-2014",      "$10.35",                                         NA,
                "Prince Edward Island",   "01-Jun-2014",      "$10.20",                                         NA,
                "Prince Edward Island",   "01-Apr-2012",      "$10.00",                                         NA,
                "Prince Edward Island",   "01-Oct-2011",       "$9.60",                                         NA,
                "Prince Edward Island",   "01-Jun-2011",       "$9.30",                                         NA,
                "Prince Edward Island",   "01-Oct-2010",       "$9.00",                                         NA,
                "Prince Edward Island",   "01-Jun-2010",       "$8.70",                                         NA,
                "Prince Edward Island",   "01-Oct-2009",       "$8.40",                                         NA,
                "Prince Edward Island",   "01-Jun-2009",       "$8.20",                                         NA,
                "Prince Edward Island",   "01-Oct-2008",       "$8.00",                                         NA,
                "Prince Edward Island",   "01-May-2008",       "$7.75",                                         NA,
                "Prince Edward Island",   "01-Apr-2007",       "$7.50",                                         NA,
                "Prince Edward Island",   "01-Apr-2006",       "$7.15",                                         NA,
                "Prince Edward Island",   "01-Jan-2005",       "$6.80",                                         NA,
                "Prince Edward Island",   "01-Jan-2004",       "$6.50",                                         NA,
                "Prince Edward Island",   "01-Jan-2003",       "$6.25",                                         NA,
                "Prince Edward Island",   "01-Jan-2002",       "$6.00",                                         NA,
                "Prince Edward Island",   "01-Jan-2001",       "$5.80",                                         NA,
                "Prince Edward Island",   "01-Jan-2000",       "$5.60",                                         NA,
                "Prince Edward Island",   "01-Sep-1997",       "$5.40",                                         NA,
                "Prince Edward Island",   "01-Sep-1996",       "$5.15",                                         NA,
                "Prince Edward Island",   "01-Apr-1991",       "$4.75",                                         NA,
                "Prince Edward Island",   "01-Apr-1989",       "$4.50",                                         NA,
                "Prince Edward Island",   "01-Oct-1988",       "$4.25",                                         NA,
                "Prince Edward Island",   "01-Oct-1985",       "$4.00",                                         NA,
                "Prince Edward Island",   "01-Oct-1982",       "$3.75",                                         NA,
                "Prince Edward Island",   "01-Jul-1981",       "$3.30",                                         NA,
                "Prince Edward Island",   "01-Jul-1980",       "$3.00",                                         NA,
                "Prince Edward Island",   "26-Nov-1978",       "$2.75",                                         NA,
                "Prince Edward Island",   "01-Jul-1977",       "$2.70",                                         NA,
                "Prince Edward Island",   "01-Jul-1976",       "$2.50",                                         NA,
                "Prince Edward Island",   "01-Oct-1975",       "$2.30",                                         NA,
                "Prince Edward Island",   "01-Jan-1975",       "$2.05",                                         NA,
                "Prince Edward Island",   "01-Jul-1974",       "$1.75",                                         NA,
                "Prince Edward Island",   "01-Jan-1974",       "$1.65",                                         NA,
                "Prince Edward Island",   "01-Jul-1973",       "$1.40",                                     "Men.",
                "Prince Edward Island",   "01-Jul-1973",       "$1.30",                                   "Women.",
                "Prince Edward Island",   "01-Jul-1972",       "$1.10",                                   "Women.",
                "Prince Edward Island",   "01-Sep-1969",       "$1.25",                                     "Men.",
                "Prince Edward Island",   "01-Jul-1969",       "$0.95",                                   "Women.",
                "Prince Edward Island",   "01-Jan-1969",       "$0.85",                                   "Women.",
                "Prince Edward Island",   "01-Jul-1968",       "$0.80",                                   "Women.",
                "Prince Edward Island",   "16-Apr-1966",       "$1.10",                                     "Men.",
                "Prince Edward Island",   "01-Jan-1965",       "$1.00", "Men. Existing rate as of January 1, 1965"
                ) %>% 
  select(-Note)


## SASKATCHEWAN WAGES 1965-2021
sk_wages <- tibble::tribble(
                 ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                                                                                                                                                                                                                          ~Note,
                "Saskatchewan",   "01-Oct-2021",      "$11.81",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Oct-2020",      "$11.45",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Oct-2019",      "$11.32",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Oct-2018",      "$11.06",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Oct-2017",      "$10.96", "On October 1 of each year, this rate increases based on the average of the percentage change in the Consumer Price Index and the percentage change in average hourly wage for Saskatchewan during the previous year. Minimum wage increases are subject to Cabinet approval.",
                "Saskatchewan",   "01-Oct-2016",      "$10.72", "On October 1 of each year, this rate increases based on the average of the percentage change in the Consumer Price Index and the percentage change in average hourly wage for Saskatchewan during the previous year. Minimum wage increases are subject to Cabinet approval.",
                "Saskatchewan",   "01-Oct-2015",      "$10.50", "On October 1 of each year, this rate increases based on the average of the percentage change in the Consumer Price Index and the percentage change in average hourly wage for Saskatchewan during the previous year. Minimum wage increases are subject to Cabinet approval.",
                "Saskatchewan",   "01-Oct-2014",      "$10.20",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Dec-2012",      "$10.00",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Sep-2011",       "$9.50",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-May-2009",       "$9.25",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-May-2008",       "$8.60",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-2008",       "$8.25",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Mar-2007",       "$7.95",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Mar-2006",       "$7.55",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Sep-2005",       "$7.05",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Nov-2002",       "$6.65",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-May-2002",       "$6.35",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-1999",       "$6.00",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Dec-1996",       "$5.60",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Dec-1992",       "$5.35",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jul-1990",       "$5.00",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-1990",       "$4.75",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Aug-1985",       "$4.50",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-1982",       "$4.25",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jul-1981",       "$4.00",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-1981",       "$3.85",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-May-1980",       "$3.65",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Oct-1979",       "$3.50",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "30-Jun-1978",       "$3.25",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "31-Jan-1978",       "$3.15",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-1977",       "$3.00",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jan-1976",       "$2.80",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "31-Mar-1975",       "$2.50",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "02-Jul-1974",       "$2.25",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Dec-1973",       "$2.00",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jul-1972",       "$1.75",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "02-Jan-1972",       "$1.70",                                                                                                                                                                                                                                                                             NA,
                "Saskatchewan",   "01-Jun-1971",       "$1.50",                                                                       "Metropolitan areas (Estevan, Melville, Moose Jaw, North Battleford, Prince Albert, Regina, Saskatoon, Swift Current, Weyburn and Yorktown). The rate outside metropolitan areas was set at $0.10 less.",
                "Saskatchewan",   "01-Oct-1969",       "$1.25",                                                                       "Metropolitan areas (Estevan, Melville, Moose Jaw, North Battleford, Prince Albert, Regina, Saskatoon, Swift Current, Weyburn and Yorktown). The rate outside metropolitan areas was set at $0.10 less.",
                "Saskatchewan",   "01-Oct-1968",       "$1.05",                                                                       "Metropolitan areas (Estevan, Melville, Moose Jaw, North Battleford, Prince Albert, Regina, Saskatoon, Swift Current, Weyburn and Yorktown). The rate outside metropolitan areas was set at $0.10 less.",
                "Saskatchewan",   "01-Sep-1966",      "$40.00",                                                      "Weekly. Metropolitan areas (Estevan, Melville, Moose Jaw, North Battleford, Prince Albert, Regina, Saskatoon, Swift Current, Weyburn and Yorktown). The rate outside metropolitan areas was set at $2.00 less per week.",
                "Saskatchewan",   "01-May-1965",      "$38.00",                                                      "Weekly. Metropolitan areas (Estevan, Melville, Moose Jaw, North Battleford, Prince Albert, Regina, Saskatoon, Swift Current, Weyburn and Yorktown). The rate outside metropolitan areas was set at $2.00 less per week.",
                "Saskatchewan",   "01-Jan-1965",      "$36.50",                                                      "Weekly. Metropolitan areas (Estevan, Melville, Moose Jaw, North Battleford, Prince Albert, Regina, Saskatoon, Swift Current, Weyburn and Yorktown). The rate outside metropolitan areas was set at $2.00 less per week."
                ) %>% 
  select(-Note)


## YUKON WAGES 1965-2021
yk_wages <- tibble::tribble(
                ~Jurisdiction, ~Effective.Date, ~Minimum.Wage,                                                                                                                                                                                                                       ~Note,
                      "Yukon",   "01-Apr-2021",      "$13.85",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Apr-2020",      "$13.71",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Apr-2019",      "$12.71",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Apr-2018",      "$11.51",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2017",      "$11.32",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2016",      "$11.07",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2015",      "$10.86",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2014",      "$10.72",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2013",      "$10.54",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-May-2012",      "$10.30",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2012",       "$9.27",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2011",       "$9.00",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2010",       "$8.93",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2009",       "$8.89",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2008",       "$8.58",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Apr-2007",       "$8.37",                                             "On April 1 of each year, this rate increases by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-May-2006",       "$8.25", "Effective April 1, 2007, and on April 1 of each subsequent year, this rate will increase by an amount corresponding to the annual increase for the preceding year in the Consumer Price Index for the city of Whitehorse.",
                      "Yukon",   "01-Oct-1998",       "$7.20",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Apr-1998",       "$7.06",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Oct-1995",       "$6.86",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Oct-1994",       "$6.72",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Apr-1991",       "$6.24",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Apr-1990",       "$5.97",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-May-1988",       "$5.39",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-May-1987",       "$4.75",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Jan-1985",       "$4.25",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-May-1981",       "$3.60",                                                                                                                                                                                               "Federal rate plus ten cents",
                      "Yukon",   "01-Dec-1980",       "$3.35",                                                                                                                                                                                               "Federal rate plus ten cents",
                      "Yukon",   "01-Apr-1976",       "$3.00",                                                                                                                                                                                               "Federal rate plus ten cents",
                      "Yukon",   "23-Jul-1975",       "$2.70",                                                                                                                                                                                               "Federal rate plus ten cents",
                      "Yukon",   "01-Apr-1974",       "$2.30",                                                                                                                                                                                               "Federal rate plus ten cents",
                      "Yukon",   "01-Jun-1973",       "$2.00",                                                                                                                                                                                               "Federal rate plus ten cents",
                      "Yukon",   "01-Jan-1972",       "$1.75",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-May-1970",       "$1.50",                                                                                                                                                                                                                          NA,
                      "Yukon",   "01-Jul-1968",       "$1.25",                                                                                                                                                                                                                          NA
                ) %>% 
  select(-Note)
  
## USE RBIND TO COMBINE THE PROVINCES INTO ONE DATASET
prov_min_wage <- rbind(ontario_wages, quebec_waves, yk_wages, nv_wages, nw_wages, bc_wages, nl_wages, pei_wages, ns_wages, sk_wages, manitoba_wages,
      nb_wages, alberta_wages)

glimpse(prov_min_wage)


## CHANGE EFFECTIVE.DATE COLUMN TO DATE AND MIMIMUM.WAGE TO NUMERIC AFTER
## REMOVING THE '$' SYMBOL
prov_min_wage <- prov_min_wage %>% 
  mutate(Date = dmy(Effective.Date)) %>% 
  mutate(Year = year(Date)) %>% 
  mutate(wage = str_replace(Minimum.Wage, "[$]","")) %>% 
  mutate(wage = as.numeric(wage)) %>% 
  select(-Effective.Date)


summary(prov_min_wage)

## CREATE A NEW DATASET ARRANGE DATA BY YEAR AND GROUPING BY JURIDICTION AND YEAR
## ADD COLUMNS 'avg_wage' and 'median_wage' FOR ANALYSIS
min_wages <- prov_min_wage %>% 
  arrange(Year) %>% 
  group_by(Jurisdiction, Year) %>% 
  summarise(avg_wage = mean(wage),
            median_wage = median(wage),
            .groups = "drop")


## CREATE A TIBBLE OF THE PROVINCIAL ABBREVIATIONS FOR MERGING LATER
prov_abbrev <- tibble::tribble(
                 ~Canadian.Provinces.and.Territories, ~`Two-Letter.Abbreviation`,
                                           "Alberta",                       "AB",
                                  "British Columbia",                       "BC",
                                          "Manitoba",                       "MB",
                                     "New Brunswick",                       "NB",
                         "Newfoundland and Labrador",                       "NL",
                             "Northwest Territories",                       "NT",
                                       "Nova Scotia",                       "NS",
                                           "Nunavut",                       "NU",
                                           "Ontario",                       "ON",
                              "Prince Edward Island",                       "PE",
                                            "Quebec",                       "QC",
                                      "Saskatchewan",                       "SK",
                                             "Yukon",                       "YT"
                 )


## SHORTEN THE NAMES OF THE COLUMNS IN 'prov_abbrev'
prov_abbrev <- prov_abbrev %>% 
  rename(Province = Canadian.Provinces.and.Territories,
         Abbreviation = `Two-Letter.Abbreviation`)




## JOIN THE TWO DATASETS 'min_wages' and 'prov_abbrev' WITH AN INNER_JOIN
min_wages_can <- min_wages %>% 
  inner_join(prov_abbrev, by = c("Jurisdiction" = "Province")) 



## SAVE THE NEW DATASET FOR FURTHER ANALYSIS
write_csv(min_wages_can, "data/min_wages_canada_prov_1965-2020.csv")


