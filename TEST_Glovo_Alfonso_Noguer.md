---
title: "Glovo Technical Test"
author: "Alfonso Noguer"
date: "3/10/2019"

output:
 html_document:
   keep_md: TRUE
   theme: cerulean
   toc: true
   toc_depth: 3
   toc_float:
     collapsed: true
     smooth_scroll: true
---



## 1. Distance

Create an SQL query that shows the distance between the courier starting position and the pickup point, as well as the distance between the pickup point and the delivery point.

We have two tables One with the origin and another with the destinations.

For answering all the distances we would need 3 positions and we only have two so we can't calculate both distances.


### SQL

In SQL we do something similar first we create 2 new tables with the filtered values we care about.

Then we join both of them and calculate the distance in the select (we use euclidean in this example but easily it could be manhattan)


```sql
SELECT 
  a.id,
  a.courier_id,
  a.point_type, 
  SQR(POWER(a.acceptance_latitude - b_latitude,2) + 
        POWER(a.acceptance_longitude - b.longitude,2) AS distance
WHERE "contidion"
FROM orders AS a, order_points AS b
```

Being the condition whatever you want like **a.courier_id = "number"** or 
**a.coustomer_id = "number"**


### R

To calculate the distance I have two options one is to directly join all both dataframes and mutate the columns to get a distance, but that would take an awful amount of time and memory so I discard that option.

The second approach is to filter the 4 coordinates long/lat of origin and destinations and calculate the distance for those.

To do this we use the ID as the Key for both tables so we can know how to join origin and destination.


```r
pacman::p_load(tidyverse)

#if we want to filter by something different than ID we use something like

key <- order_points %>% filter(order_id == "id we want to search") %>% select(id)

x1 <- orders %>% filter(id == "id/key we want to search") %>% select(acceptance_latitude)

y1 <- orders %>% filter(id == "id/key we want to search") %>% select(acceptance_longitude)

x2 <- order_points %>% filter(id == "id/key we want to search") %>% select(latitude)

y2 <- order_points %>% filter(id == "id/key we want to search") %>% select(longitude)

length_manhattan <- abs(x1-x2) + abs(y1-y2) 
# or
length_euclidean <- (x1-x2)^2 + (y1-y2)^2
```

This is pretty basic code I could optimize the filtering and return 2 vectors or even join both tables filter and calculate the distance and return the output. 
And especially fit it in a functions but I think this is more clearer and easier to understand.


## 2. Cohort Retention

Build one SQL query to create a cohort of Signup to First Order and show the result.
The objective of this cohort is to see, out of the users that signed up in Week N, how many did their first order in Week N+1, N+2, N+3...


### SQL

First, we bucket them into different cohort by their sign up month and store into **cohort_items**.
After that, we build **user_activities** which would return all the pairs of **(user_id, week_number)** that indicates if a user is active in that month after their original signup date.
The **WEEK_DIFF** is a function that takes in 2 dates, and return the number of months between them.
**Cohort Size:** is simply how many users are in each group.
And finally, I put everything together.


```sql
WITH cohort_items AS (
  SELECT
    date_trunc("week", O.activation_time_local)::date as cohort_week,
    cutomer_id
  FROM orders O
  order by 1, 2
)

WITH user_activities AS (
  SELECT
    A.user_id,
    WEEK_DIFF(
      date_trunc("week", U.timestamp)::date,
      C.cohort_week
    ) AS week_number
  FROM users U
  LEFT JOIN cohort_items C ON U.id = C.customer_id
  GROUP BY 1, 2
)

WITH cohort_size AS (
  select cohort_week, count(1) AS num_users
  FROM cohort_items
  GRUP BY 1
  ORDER BY 1
)

WITH retention_table AS (
  SELECT
    C.cohort_week,
    A.week_number,
    count(1) AS num_users
  FROM user_activities A
  LEFT JOIN cohort_items C ON A.user_id = C.user_id
  GROUP BY 1, 2
)

SELECT
  B.cohort_week,
  S.num_users AS total_users,
  B.weeek_number,
  B.num_users::float * 100 / S.num_users AS percentage
FROM retention_table B
LEFT JOIN cohort_size S ON B.cohort_week = S.cohort_week
WHERE B.cohort_week IS NOT NULL
ORDER BY 1, 3

```


### R

To do this in R we generate a function that returns the unique users in a time period in any of our dataframes.

With that function, I do a double loop the first loop gives us the respective cohort, and the second calculates the retention rate by each successive time period.
to make it easier to process al this process is in a function where the inputs are the two dataframes of the problem, the starting date in which we want the process to start (by default the first of January 2015 the year Glovo launched)
and the interval of the cohorts in days (default 7 days, 1 week as the question indicates).


```r
pacman::p_load(lubridate,tidyverse)
#first we generate a function that finds the date_time in the dataframe 
#and returns the unique coustomer_ids if existing and costomer ids if not
#I could add a condition that detects with column is the time_stamp and selects
#
users_period_of_time <- function(orders, start_date= dmy(01-01-2015),
                                time_increment_in_days = 7){
  return(
    orders %>% 
      filter(timestamp > start_date &
               timestamp < (start_date + time_increment_in_days)) %>% 
      select(ifelse(customer_id %in% names(orders),"customer_id","id")) %>% 
      unique())
}

retention <- function(users, orders, start_date= dmy(01-01-2015),
                      time_increment_days = 7){
  last_date <- max(orders$timestamp)
  date <- start_date
  results <- list()
  while (date + time_increment_in_days < last_date) {
    users <- users_period_of_time(orders = users,
                                  start_date = date,
                                  time_increment_in_days = time_increment_days)
    total_users <- length(users)
    retention <- 0
    period <- 0
    vector <- c()
    while (retention < 1 |
           date + time_increment_in_days * period > last_date)
      {
      uniq_users_period <- users[users %in% users_period_of_time(orders = orders,
                                                        start_date = time_segment)]
      interesting_users <- users[users %in% uniq_users_period]
      users <- users[!users %in% uniq_users_period]
      retention <- retention + length(interesting_users)/total_users
      vector <- c(vector,paste0("rentention ",round(retention,2)," in period ",period))
      period = period + 1
    }#close second while
    row_name <- paste0("cohort_",date)
    results[[row_name]] <- c(paste0("total users ",total_users,vector))
    date <- date + time_increment_in_days
  }#close first while
  return(results)
}
```

This code could also be optimized, for example, use nested functions instead of loops. But I'm not that adept with that technique and I would need a sample to iterate with. This is the best I am comfortable without data.

## 3. Restaurant Ranking

Let’s say you have one table: **events**. This table stores all the events from the app, from screen impressions to any kind of user activity. For instance, if we click on the **Restaurant** category, that will store a screen impression event in the table.

This table has a field **attributes_json** with a JSON formatted string with information regarding the Restaurants shown to the user.

Considering the available information above, build an SQL query that retrieves the average restaurant position per month, country and city. Consider we have only 100 available restaurants in the Food category and the JSON field stores all of the positions.


### SQL

This problem is solved by selecting the **storeIds** we want from the nested JSON in the table, once we have those **storeIds**, I just need to go to the **reviews_table** were there are the reviews of all restaurants **storeID** and the reviews they had **review_value** and the **timestamp** of the review.

Then we filter by the stores we found from the frist table and the month we care about, group by **storeID** sort by descending **AVG(review_value)**.

For this example, the month we are going to filter is September of 2019.


```sql
WITH selected_stores AS (
  SELECT
    json.storeIds AS stores 
  FROM events, UNNEST(attributes_json) AS json WHERE json.country_code = "country code ej. ES" & json.city_code = "city code ej. BCN"
)

SELECT
  r.storeIds AS stores
  AVG(r.review_value) AS average_score
FROM 
  reviews_table AS r,
  selected_stores AS s
WHERE 
  r.storeIds = selected_stores.stores 
  AND
  MONTH(r.timestamp) = 9 AND YEAR(r.timestamp) = 2019
GROUP BY average_score DESC
  
```

I thought of adding the Country and location but it felt like it would clutter the results.

Thank you for your time and attention.<br/><br/>
[Alfonso Noguer](https://www.linkedin.com/in/alfonso-noguer/)
