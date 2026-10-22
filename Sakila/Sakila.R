# Run once if needed:
# install.packages(c("DBI", "RSQLite", "microbenchmark", "knitr"))

library(DBI)
library(RSQLite)
library(microbenchmark)
library(knitr)


# Set up the files
db_path <- "~/Desktop/Data506 HW3/Sakila/sakila_master.db"

con <- dbConnect(
  SQLite(),
  dbname = db_path,
  flags = SQLITE_RO
)

# List available tables
dbListTables(con)


# Helper: check if both approaches return the same values
same_results <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL

  isTRUE(all.equal(
    x, y,
    check.attributes = FALSE,
    tolerance = 1e-8
  ))
}

# Helper: make a readable timing table
timing_table <- function(benchmark) {
  results <- summary(benchmark, unit = "ms")

  data.frame(
    Approach = as.character(results$expr),
    Minimum_ms = results$min,
    Median_ms = results$median,
    Mean_ms = results$mean,
    Maximum_ms = results$max,
    Repetitions = results$neval
  )
}

# Helper: interpret the measured median times
interpret_timing <- function(benchmark) {
  results <- timing_table(benchmark)

  faster <- which.min(results$Median_ms)
  slower <- setdiff(seq_len(nrow(results)), faster)

  cat(sprintf(
    paste0(
      "\n%s had the lower median time: %.3f ms versus %.3f ms.\n",
      "It was approximately %.2f times faster in this run.\n"
    ),
    results$Approach[faster],
    results$Median_ms[faster],
    results$Median_ms[slower],
    results$Median_ms[slower] / results$Median_ms[faster]
  ))
}

# a) Customer count and active percentage for each store


# First way: SQL extraction followed by R

answer_a_r <- function(con) {

  customers <- dbGetQuery(
    con,
    "SELECT customer_id, store_id, active FROM customer"
  )

  # Separate customers into groups by store
  groups <- split(customers, customers$store_id)

  # Calculate the customer count and active percentage
  result <- do.call(rbind, lapply(groups, function(group) {
    data.frame(
      store_id = group$store_id[1],
      customers = nrow(group),
      active_percentage =
        100 * sum(group$active == 1, na.rm = TRUE) / nrow(group)
    )
  }))

  result <- result[order(result$store_id), ]
  rownames(result) <- NULL

  result
}

a_r <- answer_a_r(con)

print(kable(
  a_r,
  digits = 2,
  caption = "Part a: SQL extraction followed by R"
))


# Second Way: Single SQL query

sql_a <- "
SELECT
  store_id,
  COUNT(*) AS customers,
  100.0 * SUM(CASE WHEN active = 1 THEN 1 ELSE 0 END)
    / COUNT(*) AS active_percentage
FROM customer
GROUP BY store_id
ORDER BY store_id
"

answer_a_sql <- function(con) {
  dbGetQuery(con, sql_a)
}

a_sql <- answer_a_sql(con)

print(kable(
  a_sql,
  digits = 2,
  caption = "Part a: Single SQL query"
))

# Verify that both methods agree.
stopifnot(same_results(a_r, a_sql))


# Compare run time

set.seed(506)

benchmark_a <- microbenchmark(
  SQL_plus_R = answer_a_r(con),
  SQL_only = answer_a_sql(con),
  times = 100L
)

print(kable(
  timing_table(benchmark_a),
  digits = 3,
  caption = "Part a: Execution time in milliseconds"
))

interpret_timing(benchmark_a)

# b) Staff Members names and countries

# Use each staff member's recorded address:
# staff -> address -> city -> country.
# This identifies address country, not nationality.


# First Way: SQL extraction followed by R

answer_b_r <- function(con) {

  staff <- dbGetQuery(
    con,
    "SELECT staff_id, first_name, last_name, address_id FROM staff"
  )

  addresses <- dbGetQuery(
    con,
    "SELECT address_id, city_id FROM address"
  )

  cities <- dbGetQuery(
    con,
    "SELECT city_id, country_id FROM city"
  )

  countries <- dbGetQuery(
    con,
    "SELECT country_id, country FROM country"
  )

  # Join the four tables using their shared IDs.
  result <- merge(staff, addresses, by = "address_id")
  result <- merge(result, cities, by = "city_id")
  result <- merge(result, countries, by = "country_id")

  # Retain the requested information and identifying staff ID.
  result <- result[
    order(result$staff_id),
    c("staff_id", "first_name", "last_name", "country")
  ]

  rownames(result) <- NULL

  result
}

b_r <- answer_b_r(con)

print(kable(
  b_r,
  caption = "Part b: SQL extraction followed by R"
))


# Second Way: Single SQL query

sql_b <- "
SELECT
  s.staff_id,
  s.first_name,
  s.last_name,
  co.country
FROM staff AS s
JOIN address AS a
  ON s.address_id = a.address_id
JOIN city AS ci
  ON a.city_id = ci.city_id
JOIN country AS co
  ON ci.country_id = co.country_id
ORDER BY s.staff_id
"

answer_b_sql <- function(con) {
  dbGetQuery(con, sql_b)
}

b_sql <- answer_b_sql(con)

print(kable(
  b_sql,
  caption = "Part b: Single SQL query"
))

stopifnot(same_results(b_r, b_sql))


# Compare run time

benchmark_b <- microbenchmark(
  SQL_plus_R = answer_b_r(con),
  SQL_only = answer_b_sql(con),
  times = 100L
)

print(kable(
  timing_table(benchmark_b),
  digits = 3,
  caption = "Part b: Execution time in milliseconds"
))

interpret_timing(benchmark_b)

# c) FILMS RENTED FOR THE HIGHEST DOLLAR VALUE

# Interpretation:
# Find films associated with the largest individual rental
# payment, not the highest total revenue across all rentals.
#
# Join:
# payment -> rental -> inventory -> film.
#
# Keep all tied films, listing each film only once


# Verify that each rental has at most one payment in this database.
payment_ids <- dbGetQuery(
  con,
  "SELECT rental_id FROM payment WHERE rental_id IS NOT NULL"
)

stopifnot(anyDuplicated(payment_ids$rental_id) == 0L)


# First Way: SQL extraction followed by R

answer_c_r <- function(con) {

  payments <- dbGetQuery(
    con,
    "SELECT rental_id, amount FROM payment"
  )

  rentals <- dbGetQuery(
    con,
    "SELECT rental_id, inventory_id FROM rental"
  )

  inventory <- dbGetQuery(
    con,
    "SELECT inventory_id, film_id FROM inventory"
  )

  films <- dbGetQuery(
    con,
    "SELECT film_id, title FROM film"
  )

  # A payment without a rental ID cannot identify a film.
  payments <- payments[!is.na(payments$rental_id), ]

  result <- merge(payments, rentals, by = "rental_id")
  result <- merge(result, inventory, by = "inventory_id")
  result <- merge(result, films, by = "film_id")

  highest_payment <- max(result$amount, na.rm = TRUE)

  # Retain every film tied at the maximum payment.
  result <- result[
    !is.na(result$amount) & result$amount == highest_payment,
    c("film_id", "title", "amount")
  ]

  # Remove repeated rows for the same film and amount.
  result <- unique(result)

  names(result)[3] <- "rental_payment_usd"

  result <- result[order(result$title, result$film_id), ]
  rownames(result) <- NULL

  result
}

c_r <- answer_c_r(con)

print(kable(
  c_r,
  digits = 2,
  caption = "Part c: SQL extraction followed by R"
))


# Second Way: Approach 2: Single SQL query

# WITH defines an intermediate result within one SQL statement.
# DISTINCT removes duplicate film rows.
# The subquery finds the maximum payment.

sql_c <- "
WITH rental_payments AS (
  SELECT
    f.film_id,
    f.title,
    p.amount
  FROM payment AS p
  JOIN rental AS r
    ON p.rental_id = r.rental_id
  JOIN inventory AS i
    ON r.inventory_id = i.inventory_id
  JOIN film AS f
    ON i.film_id = f.film_id
)
SELECT DISTINCT
  film_id,
  title,
  amount AS rental_payment_usd
FROM rental_payments
WHERE amount = (
  SELECT MAX(amount)
  FROM rental_payments
)
ORDER BY title, film_id
"

answer_c_sql <- function(con) {
  dbGetQuery(con, sql_c)
}

c_sql <- answer_c_sql(con)

print(kable(
  c_sql,
  digits = 2,
  caption = "Part c: Single SQL query"
))

stopifnot(same_results(c_r, c_sql))

cat(sprintf(
  "\nThe highest individual rental payment is $%.2f, shared by %d films.\n",
  max(c_sql$rental_payment_usd),
  nrow(c_sql)
))


# Compare run time

benchmark_c <- microbenchmark(
  SQL_plus_R = answer_c_r(con),
  SQL_only = answer_c_sql(con),
  times = 100L
)

print(kable(
  timing_table(benchmark_c),
  digits = 3,
  caption = "Part c: Execution time in milliseconds"
))

interpret_timing(benchmark_c)

dbDisconnect(con)


# References:
# Database:
# https://github.com/bradleygrant/sakila-sqlite3
#
# DBI queries:
# https://dbi.r-dbi.org/reference/dbGetQuery.html
#
# microbenchmark:
# https://cran.r-project.org/web/packages/microbenchmark/microbenchmark.pdf