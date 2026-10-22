# Run once if needed:
# install.packages("knitr")

library(knitr)
file_path <- "~/Desktop/Data506 HW3/Australian Records/au-500.csv"

# Import all columns as character to preserve leading zeros
australia <- read.csv(
  file_path,
  stringsAsFactors = FALSE,
  colClasses = "character",
  na.strings = c("", "NA")
)

# Check the imported data
dim(australia)
names(australia)
head(australia)

stopifnot(nrow(australia) == 500)


# a) Percentage of websites ending in .com

websites <- tolower(trimws(australia$web))

# Remove the protocol, such as http:// or https://.
hosts <- sub("^[a-z]+://", "", websites)

# Remove paths, query strings, and fragments.
hosts <- sub("[/?#].*$", "", hosts)

# Remove a port number or trailing dot if present.
hosts <- sub(":[0-9]+$", "", hosts)
hosts <- sub("\\.$", "", hosts)

valid_websites <- !is.na(hosts) & nzchar(hosts)

# The final $ requires .com to occur at the end.
# This excludes domains ending in .com.au.
is_com <- grepl("\\.com$", hosts[valid_websites])

result_a <- data.frame(
  Websites = sum(valid_websites),
  Dot_com_websites = sum(is_com),
  Percentage_dot_com = 100 * mean(is_com)
)

print(kable(
  result_a,
  digits = 2,
  caption = "Part a: Percentage of websites ending in .com"
))

cat(sprintf(
  "\n%.2f%% of nonmissing websites end in .com.\n",
  result_a$Percentage_dot_com
))

# b) Most Common Email Domain

emails <- tolower(trimws(australia$email))

# Retain nonmissing email addresses with one @ and no whitespace.
valid_emails <- !is.na(emails) &
  grepl("^[^@[:space:]]+@[^@[:space:]]+$", emails)

# Remove everything through the @ symbol.
email_domains <- sub("^[^@]+@", "", emails[valid_emails])

domain_counts <- sort(
  table(email_domains),
  decreasing = TRUE
)

domain_table <- data.frame(
  Domain = names(domain_counts),
  Count = as.integer(domain_counts),
  row.names = NULL
)

# Retain all domains tied for the highest count.
most_common <- domain_table[
  domain_table$Count == max(domain_table$Count),
]

print(kable(
  most_common,
  caption = "Part b: Most common email domain(s)"
))

# Also display the ten most frequent domains.
print(kable(
  head(domain_table, 10),
  caption = "Part b: Ten most frequent email domains"
))

# c) Company names with non-alphabetic character

companies <- trimws(australia$company_name)
companies <- companies[!is.na(companies) & nzchar(companies)]

# [^...] matches a character NOT in the listed categories.
# Allow letters, commas, and whitespace.
has_other_character <- grepl(
  "[^[:alpha:],[:space:]]",
  companies
)

# Also allow ampersands.
has_other_except_ampersand <- grepl(
  "[^[:alpha:],[:space:]&]",
  companies
)

result_c <- data.frame(
  Rule = c(
    "Exclude commas and whitespace",
    "Exclude commas, whitespace, and ampersands"
  ),

  Companies_with_eligible_character = c(
    sum(has_other_character),
    sum(has_other_except_ampersand)
  ),

  Total_companies = length(companies),

  Proportion = c(
    mean(has_other_character),
    mean(has_other_except_ampersand)
  )
)

print(kable(
  result_c,
  digits = 4,
  caption = "Part c: Proportion of company names with eligible characters"
))

# d) Format both phone columns as XXXX-XXX-XXX

# Preserve original phone numbers for comparison.
australia$phone1_original <- australia$phone1
australia$phone2_original <- australia$phone2

format_phone <- function(phone) {

  # Remove everything except digits.
  digits <- gsub("[^0-9]", "", phone)

  # Stop if a nonmissing phone number does not have 10 digits.
  invalid <- !is.na(digits) & nchar(digits) != 10

  if (any(invalid)) {
    stop("Some nonmissing phone numbers do not contain 10 digits.")
  }

  # Divide the digits into groups of 4, 3, and 3.
  sub(
    "^([0-9]{4})([0-9]{3})([0-9]{3})$",
    "\\1-\\2-\\3",
    digits
  )
}

australia$phone1 <- format_phone(australia$phone1)
australia$phone2 <- format_phone(australia$phone2)

# Check that every nonmissing number has the required format.
stopifnot(all(
  is.na(australia$phone1) |
    grepl("^[0-9]{4}-[0-9]{3}-[0-9]{3}$", australia$phone1)
))

stopifnot(all(
  is.na(australia$phone2) |
    grepl("^[0-9]{4}-[0-9]{3}-[0-9]{3}$", australia$phone2)
))

# Verify that formatting did not change the digits.
stopifnot(identical(
  gsub("[^0-9]", "", australia$phone1_original),
  gsub("[^0-9]", "", australia$phone1)
))

stopifnot(identical(
  gsub("[^0-9]", "", australia$phone2_original),
  gsub("[^0-9]", "", australia$phone2)
))

print(kable(
  head(australia[c("phone1", "phone2")], 10),
  caption = "Part d: First ten phone numbers in each reformatted column"
))

# This changes formatting only; it does not turn a landline
# into an actual mobile telephone number.

# e) Histogram of log apartment numbers

addresses <- trimws(australia$address)

# Following the question, any digits at the end of an address
# are treated as an apartment number.
has_apartment <- !is.na(addresses) &
  grepl("[0-9]+$", addresses)

apartment_text <- rep(NA_character_, length(addresses))

# Extract the complete final sequence of digits.
apartment_text[has_apartment] <- regmatches(
  addresses[has_apartment],
  regexpr("[0-9]+$", addresses[has_apartment])
)

australia$apartment_number <- as.numeric(apartment_text)

# Logarithms require positive values.
positive_apartments <- australia$apartment_number[
  !is.na(australia$apartment_number) &
    australia$apartment_number > 0
]

stopifnot(length(positive_apartments) > 0)

apartment_summary <- data.frame(
  Total_addresses = nrow(australia),
  With_trailing_number = sum(has_apartment),
  Positive_apartment_numbers = length(positive_apartments),
  Zero_apartment_numbers = sum(
    australia$apartment_number == 0,
    na.rm = TRUE
  )
)

print(kable(
  apartment_summary,
  caption = "Part e: Apartment-number extraction summary"
))

# Show examples to check the extraction.
print(kable(
  head(
    australia[
      has_apartment,
      c("address", "apartment_number")
    ],
    10
  ),
  caption = "Part e: Examples of extracted apartment numbers"
))

# log() is the natural logarithm in R.
hist(
  log(positive_apartments),
  breaks = "Sturges",
  col = "lightblue",
  border = "white",
  main = "Log of apartment numbers",
  xlab = "Natural log of apartment number",
  ylab = "Frequency"
)

# f) Compare leading digits with Benford's law

# Use positive apartment numbers only.
# Remove leading zeros from the extracted text before
# identifying the first significant digit

positive_rows <- !is.na(australia$apartment_number) &
  australia$apartment_number > 0

significant_digits <- sub(
  "^0+",
  "",
  apartment_text[positive_rows]
)

first_digits <- as.integer(
  substr(significant_digits, 1, 1)
)

# Include all digits 1–9, even if some have zero observations.
observed_counts <- as.integer(
  table(factor(first_digits, levels = 1:9))
)

n_apartments <- sum(observed_counts)

benford_probabilities <- log10(1 + 1 / (1:9))
observed_proportions <- observed_counts / n_apartments
expected_counts <- n_apartments * benford_probabilities

benford_table <- data.frame(
  First_digit = 1:9,
  Observed_count = observed_counts,
  Expected_count = expected_counts,
  Observed_proportion = observed_proportions,
  Benford_proportion = benford_probabilities
)

print(kable(
  benford_table,
  digits = c(0, 0, 2, 4, 4),
  caption = "Part f: Observed leading digits versus Benford's law"
))


# Compare observed and expected proportions visually.
barplot(
  rbind(observed_proportions, benford_probabilities),
  beside = TRUE,
  names.arg = 1:9,
  col = c("steelblue", "orange"),
  legend.text = c("Observed", "Benford"),
  args.legend = list(x = "topright", bty = "n"),
  ylim = c(
    0,
    1.25 * max(observed_proportions, benford_probabilities)
  ),
  main = "Apartment leading digits versus Benford's law",
  xlab = "First significant digit",
  ylab = "Proportion"
)


# Chi-squared goodness-of-fit test.
# H0: Leading-digit probabilities follow Benford's law.
# Use a simulated p-value if any expected count is below 5.

set.seed(506)

use_simulation <- any(expected_counts < 5)

benford_test <- chisq.test(
  x = observed_counts,
  p = benford_probabilities,
  simulate.p.value = use_simulation,
  B = 10000
)

print(benford_test)

test_table <- data.frame(
  Test = if (use_simulation) {
    "Chi-squared test with Monte Carlo p-value"
  } else {
    "Chi-squared goodness-of-fit test"
  },

  Statistic = unname(benford_test$statistic),

  P_value = format.pval(
    benford_test$p.value,
    digits = 3,
    eps = 0.001
  )
)

print(kable(
  test_table,
  digits = 3,
  caption = "Part f: Test of agreement with Benford's law"
))


# Interpret the result.
if (benford_test$p.value < 0.05) {

  cat(
    "\nReject H0 at the 5% level: the leading-digit distribution",
    " differs significantly from Benford's law.\n"
  )

} else {

  cat(
    "\nDo not reject H0 at the 5% level: there is insufficient",
    " evidence of a difference from Benford's law.\n"
  )

}

cat(
  "\nThis test alone cannot establish whether the data are real.",
  " Apartment numbers are assigned identifiers and may be",
  " constrained to limited ranges, so genuine apartment numbers",
  " need not follow Benford's law. Likewise, agreement with",
  " Benford's law would not prove authenticity.",
  " The source explicitly identifies this dataset as synthetic.\n"
)


# Reference:
# https://www.briandunning.com/sample-data/