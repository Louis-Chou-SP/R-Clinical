result <- mtcars %>% 
  mutate(
    efficiency = case_when(
      mpg >= 25 ~ "Efficient",
      mpg >= 15 ~ "Average",
      TRUE      ~ "Inefficient"
    )
  ) %>%
  select(mpg, efficiency)

result %>% 
  group_by(efficiency) %>% 
  summarise(n = n())

result2 <- mtcars %>%
  mutate(
    car_type = case_when(
      cyl == 4 & mpg >= 25 ~ "Eco",
      cyl == 8             ~ "Performance",
      TRUE                 ~ "Standard"
    )
  ) %>%
  select(cyl, mpg, car_type)

result2 %>%
  group_by(car_type) %>%
  summarise(n = n())

drug_data <- data.frame(
  subjid  = c("ABC-001", "ABC-002", "ABC-003"),
  drug_nm = c("  Aspirin  ", "IBUPROFEN", "paracetamol")
)

result3 <- drug_data %>%
  mutate(
    id_num = str_sub(subjid, 5),
    drug_clean = str_trim(drug_nm),
    drug_upper = str_to_upper(drug_clean)
  )
result3

timeline <- data.frame(
  subjid   = c("001", "002", "003"),
  rand_dt  = as.Date(c("2024-01-15", "2024-02-01", "2024-03-10")),
  end_dt   = as.Date(c("2024-06-30", "2024-07-15", "2024-09-01"))
)

result4 <- timeline %>%
  mutate(
    duration = as.numeric(end_dt - rand_dt),
    rand_year = year(rand_dt),
    rand_month = month(rand_dt)
  )
result4

# Subject-level data (like DM)
dm <- data.frame(
  USUBJID = c("001", "002", "003", "004"),
  AGE     = c(25, 34, 45, 28),
  SEX     = c("M", "F", "M", "F"),
  TRT     = c("Drug A", "Drug A", "Placebo", "Placebo")
)

# Adverse Event data (like AE)
ae <- data.frame(
  USUBJID = c("001", "001", "002", "003", "005"),
  AETERM  = c("Headache", "Nausea", "Fatigue", "Headache", "Rash"),
  AESEV   = c("MILD", "MODERATE", "MILD", "SEVERE", "MILD")
)

ae_dm1 <- dm %>%
  left_join(ae, by = "USUBJID")
ae_dm1

ae_dm2 <- dm %>%
  inner_join(ae, by = "USUBJID")
ae_dm2

ae_dm3 <- dm %>%
  full_join(ae, by = "USUBJID")
ae_dm3

ae_dm4 <- dm %>%
  anti_join(ae, by = "USUBJID")
ae_dm4

adlb_sim <- data.frame(
  USUBJID = c("001", "001", "001", "002", "002"),
  PARAM   = c("ALT", "ALT", "ALT", "ALT", "ALT"),
  AVISIT  = c("Baseline", "Week 4", "Week 8",
              "Baseline", "Week 4"),
  AVAL    = c(25, 30, 28, 40, 35)
)

adlb1 <- adlb_sim %>%
  group_by(USUBJID, PARAM) %>%
  mutate(
    BASE = first(AVAL),
    CHG = AVAL - BASE
  )%>%
  ungroup()
adlb1

adlb2 <- adlb_sim %>%
  group_by(USUBJID) %>%
  summarise(
    MEAN = mean(AVAL)
  )%>%
  ungroup()
adlb2

# 3-1
result1 <- dm_pilot %>%
  group_by(ARM) %>%
  summarise(
    n = n(),
    mean_age = mean(AGE),
    sd_age = sd(AGE)
  )
  #select(ARM, n, mean_age, sd_age)
result1

# 3-2
result2 <- ae_pilot %>%
  filter(AESEV == "SEVERE") %>%
  arrange(USUBJID) %>%
  select(USUBJID, AETERM, AESEV, AESTDTC)

result2

#3-3
result3 <- ae_pilot %>%
  left_join(
    dm_pilot %>% select(USUBJID, ARM),
    by = "USUBJID"
  ) %>%
  group_by(ARM) %>%
  summarise(n = n()) %>%
  select(ARM, n)
result3

#3-4
result4_ <- dm_pilot %>%
  anti_join(ae_pilot %>% select(USUBJID), by = "USUBJID")

ae_free_n   <- nrow(result4_)
total_n     <- nrow(dm_pilot)
ae_free_pct <- ae_free_n / total_n * 100

cat("AE-free subjects:", ae_free_n, "/", total_n,
    "(", round(ae_free_pct, 1), "%)")