# Stub to load the data
stub.DiD <- "region_level_cumulative"

########################################
### Seattle and WA Separate Summary Statistics###
########################################

# Wage bins to be used (note "12" means "jobs paying <= $12.99")

# Read file 
df <- read.dta13(paste0(path.to.data,stub.DiD,".dta", collapse = "")) %>%
  mutate(seattle = ifelse(region0 == 1, 1, 0)) %>%  
  filter(yearquarter > 20141 & yearquarter < 20164)

# Create Table 5
table5 <- df %>%
  select(c(yearquarter, seattle, ends_with("12"), ends_with("18"), ends_with("40"))) %>%
  select(-c(starts_with("d_"))) %>% select(-c(contains("wagerate"))) %>%
  group_by(seattle, yearquarter) %>%
  summarise(cum_nworkers_beg12 = sum(cum_nworkers_beg12),
            cum_nworkers_beg18 = sum(cum_nworkers_beg18),
            cum_nworkers_beg40 = sum(cum_nworkers_beg40),
            cum_hours_flow12 = sum(cum_hours_flow12),
            cum_hours_flow18 = sum(cum_hours_flow18),
            cum_hours_flow40 = sum(cum_hours_flow40),
            cum_payroll_flow12 = sum(cum_payroll_flow12),
            cum_payroll_flow18 = sum(cum_payroll_flow18),
            cum_payroll_flow40 = sum(cum_payroll_flow40)
            ) %>%
  mutate(cum_mean_wagerate12  = cum_payroll_flow12/ cum_hours_flow12,
         cum_mean_wagerate18  = cum_payroll_flow18/ cum_hours_flow18,
         cum_mean_wagerate40  = cum_payroll_flow40/ cum_hours_flow40)

write.csv(table5, file = paste0(path.to.save,"table_5_sum_stat.csv"))
