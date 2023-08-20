library(tidyverse)

cc_state_sector_grads <- read_csv("cc_state_sector_grads.csv")



sgrad <- cc_state_sector_grads %>%
  select(state_abbr, gender, year, grad_cohort) %>%
  group_by(state_abbr, gender)
    summarise (totalgrads = sum(grad_cohort))
  
sapply(sgrad, class)
sgrad$grad_cohort <- as.integer(sgrad$grad_cohort)


sgrad <- sgrad %>%
  group_by(state_abbr, gender, year) %>%
  summarise(totalgrads = sum(grad_cohort))






ALTER TABLE `state graduates`
MODIFY COLUMN grad_cohort INTEGER;

SELECT state_abbr, year, gender, SUM(grad_cohort) OVER (PARTITION BY state_abbr, year, gender)
FROM `state graduates`
WHERE year = 2013
GROUP BY state_abbr, year, gender
ORDER BY state_abbr, year, gender DESC;


SELECT state_abbr, year, gender, SUM(grad_cohort) OVER (PARTITION BY state_abbr, year, gender) AS total_grad_cohort
FROM `state graduates`
WHERE year = 2013
GROUP BY state_abbr, year, gender
ORDER BY state_abbr, year, gender DESC;
