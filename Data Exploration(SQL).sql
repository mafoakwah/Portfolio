--preview conn=DBI::dbConnect(RSQLite::SQLite())
library(RMySQL)


SELECT state_abbr, year, control, cohort, SUM(grad_cohort) AS TotalGrads
FROM `state graduates` 
WHERE year > 2008
GROUP BY state_abbr, year, cohort
ORDER BY state_abbr, year, cohort DESC

SELECT state_abbr, gender, year, SUM(grad_cohort) AS TotalGradbygender
FROM `state graduates`
WHERE gender != 'B' AND gender !='g' AND year > 2008
GROUP BY state_abbr, gender, year
ORDER BY state_abbr, gender, year;


CREATE VIEW GradsByRace AS
SELECT race, year, state_abbr, SUM(grad_cohort) AS Gradbyrace
FROM `state graduates`
WHERE  year = 2013
GROUP BY state_abbr, race, year 
ORDER BY state_abbr, race, year DESC;

SELECT race, year, state_abbr, SUM(grad_cohort) AS Gradbyrace
FROM `state graduates`
WHERE  year = 2013
GROUP BY state_abbr, race, year 
ORDER BY state_abbr, race, year DESC

SELECT 