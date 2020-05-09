SELECT country, state, date, parameter, unit , AVG(value) FROM openaq WHERE date = '2020-04-30';
SELECT * FROM openaq_state WHERE date <= '2020-04-30';
SELECT * FROM openaq LIMIT 10;