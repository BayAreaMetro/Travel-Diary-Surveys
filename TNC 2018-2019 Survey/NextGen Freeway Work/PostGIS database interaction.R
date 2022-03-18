
password <- readlines("M:/Data/HomeInterview/TNC Survey/SFCTA Map Matching/Password.txt")
conn <- RPostgreSQL::dbConnect("PostgreSQL", host = "localhost",
                               dbname = "tncdata", user = "postgres", password = "<PASSWORD>")