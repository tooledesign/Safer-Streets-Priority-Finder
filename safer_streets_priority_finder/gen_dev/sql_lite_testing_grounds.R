library(DBI)
library(RSQLite)

# Create DB/connect
con <- dbConnect(RSQLite::SQLite(), "login.sqlite")

# create table 
dbGetQuery(con, sprintf('DROP TABLE IF EXISTS login_info;'))
dbGetQuery(con, "CREATE TABLE login_info (username TEXT, password TEXT, user_id INTEGER PRIMARY KEY);")
dbListTables(con)
print(dbGetQuery(con, sprintf('SELECT * FROM login_info')))

un = 'gis'
pw = 'gis'
u_id = 11613 

# test for username, should not be there 
test <- dbGetQuery(con, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\'', un))
if (length(test[,1]) == 0){
  print('Username not found in DB.')
} else {
  print('Username in DB.')
}

# create user and test for user name 
dbGetQuery(con, sprintf("INSERT INTO login_info ( username, password, user_id) VALUES (\'%s\', \'%s\', %s);", un, pw, user_id))
test <- dbGetQuery(con, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\'', un))
if (length(test[,1]) == 0){
  print('Username not found in DB.')
} else {
  print('Username in DB.')
}


# try to add duplicate
# create function for testing
add_user <- function(connection, username, password, user_id){
if (length(dbGetQuery(connection, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\' OR user_id = \'%s\'', username, user_id))[,1]) >= 1){
  print('This username already exists, please choose something else.')
} else {
  dbGetQuery(connection, sprintf("INSERT INTO login_info ( username, password, user_id) VALUES (\'%s\', \'%s\', %s);", username, password, user_id))
  print('user added')
}
}

# attempt user 2, situation a second user tries the same UN
add_user(connection=con, username=un, password='gis', user_id=u_id)

# attempt user 2, situation a second user with same user name (this would only happen if two user logged in at the exact same time... i.e. not very likely)
add_user(connection=con, username='gis2', password=pw, user_id=u_id)

# add user 2 with same pw as user 1 
add_user(connection=con, username='gis2', password=pw, user_id=11614)


# call foreign key  
# test
print(dbGetQuery(con, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\' AND password = \'%s\'', 'gis', 'gis'))[,1])

# call foreign key under incorrect circumstances 
get_user_id <- function(connection, username, password){
if (length(dbGetQuery(connection, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\'', username))[,1]) == 0){
  print('That username does not exist.')
} else if (length(dbGetQuery(connection, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\' AND password = \'%s\'', username, password))[,1]) == 0){
  print('That username does not exist or the password is incorrect.')
} else {
  test <- dbGetQuery(connection, sprintf('SELECT user_id FROM login_info WHERE username = \'%s\' AND password = \'%s\'', username, password))[,1]
  return(test)
}
}

# test with active un/pw
get_user_id(connection=con, username='gis', password='gis')

# test with incorrect UN 
get_user_id(connection=con, username='giss', password='gis')

# test in incorrect PW 
get_user_id(connection=con, username='gis', password='giss')

# test with mismatched US/passwords
print(dbGetQuery(con, sprintf('SELECT * FROM login_info')))
add_user(connection=con, username='gis3', password='gis3', user_id=11615)
get_user_id(connection=con, username='gis', password='gis3')

# potential TODOs 
# check to make sure person is not a robot 
# tie username to email and verify with server sending email to user 
# add questions for un/pw verification OR password update 














