# 4. This step selects all files under the directory "Data", including files in subdirectories and assigns them to a variable. 
csv_files <- list.files(path = "./Data", pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# 5. This step notes how many files "csv_files" includes
length(csv_files)

# 6. This step listed the data in "wingspan_vs_mass.csv", then saved it as a R object.
df <- read.csv("./Data/wingspan_vs_mass.csv")

# 7. This step displays the first 5 rows of object "df". 
head(df, n = 5)

# 8. This step locates all files that start with "b", as specified by "^".
list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE)

# 9. This step is a for-loop that runs the first line in each of 3 different file types that start with the letter "b". 
for(a in 1:3){
  b_files <- list.files(path = "Data/", pattern = "^b", full.names = TRUE, recursive = TRUE)
  b_files <- readLines(con = b_files[a], n = 1)
  print(head(b_files, 1)) 
}

# 10. This step is a for-loop that prints the first row of each .csv file under the object "csv_files".
for(b in 1:140){
  rcsv <- read_csv(file = csv_files[b])
  print(head(rcsv, 1))
}
