#!/usr/bin/env

#= THIS SCRIPT TRANSFORMS COLUMN-MAJOR ORDER DATA TO ROW-MAJOR ORDER DATA =#
#In the Unix command line, the format for entering data is:
## $Rscript test.R [filename] [# of rows] [# of cols]
#where
## filename = tsv file with column-major data
## # of rows = number of alphabetical rows (up to 702, where rows after Z are named AA, AB...ZY, ZZ)
## # of cols = number of numerical columns
#if the number of rows AND columns is not specified, program defaults to 8x12

#expected output is a tsv with row-major order data

#Load libraries--------------
#if libraries do not load, uncomment next line:
##install.packages(c("reshape2", "data.table", "dplyr"))

library(reshape2)
library(data.table)
library(dplyr)

print ("Libraries loaded")

#Step 1: Reading in data------------
args=commandArgs(trailingOnly=TRUE)

df <- read.table(args[1], header=F)

if(is.na(args[2]) & is.na(args[3])) {
	row_dim <- 8
	col_dim <- 12
} else if (is.na(args[2]) | is.na(args[3])) {
	print ("ERROR: MISSING ROW OR COLUMN DIMENSIONS")
} else if (!is.na(args[2]) & !is.na(args[3])) {
	row_dim <- as.numeric(args[2])
	col_dim <- as.numeric(args[3])
}

print ("Finished Step 1")

#Step 2: Generating new row names and col numbers---------------
letters702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS))) #makes a list of letters, including AA, AB, etc.
letter_list <- as.data.frame(letters702)

df_rowlet <- data.frame(lett=letter_list[1:row_dim,])
my_list = list()
number_list <- c()
my_num <- 1:col_dim
for(i in my_num) {
	df_colnum <- as.data.frame(i)
	df2 <- as.data.table(df_colnum[rep(seq_len(nrow(df_colnum)), each=length(df_rowlet)),])
	number_list <- cbind(df_rowlet, df2)
	my_list[[i]] <- number_list
}

df3 <- dplyr::bind_rows(my_list)
df4 <- df3[order(df3$lett),]
df4$lett <- as.character(df4[,1])
df4$test <- nchar(df4$lett)
df5 <- df4[order(df4$test),]
df6 <- as.data.table(paste(df5$lett, df5$V1, sep=""))

print ("Finished Step 2")

#Step 3: Cleaning up wells-----------
df_a <- colsplit(df$V1, "(?<=\\p{L})(?=[\\d+$])", c("char", "digit")) #if the script fails here, try quitting R and starting again
df_b <- cbind(df, df_a) #always put input tsv first or it'll overwrite data
df_c <- df_b[order(df_b$digit),]
df_d <- within(df_c, rm("V1","char","digit"))

df_6d <- cbind(df6, df_d)

write.table(df_6d, "flipped_out.tsv", row.names=F, col.names=F, quote=F, sep="\t")

print ("Finished Step 3")
print ("Your table is ready")

q()