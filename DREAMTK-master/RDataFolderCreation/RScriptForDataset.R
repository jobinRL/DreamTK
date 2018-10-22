#PRECONDITIONS, you have ran the SQL Script in the same folder and you have the model data in the R memory.
#we start by loading in the chemicals.
library(data.table)  # Required to use tcpl
library(tcpl)        # Required to determine cytotoxicity
library(RMySQL) # Required to connect to MySQL database
library(dplyr)

# Connection to the MySQL database for tcpl
tcplConf(drvr="MySQL", user="root", pass="CTLab", host="localhost", db="INVITRODB_V2")

# Connecting to the MySQL database for data extraction
invitro_connect = dbConnect(MySQL(), user="root", password="CTLab", dbname="INVITRODB_V2")

# Extracting CASRN, chemical ID, and chemical name from INVITRODB_V2
chemical_data = dbGetQuery(invitro_connect, "SELECT casn, chid, chnm FROM chemical")

# Calculating cytotoxicity for given chemical IDs
chem_cytox=tcplCytoPt(chid=chemical_data$chid)

# Removing unneeded columns from chem_cytox
chem_cytox=chem_cytox[,c("casn","cyto_pt_um")]
#kinda useless but I really only need those 2 calues under specific names.
chemical_data = dbGetQuery(invitro_connect,"SELECT casn, chnm as name FROM chemical")
# Merge cytotoxicity data with chemical data
chemical_data = merge(x=chemical_data,y=chem_cytox,by="casn",all=TRUE)



# Set flag for toxicity
chemical_data$cytotoxic=ifelse(100<chemical_data$cyto_pt_um,"N","Y")
#just changing the name of a column casually like that
names(chemical_data)[names(chemical_data) == 'cyto_pt_um'] <- 'cytotoxicity_um';


dbDisconnect(invitro_connect)

#chemicals done

#now we load the files we created with the sql script
#file paths are hard coded oops.
#gotta make sure that "" is North America. (N/A values)
ac50_data = read.csv("C:/ProgramData/MySQL/MySQL Server 5.6/ac50_data.csv",header = TRUE, sep = ",", na.strings = "");
assay_data = read.csv("C:/ProgramData/MySQL/MySQL Server 5.6/assay_data.csv",header = TRUE, sep = ";", na.strings = "");

#this will save the dataset onto the base R folder where you wil have to paste it where you need it .
save(ac50_data,assay_data,chemical_data,kow_data,model_3css_data,physiology_data,pka_data, compress = TRUE, compression_level = 9, file = "DreamTKv0.7.RData")


#}