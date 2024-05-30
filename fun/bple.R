# set directory
setwd("C:\\Users\\Maria Laura Miranda\\OneDrive\\Área de Trabalho\\UFMG\\Dissertação\\Data\\Thesis\\E0per")

# load packages
require(dplyr)
require(rstudioapi)

# read all files from HMD
filelist = list.files(pattern = ".*.txt")
datalist = lapply(filelist, function(x)read.table(x, skip=2, header=T)) 
datafr = do.call("rbind", datalist) 

# adding the name of the country
datafr$country <- c(rep("AUS", 99), rep("AUT", 73), rep("BEL", 180), 
                    rep("BGR", 74), rep("BLR", 60), rep("CAN", 99), 
                    rep("CHE", 145), rep("CHL", 29), rep("CZE", 70), 
                    rep("DEUTE", 65), rep("DEUTNP", 31), rep("DEUTW", 65),
                    rep("DNK", 187), rep("ESP", 113), rep("EST", 61),
                    rep("FIN", 144), rep("FRACNP", 204), rep("FRATNP", 204),
                    rep("GBR_NIR", 99), rep("GBR_NP", 99), rep("GBR_SCO", 166),
                    rep("GBRCENW", 180), rep("GBRTENW", 180), rep("GRC", 39),
                    rep("HKG", 34), rep("HRV", 20), rep("HUN", 71),
                    rep("IRL", 71), rep("ISL", 183), rep("ISR", 34),
                    rep("ITA", 148), rep("JPN", 74), rep("KOR", 16),
                    rep("LTU", 62), rep("LUX", 61), rep("LVA", 61),
                    rep("NLD", 170), rep("NOR", 175), rep("NZL_MA", 61),
                    rep("NZL_NM", 108), rep("NZL_NP", 74), rep("POL", 62),
                    rep("PRT", 81), rep("RUS", 56), rep("SVK", 70),
                    rep("SVN", 37), rep("SWE", 271), rep("TWN", 50),
                    rep("UKR", 55), rep("USA", 88))

datafr <- datafr %>% # removing NAs which are in the form "."
  filter(Male != "." | Female != ".")

datafr$Female <- as.numeric(sub(",",".",datafr$Female)) # changing as numeric
datafr$Male <- as.numeric(sub(",",".",datafr$Male))

# getting the max values in each year
bple <- 
  datafr %>% 
  filter(country != "BLR") %>% 
  group_by(Year) %>% 
  summarise(e0_Masc = max(Male, na.rm = TRUE), 
            e0_Fem = max(Female, na.rm = TRUE), 
            c_Masc = country[which(max(Male, na.rm = TRUE) == Male)][1], 
            c_Fem = country[which(max(Female, na.rm = TRUE) == Female)][1])


# saving in a file
setwd(dirname(getActiveDocumentContext()$path))
write.csv(bple, "bple.csv")
