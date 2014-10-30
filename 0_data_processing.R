# Change this to your working directory containing the competition data
setwd("C:/Users/jeff.keller/Desktop/RFDilemmaZone")

# Read in the data
data.raw <- read.table("WUA_yldata_for TRB contest Oct 13 2013.csv", sep=",", header = TRUE)
head(data.raw)

# Filter out the trial runs
data <- data.raw[-grep(x = data.raw[, "Run.Name"], pattern = "FAMILIAR"), ]

# Extract participant number
data$ID <- as.numeric(gsub(x = data[, "Subject"], pattern = "\\D", replacement = ""))

# Extract participant age
data$age <- substr(x = data[, "Subject"], start = 9, stop = 9)

# Extract participant gender
data$gender <- substr(x = data[, "Subject"], start = 10, stop = 10)

# Extract drive number
data$drive <- 0
# The drive number for non-starts
e <- substr(x = data[, "Run.Name"], start = 11, stop = 11) == "D"
data[, "drive"][e] <- substr(x = data[, "Run.Name"], start = 12, stop = 12)[e]
# Give restarts a value of 4
data[, "drive"][!e] <- 4

# Extract device type
idx <- e * 13 + (!e) * 14
data$device <- substr(x = data[, "Run.Name"], start = idx, stop = idx + 1)

# Extract treatment number
idx <- e * 17 + (!e) * 18
data$treatment.num <- substr(x = as.character(data[, "Run.Name"]), start = idx, stop = idx)

# Extract treatment order
idx <- e * 18 + (!e) * 19
data$treatment.order <- substr(x = as.character(data[, "Run.Name"]), start = idx, stop = idx + 2)

# Extract the treatment B = 1, I = 2, O = 3
data$treatment <- (data[, "treatment.order"] == "BIO" & data[, "Event.Id"] %in% 300:305) * 1 +
                  (data[, "treatment.order"] == "BIO" & data[, "Event.Id"] %in% 306:311) * 2 +
                  (data[, "treatment.order"] == "BIO" & data[, "Event.Id"] %in% 312:317) * 3 +
                  (data[, "treatment.order"] == "BOI" & data[, "Event.Id"] %in% 300:305) * 1 +
                  (data[, "treatment.order"] == "BOI" & data[, "Event.Id"] %in% 306:311) * 3 +
                  (data[, "treatment.order"] == "BOI" & data[, "Event.Id"] %in% 312:317) * 2 +
                  (data[, "treatment.order"] == "IBO" & data[, "Event.Id"] %in% 300:305) * 2 +
                  (data[, "treatment.order"] == "IBO" & data[, "Event.Id"] %in% 306:311) * 1 +
                  (data[, "treatment.order"] == "IBO" & data[, "Event.Id"] %in% 312:317) * 3 +
                  (data[, "treatment.order"] == "IOB" & data[, "Event.Id"] %in% 300:305) * 2 +
                  (data[, "treatment.order"] == "IOB" & data[, "Event.Id"] %in% 306:311) * 3 +
                  (data[, "treatment.order"] == "IOB" & data[, "Event.Id"] %in% 312:317) * 1 +
                  (data[, "treatment.order"] == "OBI" & data[, "Event.Id"] %in% 300:305) * 3 +
                  (data[, "treatment.order"] == "OBI" & data[, "Event.Id"] %in% 306:311) * 1 +
                  (data[, "treatment.order"] == "OBI" & data[, "Event.Id"] %in% 312:317) * 2 +
                  (data[, "treatment.order"] == "OIB" & data[, "Event.Id"] %in% 300:305) * 3 +
                  (data[, "treatment.order"] == "OIB" & data[, "Event.Id"] %in% 306:311) * 2 +
                  (data[, "treatment.order"] == "OIB" & data[, "Event.Id"] %in% 312:317) * 1

#Remove one participant with a missing event id number
data <- data[data[, "treatment"] %in% 1:3,]

# Rename variables
names(data) <- c("Subject",
                 "Run.Name",
                 "Event1.Time",
                 "Event2.Time",
                 "Event3.Time",
                 "Accel.Time",
                 "Accel.Dir",
                 "Stop.Time",
                 "Stop.Dist",
                 "Accel.Min",
                 "Accel.Max",
                 "Event1.Vel",
                 "Event1.Dist",
                 "Stopline.Vel",
                 "Stopline.Time",
                 "Event2.Vel",
                 "Event.ID",
                 "ID",
                 "Age",
                 "Gender",
                 "Drive",
                 "Device",
                 "Treatment.Num",
                 "Treatment.Order",
                 "Treatment"
                 )

# Set missing values
data[, "Accel.Time"][data[, "Accel.Time"] == "N"] <- NA
data[, "Stop.Time"][data[, "Stop.Time"] == -1] <- NA 
data[, "Stop.Dist"][data[, "Stop.Dist"] == 9999] <- NA
data[, "Event2.Vel"][data[, "Event2.Vel"] == -100000] <- NA

# Set pedal movement
data[, "Accel.Dir"][data[, "Accel.Dir"] == -1] <- "Released"
data[, "Accel.Dir"][data[, "Accel.Dir"] == 1] <- "Depressed"

# Establish data types
data[, "Event1.Time"] <- as.numeric(data[, "Event1.Time"])
data[, "Event2.Time"] <- as.numeric(data[, "Event2.Time"])
data[, "Event3.Time"] <- as.numeric(data[, "Event3.Time"])
data[, "Accel.Time"] <- as.numeric(levels(data[, "Accel.Time"])[as.numeric(data[, "Accel.Time"])])
data[, "Accel.Dir"] <- as.factor(data[, "Accel.Dir"])
data[, "Stop.Time"] <- as.numeric(data[, "Stop.Time"])
data[, "Stop.Dist"] <- as.numeric(data[, "Stop.Dist"])
data[, "Accel.Min"] <- as.numeric(data[, "Accel.Min"])
data[, "Accel.Max"] <- as.numeric(data[, "Accel.Max"])
data[, "Event1.Vel"] <- as.numeric(data[, "Event1.Vel"])
data[, "Event1.Dist"] <- as.numeric(data[, "Event1.Dist"])
data[, "Stopline.Vel"] <- as.numeric(data[, "Stopline.Vel"])
data[, "Stopline.Time"] <- as.numeric(data[, "Stopline.Time"])
data[, "Event2.Vel"] <- as.numeric(data[, "Event2.Vel"])
data[, "Event.ID"] <- as.factor(data[, "Event.ID"])
data[, "ID"] <- as.factor(data[, "ID"])
data[, "Age"] <- as.factor(data[, "Age"])
data[, "Gender"] <- as.factor(data[, "Gender"])
data[, "Drive"] <- as.factor(data[, "Drive"])
data[, "Device"] <- as.factor(data[, "Device"])
data[, "Treatment.Num"] <- as.factor(data[, "Treatment.Num"])
data[, "Treatment.Order"] <- as.factor(data[, "Treatment.Order"])
data[, "Treatment"] <- as.factor(data[, "Treatment"])
levels(data[, "Treatment"]) <- c("B", "I", "O")

# summarize each variable
for (i in c(1:ncol(data))) {
  cat(names(data)[i], "\n\n")
  print(summary(data[,i]))
  cat("-------------------------------------------\n")
}

# set some dirty data to missing
data[, "Accel.Time"][data[, "Accel.Time"] == -1] <- NA
data[, "Accel.Dir"][data[, "Accel.Dir"] == 0] <- NA
data[, "Accel.Dir"] <- factor(data[, "Accel.Dir"])
data[, "Accel.Min"][data[, "Accel.Min"] == 1e+05] <- NA
data[, "Accel.Max"][data[, "Accel.Max"] == -1e+05] <- NA
data[, "Stopline.Vel"][data[, "Stopline.Vel"] == -1e+05] <- NA
data[, "Stopline.Time"][data[, "Stopline.Time"] == -1] <- NA

# Exclude observations missing critical values
data <- data[!(data[, "Event1.Time"] == -1 | data[, "Event2.Time"] == -1), ]

# Save the data set
save(data, file = "driving_data.rdata")
