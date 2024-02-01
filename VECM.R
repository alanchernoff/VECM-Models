library(readxl)
library(xts)
library(zoo)
library(dynlm)

setwd("C:/Users/acher/Downloads")

windy = "1986-01/2022-12"
{

df <- read_excel('bordoduca 2024 Jan forecast Data set.xlsx', col_names = TRUE)
df$DBEAR_D_D2008Q4 <- (df$DBEAR + df$d_D2008Q4)
df$Q <- as.yearqtr(df$Q, "%Y Q%q")
df <- as.xts(df)


#lag(df$LV3124)
#data=window(df, start = 1986, end = 2022)
#View(data)
df$DLV3124_l1 = lag(df$DLV3124,1)
df$DLV3124_l2 = lag(df$DLV3124,2)
df$DLV3124_l3 = lag(df$DLV3124,3)
df$DLV3124_l4 = lag(df$DLV3124,4)
df$DLV3124_l5 = lag(df$DLV3124,5)

df$DLV4T124_l1 = lag(df$DLV4T124,1)
df$DLV4T124_l2 = lag(df$DLV4T124,2)
df$DLV4T124_l3 = lag(df$DLV4T124,3)
df$DLV4T124_l4 = lag(df$DLV4T124,4)
df$DLV4T124_l5 = lag(df$DLV4T124,5)

df$DLV4X124_l1 = lag(df$DLV4X124,1)
df$DLV4X124_l2 = lag(df$DLV4X124,2)
df$DLV4X124_l3 = lag(df$DLV4X124,3)
df$DLV4X124_l4 = lag(df$DLV4X124,4)
df$DLV4X124_l5 = lag(df$DLV4X124,5)

df$DLSLD1X124_l1 = lag(df$DLSLD1X124,1)
df$DLSLD1X124_l2 = lag(df$DLSLD1X124,2)
df$DLSLD1X124_l3 = lag(df$DLSLD1X124,3)
df$DLSLD1X124_l4 = lag(df$DLSLD1X124,4)
df$DLSLD1X124_l5 = lag(df$DLSLD1X124,5)

df$DCFMA_l1 = lag(df$DCFMA,1)
df$DCFMA_l2 = lag(df$DCFMA,2)
df$DCFMA_l3 = lag(df$DCFMA,3)
df$DCFMA_l4 = lag(df$DCFMA,4)

df$DGSTRVAXLAG124_l1 = lag(df$DGSTRVAXLAG124,1)
df$DGSTRVAXLAG124_l2 = lag(df$DGSTRVAXLAG124,2)
df$DGSTRVAXLAG124_l3 = lag(df$DGSTRVAXLAG124,3)
df$DGSTRVAXLAG124_l4 = lag(df$DGSTRVAXLAG124,4)


df$DBaaTR_l1 = lag(df$DBaaTR,1)
df$DBaaTR_l2 = lag(df$DBaaTR,2)
df$DBaaTR_l3 = lag(df$DBaaTR,3)
df$DBaaTR_l4 = lag(df$DBaaTR,4)

df$DVAXFULL124_l1 = lag(df$DVAXFULL124,1)
df$DVAXFULL124_l2 = lag(df$DVAXFULL124,2)

df$LV3124_l1 = lag(df$LV3124,1)
df$LV4T124_l1 = lag(df$LV4T124,1)
df$LV4X124_l1 = lag(df$LV4X124,1)

df$LSLD1X124_l1 = lag(df$LSLD1X124,1)

df$DFDICPREM13_l1 = lag(df$DFDICPREM13,1)


#View(df)

model1 <-lm(DLV3124~ LV3124_l1+LSLD1X124_l1+CFMA+GSTRVAXLAG124+
              DLV3124_l1+DLV3124_l2+DLV3124_l3+DLV3124_l4+DLV3124_l5+
              DLSLD1X124_l1+DLSLD1X124_l2+DLSLD1X124_l3+DLSLD1X124_l4+DLSLD1X124_l5+
              DCFMA+DCFMA_l1+DCFMA_l2+DCFMA_l3+DCFMA_l4+
              DGSTRVAXLAG124+DGSTRVAXLAG124_l1+DGSTRVAXLAG124_l2+DGSTRVAXLAG124_l3+DGSTRVAXLAG124_l4+
              DBaaTR_l1+DBaaTR_l2+DBaaTR_l3+DBaaTR_l4+
              lrefi124+DBEAR_D_D2008Q4+D2020Q2+
              DVAXFULL124+DVAXFULL124_l1+DVAXFULL124_l2+
              DFDICPREM13_l1+DY2K1+DUMNEFREEZE, data=window(df[windy]))
summary(model1)


model2 <-lm(DLV4T124~ LV4T124_l1+LSLD1X124_l1+CFMA+GSTRVAXLAG124+
              DLV4T124_l1+DLV4T124_l2+DLV4T124_l3+DLV4T124_l4+DLV4T124_l5+
              DLSLD1X124_l1+DLSLD1X124_l2+DLSLD1X124_l3+DLSLD1X124_l4+DLSLD1X124_l5+
              DCFMA+DCFMA_l1+DCFMA_l2+DCFMA_l3+DCFMA_l4+
              DGSTRVAXLAG124+DGSTRVAXLAG124_l1+DGSTRVAXLAG124_l2+DGSTRVAXLAG124_l3+DGSTRVAXLAG124_l4+
              DBaaTR_l1+DBaaTR_l2+DBaaTR_l3+DBaaTR_l4+
              lrefi124+DBEAR_D_D2008Q4+D2020Q2+
              DVAXFULL124+DVAXFULL124_l1+DVAXFULL124_l2+
              DFDICPREM13_l1+DY2K1+DUMNEFREEZE, data=window(df[windy]))
summary(model2)

model3 <-lm(DLV4X124 ~ LV4X124_l1+LSLD1X124_l1+CFMA+GSTRVAXLAG124+
              DLV4X124_l1+DLV4X124_l2+DLV4X124_l3+DLV4X124_l4+DLV4X124_l5+
              DLSLD1X124_l1+DLSLD1X124_l2+DLSLD1X124_l3+DLSLD1X124_l4+DLSLD1X124_l5+
              DCFMA+DCFMA_l1+DCFMA_l2+DCFMA_l3+DCFMA_l4+
              DGSTRVAXLAG124+DGSTRVAXLAG124_l1+DGSTRVAXLAG124_l2+DGSTRVAXLAG124_l3+DGSTRVAXLAG124_l4+
              DBaaTR_l1+DBaaTR_l2+DBaaTR_l3+DBaaTR_l4+
              lrefi124+DBEAR_D_D2008Q4+D2020Q2+
              DVAXFULL124+DVAXFULL124_l1+DVAXFULL124_l2+
              DFDICPREM13_l1+DY2K1+DUMNEFREEZE, data=window(df[windy]))
summary(model3)
}

c(predict(model1,df["2023-01"]),predict(model2,df["2023-01"]),predict(model3,df["2023-01"]))
