#1.杞藉叆R鍖?
library(survival)
library(plyr)
#2.娓呯悊宸ヤ綔鐜
gc()
#3.璇诲叆鏁版嵁
col.idx <- c("Gender","Age","APM.score","TIS.score","IIS.score","TMB","indelTMB","TNM.stage","Perineural.invasion","Necrosis","Margin",
             "Postoperative.radiotherapy","Histopathology","MYB.Fusion","Locoregional.recurrence","LRRFS","Distant.metastasis","DMFS")
df.clin <- acc.75.clinical[,col.idx]
colnames(df.clin) <- c("Gender","Age","APM","TIS","IIS","TMB","indelTMB","TNM.stage","PNI","Necrosis","Margin",
                       "PORT","Solid.subtype","MYB.Fusion","LRRFS.status","LRRFS","DMFS.status","DMFS")
df.clin <- df.clin[rownames(subset(acc.75.clinical,Recurrence.Tissue == "no")),]

df.clin$PORT <- sub("no ","no",sub("yes, radiotherapy and chemotherapy|yes, heavy ion radiotherapy|yes, concurrent chemoradiotherapy|yes ","yes",df.clin$PORT))
df.clin$indelTMB <- as.numeric(as.character(df.clin$indelTMB))
df.clin$TMB <- as.numeric(as.character(df.clin$TMB))
df.clin$TNM.stage[grep("T3|T4", df.clin$TNM.stage)] <- "III-IV"
df.clin$TNM.stage[grep("T1|T2", df.clin$TNM.stage)] <- "I-II"
df.clin$TNM.stage[which(df.clin$TNM.stage=="")] <- "unknown"
df.clin$Gender <- factor(df.clin$Gender, levels = c("Female","Male"))
df.clin$Age <- factor(ifelse(df.clin$Age >= 45,">=45","<45"), levels = c("<45",">=45"))
df.clin$APM <- factor(ifelse(df.clin$APM >= median(df.clin$APM),"high","low"), levels = c("low","high"))
df.clin$TIS <- factor(ifelse(df.clin$TIS >= median(df.clin$TIS),"high","low"), levels = c("low","high"))
df.clin$IIS <- factor(ifelse(df.clin$IIS >= median(df.clin$IIS),"high","low"), levels = c("low","high"))
df.clin$TMB <- factor(ifelse(df.clin$TMB >= median(na.omit(df.clin$TMB)),"high","low"), levels = c("low","high"))
df.clin$indelTMB <- factor(ifelse(df.clin$indelTMB >= median(na.omit(df.clin$indelTMB)),"high","low"), levels = c("low","high"))
df.clin$TNM.stage <- factor(df.clin$TNM.stage, levels = c("I-II","III-IV"))
df.clin$PNI <- factor(df.clin$PNI, levels = c("negative","positive"))
df.clin$Necrosis <-  factor(df.clin$Necrosis, levels = c("negative","positive"))
df.clin$Margin <- factor(df.clin$Margin, levels = c("negative","positive"))
df.clin$PORT <- factor(df.clin$PORT, levels = c("no","yes"))
df.clin$Solid.subtype <- factor(ifelse(df.clin$Solid.subtype == "solid","yes","no"), levels = c("no","yes"))
df.clin$MYB.Fusion <- factor(df.clin$MYB.Fusion , levels = c("no","yes"))



#4.鏌ョ湅鏁版嵁鏁版嵁鎬ц川
str(df.clin)
#5.鏌ョ湅缁撳眬锛?0=澶嶅彂锛?1鏈鍙?
df.clin$LRRFS.status<-factor(df.clin$LRRFS.status)
summary(df.clin$LRRFS.status)
df.clin$DMFS.status<-factor(df.clin$DMFS.status)
summary(df.clin$DMFS.status)


#1.鏋勫缓妯″瀷鐨剏锛屽彧闇€淇敼杩欎竴琛屼唬鐮佸拰绗?3-(2)姝ヤ唬鐮?
y <- Surv(time=df.clin$DMFS,event=df.clin$DMFS.status==1)#0涓哄鍙?

#2.鎵归噺鍗曞洜绱犲洖褰掓ā鍨嬪缓绔嬶細Uni_cox_model
Uni_cox_model<- function(x){
  FML <- as.formula(paste0 ("y~",x))
  cox<- coxph(FML,data=df.clin)
  cox1<-summary(cox)
  HR <- round(cox1$coefficients[,2],2)
  PValue <- round(cox1$coefficients[,5],3)
  CI5 <-round(cox1$conf.int[,3],2)
  CI95 <-round(cox1$conf.int[,4],2)
  Uni_cox_model<- data.frame('Characteristics' = x,
                             'HR' = HR,
                             'CI5' = CI5,
                             'CI95' = CI95,
                             'p' = PValue)
  return(Uni_cox_model)}  

#3.灏嗘兂瑕佽繘琛岀殑鍗曞洜绱犲洖褰掑彉閲忚緭鍏ユā鍨?
#3-(1)鏌ョ湅鍙橀噺鐨勫悕瀛楀拰搴忓彿
names(df.clin)

#3-(2)杈撳叆鍙橀噺搴忓彿
variable.names<- colnames(df.clin)[c(1:14)] #渚嬶細杩欓噷閫夋嫨浜?3-10鍙峰彉閲?

#4.杈撳嚭缁撴灉
Uni_cox <- lapply(variable.names, Uni_cox_model)
Uni_cox<- ldply(Uni_cox,data.frame)

#5.浼樺寲琛ㄦ牸锛岃繖閲屼妇渚婬R+95% CI+P 椋庢牸
Uni_cox$CI<-paste(Uni_cox$CI5,'-',Uni_cox$CI95)
Uni_cox<-Uni_cox[,-3:-4]

#鏌ョ湅鍗曞洜绱燾ox琛ㄦ牸
Uni_cox

write.csv(Uni_cox, file = "./output/primary.dmfs.uni.cox.res.csv")



#1.鎻愬彇鍗曞洜绱爌<0.05鍙橀噺
Uni_cox$Characteristics[Uni_cox$p<0.05]

#2.澶氬洜绱犳ā鍨嬪缓绔?
mul_cox_model<- as.formula(paste0 ("y~",paste0(na.omit(Uni_cox$Characteristics[Uni_cox$p<0.05]),collapse = "+")))
mul_cox<-coxph(mul_cox_model,data=df.clin)
cox4<-summary(mul_cox) 

#3.鎻愬彇澶氬洜绱犲洖褰掔殑淇℃伅
mul_HR<- round(cox4$coefficients[,2],2) 
mul_PValue<- round(cox4$coefficients[,5],4) 
mul_CI1<-round(cox4$conf.int[,3],2)
mul_CI2<-round(cox4$conf.int[,4],2)

#4.澶氬洜绱犵粨鏋滀紭鍖栧苟鎴愯〃锛歮ul_cox1
mul_CI<-paste(mul_CI1,'-',mul_CI2)
mul_cox1<- data.frame("HR"=mul_HR,"CI"=mul_CI, "P"=mul_PValue)

write.csv(mul_cox1, file = "./output/primary.dmfs.allmul.cox.res.csv")
