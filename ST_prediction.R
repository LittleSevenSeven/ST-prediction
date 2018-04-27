#财务报表分析与ST预测
data = read.csv('/Users/ycy/Desktop/财务报表分析与ST预测.csv', header = T, sep = ',')
data
data[c(1:10),] #展示数据
#——————————————描述性分析——————————————————
summary(data) #描述统计量
n = dim(data)[1];n #数据量

N=sapply(data,length)											#描述性分析：样本量
MEAN=sapply(data,mean)											#描述性分析：均值
SD=sapply(data,sd)												#描述性分析：标准差
MIN=sapply(data,min)											#描述性分析：最小值
MED=sapply(data,median)											#描述性分析：中位数
MAX=sapply(data,max)											#描述性分析：最大值
result=cbind(N,MEAN,SD,MIN,MED,MAX)									#描述性分析：整合结果
result													#描述性分析：展示

data_order= data[order(data$ARA),] #按ARA从小到大的顺序排序

#------------各个解释变量按照ST状态分组做箱线图对比-----------
#table(data$ST)
100*table(data$ST)/n #因变量ST的分布
#饼状图？

boxplot(ARA~ST,xlab="ST Status",ylab="ARA",data=data)
boxplot(ASSET~ST,xlab="ST Status",ylab="ASSET",data=data)
boxplot(ATO~ST,xlab="ST Status",ylab="ATO",data=data)
boxplot(GROWTH~ST,xlab="ST Status",ylab="GROWTH",data=data)
boxplot(LEV~ST,xlab="ST Status",ylab="LEV",data=data)
boxplot(ROA~ST,xlab="ST Status",ylab="ROA",data=data)
boxplot(SHARE~ST,xlab="ST Status",ylab="SHARE",data=data)


#--------------建模-----------------
# 0-1回归模型glm.full
glm.full = glm(ST ~ ARA + ASSET + ATO + ROA + GROWTH + LEV + SHARE, 
                family=binomial(link="logit"),data = data)
glm.full
summary(glm.full)
# 空模型，不包含任何变量
glm.null = glm(ST~1,family=binomial(link="logit"),data=data)
glm.null
#全模型的显著性检验
#anova(glm.full,test = 'Chisq')
#对模型做出预测结果
pre_full=predict(glm.full,type='response')
#将预测概率pre和实际结果放在一个数据框中
#预测概率命名为prob,实际结果命名为obs
contrast_full=data.frame(prob=pre_full,obs=data$ST)
#将预测概率按照从低到高排序
#data2=data1[order(data1$prob),]

## AIC准则和BIC准则模型显著性检验和估计结果
glm.aic = step(glm.full,trace=F)
summary(glm.aic) #模型选择：AIC
glm.bic = step(glm.full,k=log(n),trace=F)
summary(glm.bic) #模型选择：BIC


#-----------------标准化之后建模------------------
#ARA_std = scale(data$ARA)
#ASSET_std = scale(data$ASSET)
#ATO_std = scale(data$ATO)
#ROA_std = scale(data$ROA)
#GROWTH_std = scale(data$GROWTH)
#LEV_std = scale(data$LEV)
#SHARE_std = scale(data$SHARE)
#ST = data$ST
#data_std = data.frame(ARA_std,ASSET_std,ATO_std,ROA_std,GROWTH_std,LEV_std,SHARE_std,ST)
#glm_std <- glm(ST ~ ARA_std + ASSET_std + ATO_std + ROA_std + GROWTH_std + LEV_std + SHARE_std, 
#               family=binomial(link="logit"),data = data_std)
#glm_std
#summary(glm_std)

#-----------------画ROC曲线----------------
#画三个模型的ROC曲线（用ROCR包）
library(ROCR)

pre_full=predict(glm.full,type='response') #上面已写
pre_aic=predict(glm.aic,type='response')
pre_bic=predict(glm.bic,type='response')
pred_full = prediction(pre_full,data$ST)
pred_aic = prediction(pre_aic,data$ST)
pred_bic = prediction(pre_bic,data$ST)
performance(pred_full,'auc')@y.values #AUC值
performance(pred_aic,'auc')@y.values
performance(pred_bic,'auc')@y.values
perf_full = performance(pred_full,'tpr','fpr')
perf_aic = performance(pred_aic,'tpr','fpr')
perf_bic = performance(pred_bic,'tpr','fpr')
par(mfrow=c(1,1))
plot(perf_full,main='The ROC curves of the three models')
plot(perf_aic,add=T,col='red')
plot(perf_bic,add=T,col='blue')
legend(0.7,0.3,c("Full","AIC","BIC"),col=c("black","red","blue"),lty=1:3,lwd=3)

#画三个模型的ROC曲线（用pROC包）
#ROCR包画图函数功能比较单一，笔者比较偏好使用功能更强大的pROC包。
#它可以方便比较两个分类器，还能自动标注出最优的临界点，图看起来也比较漂亮。
library(pROC) #画ROC曲线

roc_full=roc(data$ST,pre_full)
roc_aic=roc(data$ST,pre_aic)
roc_bic=roc(data$ST,pre_bic)

#par(mfrow=c(1,1))
#plot(roc_full,print.auc=T,print.auc.x=0.4,print.auc.y=0.4,lty=1,main="The ROC curves of the three models",lwd=3)			
#plot(roc_aic,print.auc=T,print.auc.x=0.5,print.auc.y=0.5,add=T,col="red",lty=2,lwd=3)
#plot(roc_bic,add=T,col="blue",lty=3,lwd=3)
#legend(0.3,0.3,c("Full","AIC","BIC"),col=c("black","red","blue"),lty=1:3,lwd=3)
#temp = locator(1) # 在图表上，你喜欢的地方点击一下，文字就出来了
#text(temp,"AIC",col = 'red')
#temp = locator(1) # 在图表上，你喜欢的地方点击一下，文字就出来了
#text(temp,"Full")

# 最终选择AIC模型，在ROC曲线上标注AUC值和最佳阈值
par(mfrow=c(1,1))
plot(roc_aic,print.auc=T,print.auc.x=0.4,print.auc.y=0.4,print.thres=T,
     print.auc.cex=1.5,print.thres.cex=1.5,main="The ROC curve of AIC model")

#----------------混淆矩阵-----------------
library(plyr) #count函数
contrast_aic=data.frame(prob=pre_aic,obs=data$ST)
# 阈值为50%
thres1 = 0.5							 
table1 = table(data$ST,1*(contrast_aic$prob>thres1))
# 阈值为5.26%
thres2 = 0.0526
table2 = table(data$ST,1*(contrast_aic$prob>thres2))
table2_to_frame = as.data.frame(table2)
TN_2 = table2_to_frame[1,'Freq']
FN_2 = table2_to_frame[2,'Freq']
FP_2 = table2_to_frame[3,'Freq']
TP_2 = table2_to_frame[4,'Freq']
TN_2; FN_2; FP_2; TP_2
MCR_2 = (FP_2+FN_2)/n
TPR_2 = TP_2/(TP_2+FN_2)
FPR_2 = FP_2/(FP_2+TN_2)
MCR_2; TPR_2; FPR_2
#阈值为5.5%
thres3 = 0.043  #最佳阈值
table3 = table(data$ST,1*(contrast_aic$prob>thres3))
table3_to_frame = as.data.frame(table3)
TN_3 = table3_to_frame[1,'Freq']
FN_3 = table3_to_frame[2,'Freq']
FP_3 = table3_to_frame[3,'Freq']
TP_3 = table3_to_frame[4,'Freq']
TN_3; FN_3; FP_3; TP_3
MCR_3 = (FP_3+FN_3)/n
TPR_3 = TP_3/(TP_3+FN_3)
FPR_3 = FP_3/(FP_3+TN_3)
MCR_3; TPR_3; FPR_3