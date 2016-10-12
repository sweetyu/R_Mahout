#建立数据模型M
FileDataModel<-function(file){
	data<-read.csv(file,header=FALSE)	#读取csv文件
	names(data)<-c("uid","iid","pref")	#增加列名

	user <- unique(data$uid)			#用户数组
	item <- unique(sort(data$iid))		#产品数组

	M <- matrix(0, length(user), length(item))
	uidx <- match(data$uid, user)		#用户序列
	iidx <- match(data$iid, item)		#产品序列
	i <- cbind(uidx, iidx, pref=data$pref)		#原始数据矩阵
	for(n in 1:nrow(i)){
		M[i[n,][1],i[n,][2]]<-i[n,][3]			
	}
	dimnames(M)[[2]]<-item
	print("M：");print(M)
	M						#返回数据模型矩阵
}

#计算用户相似度S	(欧氏距离)
EuclideanDistanceSimilarity<-function(M){
	row<-nrow(M)
	S<-matrix(0, row, row)
	for(z1 in 1:row){
	 for(z2 in 1:row){
	   if(z1<z2){			#下三角
		 num<-intersect(which(M[z1,]!=0),which(M[z2,]!=0)) #可计算的列
		 sum<-0								#用户z1与用户z2的相似度
		 for(z3 in num){
			sum<-sum+(M[z1,][z3]-M[z2,][z3])^2
		 }
		 S[z2,z1]<-length(num)/(1+sqrt(sum))

		 if(S[z2,z1]>1) S[z2,z1]<-1 		#对算法的阈值进行限制
		 if(S[z2,z1]< -1) S[z2,z1]<- -1 	
	   }
	  }	
	}
	ts<-t(S)					#补全三角矩阵
	w<-which(upper.tri(ts))
	S[w]<-ts[w]
	print("S：");print(S)
	S
}

#用戶最近邻N
NearestNUserNeighborhood<-function(S,n){
	row<-nrow(S)
	N<-matrix(0, row, n)		#只保存n个最近邻
	for(z1 in 1:row){
	  for(z2 in 1:n){
		m<-which.max(S[,z1])
		N[z1,][z2]<-m
		S[,z1][m]=0
	   }
	}
	print("N：");print(N)
	N
}

#推荐算法R
UserBasedRecommender<-function(uid,n,M,S,N){
	row<-ncol(N)		#row个最近邻
	col<-ncol(M)		#col个产品
	r<-matrix(0, row, col)
	N1<-N[uid,]
	for(z1 in 1:length(N1)){
	   num<-intersect(which(M[uid,]==0),which(M[N1[z1],]!=0)) #可计算的列
	   for(z2 in num){
		  r[z1,z2] = M[N1[z1],z2]*S[uid,N1[z1]]
	   }
	}
	print("R：");print(r)		#打印每个用户的推荐矩阵

	sum<-colSums(r)
	s2<-matrix(0, 2, col)
	for(z1 in 1:length(N1)){
	   num<-intersect(which(colSums(r)!=0),which(M[N1[z1],]!=0))
	   for(z2 in num){
		s2[1,][z2]<-s2[1,][z2]+S[uid,N1[z1]]
		s2[2,][z2]<-s2[2,][z2]+1
	   }
	}

	s2[,which(s2[2,]==1)]=10000
	s2<-s2[-2,]

	r2<-matrix(0, n, 2)		
	rr<-sum/s2
	item <-dimnames(M)[[2]]
	for(z1 in 1:n){
       w<-which.max(rr)
	   if(rr[w]>0.5){
		r2[z1,1]<-item[w]
		r2[z1,2]<-as.double(rr[w])
		rr[w]=0
	   }
	}
	print(paste(uid,"R2："));print(r2)
	r2
}
#运行程序
FILE<-"E:\\R\\Mahout\\testCF.csv"	#原始数据文件
NEIGHBORHOOD_NUM<-2			#最近邻个数
RECOMMENDER_NUM<-3			#最多推荐结果

M<-FileDataModel(FILE)
S<-EuclideanDistanceSimilarity(M)
N<-NearestNUserNeighborhood(S,NEIGHBORHOOD_NUM)
for(i in 1:nrow(M)){
	UserBasedRecommender(i,RECOMMENDER_NUM,M,S,N);
}