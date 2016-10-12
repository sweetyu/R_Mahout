#��������ģ��M
FileDataModel<-function(file){
	data<-read.csv(file,header=FALSE)	#��ȡcsv�ļ�
	names(data)<-c("uid","iid","pref")	#��������

	user <- unique(data$uid)			#�û�����
	item <- unique(sort(data$iid))		#��Ʒ����

	M <- matrix(0, length(user), length(item))
	uidx <- match(data$uid, user)		#�û�����
	iidx <- match(data$iid, item)		#��Ʒ����
	i <- cbind(uidx, iidx, pref=data$pref)		#ԭʼ���ݾ���
	for(n in 1:nrow(i)){
		M[i[n,][1],i[n,][2]]<-i[n,][3]			
	}
	dimnames(M)[[2]]<-item
	print("M��");print(M)
	M						#��������ģ�;���
}

#�����û����ƶ�S	(ŷ�Ͼ���)
EuclideanDistanceSimilarity<-function(M){
	row<-nrow(M)
	S<-matrix(0, row, row)
	for(z1 in 1:row){
	 for(z2 in 1:row){
	   if(z1<z2){			#������
		 num<-intersect(which(M[z1,]!=0),which(M[z2,]!=0)) #�ɼ������
		 sum<-0								#�û�z1���û�z2�����ƶ�
		 for(z3 in num){
			sum<-sum+(M[z1,][z3]-M[z2,][z3])^2
		 }
		 S[z2,z1]<-length(num)/(1+sqrt(sum))

		 if(S[z2,z1]>1) S[z2,z1]<-1 		#���㷨����ֵ��������
		 if(S[z2,z1]< -1) S[z2,z1]<- -1 	
	   }
	  }	
	}
	ts<-t(S)					#��ȫ���Ǿ���
	w<-which(upper.tri(ts))
	S[w]<-ts[w]
	print("S��");print(S)
	S
}

#�Ñ������N
NearestNUserNeighborhood<-function(S,n){
	row<-nrow(S)
	N<-matrix(0, row, n)		#ֻ����n�������
	for(z1 in 1:row){
	  for(z2 in 1:n){
		m<-which.max(S[,z1])
		N[z1,][z2]<-m
		S[,z1][m]=0
	   }
	}
	print("N��");print(N)
	N
}

#�Ƽ��㷨R
UserBasedRecommender<-function(uid,n,M,S,N){
	row<-ncol(N)		#row�������
	col<-ncol(M)		#col����Ʒ
	r<-matrix(0, row, col)
	N1<-N[uid,]
	for(z1 in 1:length(N1)){
	   num<-intersect(which(M[uid,]==0),which(M[N1[z1],]!=0)) #�ɼ������
	   for(z2 in num){
		  r[z1,z2] = M[N1[z1],z2]*S[uid,N1[z1]]
	   }
	}
	print("R��");print(r)		#��ӡÿ���û����Ƽ�����

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
	print(paste(uid,"R2��"));print(r2)
	r2
}
#���г���
FILE<-"E:\\R\\Mahout\\testCF.csv"	#ԭʼ�����ļ�
NEIGHBORHOOD_NUM<-2			#����ڸ���
RECOMMENDER_NUM<-3			#����Ƽ����

M<-FileDataModel(FILE)
S<-EuclideanDistanceSimilarity(M)
N<-NearestNUserNeighborhood(S,NEIGHBORHOOD_NUM)
for(i in 1:nrow(M)){
	UserBasedRecommender(i,RECOMMENDER_NUM,M,S,N);
}