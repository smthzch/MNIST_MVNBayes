library(corpcor)

#Multivariate Normal Bayesian Classifier
# p(C|x) = p(x|C)/sum(p(x|C'))
bayesClass.train <- function(x, y){
	y = as.factor(y)
	
	mod = list()
	mod$y = levels(y)
	for(cls in mod$y){
		cind = which(y==cls)
		subx = x[cind,]
		#subx = qnorm((x[cind,])/256)# rnorm(ncol(x))
		subx[subx>0]=1
		subx = apply(subx,2, function(x){
			si = which(x==0)
			sil = length(si)
			x[si] = rbinom(sil,1,.08)
			x
		})
		
		u = colMeans(subx)
		#rms = which(u==0)
		s = cov.shrink(subx)[,]
		#s = s[-rms,-rms]
		#s = s + diag(0.01, nrow(s))
		#u = u[-rms]
		
		invs = solve(s)
		dets = det(s)
		
		mod[paste(cls,"u",sep="")] = list(u)
		mod[paste(cls,"s",sep="")] = list(s)
		mod[paste(cls,"invs",sep="")] = list(invs)
		mod[paste(cls,"dets",sep="")] = list(dets)
		mod[paste(cls,"rm",sep="")] = list(rms)
		
	}
	
	mod
}

bayesClass.pred = function(m, x){
	y = c()
	
	for(i in 1:nrow(x)){
		probs = list()
		curP = -Inf
		curC = ""
		
		for(cls in m$y){
			u = m[paste(cls,"u",sep="")][[1]]
			invs = m[paste(cls,"invs",sep="")][[1]]
			dets = m[paste(cls,"dets",sep="")][[1]]
			#rms = m[paste(cls,"rm",sep="")][[1]]
			tx = x[i,]
			tx[tx>0] = 1
			pr = bayesClass.evalLL(tx, u, invs, dets)
			
			if(pr>curP){
				curP = pr
				curC = cls
			}
			
			probs[cls] = pr
		}
		
		y = c(y, as.numeric(curC))
	}
	y
}

l2pi = .5*log(2*pi) 
bayesClass.evalLL = function(x, u, invs, dets){
	k = length(u)
	-.5*(t(x-u)%*%invs%*%(x-u))# -  (k*l2pi + .5*dets)
}


#### rows and columns, not pixels
# 
# bayesClass.train <- function(x, y){
# 	y = as.factor(y)
# 
# 	mod = list()
# 	mod$y = levels(y)
# 	for(cls in mod$y){
# 		cind = which(y==cls)
# 		subx = x[cind,]
# 		#subx = qnorm((x[cind,])/256)# rnorm(ncol(x))
# 		#subx[subx>0]=1
# 		subx = apply(subx,2, function(x){
# 			si = which(x==0)
# 			sil = length(si)
# 			x[si] = rbinom(sil,1,.08)
# 			#x[si] = rpois(sil,1)
# 			x
# 		})
# 		
# 		u = colMeans(subx)
# 		#rms = which(u==0)
# 		s = cov(subx)
# 		#s = cov.shrink(subx)[,]
# 		#s = s[-rms,-rms]
# 		#s = s + diag(0.01, nrow(s))
# 		#u = u[-rms]
# 		
# 		invs = solve(s)
# 		dets = det(s)
# 
# 		invs = solve(s)
# 		dets = det(s)
# 
# 		mod[paste(cls,"u",sep="")] = list(u)
# 		mod[paste(cls,"s",sep="")] = list(s)
# 		mod[paste(cls,"invs",sep="")] = list(invs)
# 		mod[paste(cls,"dets",sep="")] = list(dets)
# 		mod[paste(cls,"rm",sep="")] = list(rms)
# 
# 	}
# 
# 	mod
# }
# 
# bayesClass.pred = function(m, x){
# 	y = c()
# 
# 	for(i in 1:nrow(x)){
# 		probs = list()
# 		curP = -Inf
# 		curC = ""
# 
# 		for(cls in m$y){
# 			u = m[paste(cls,"u",sep="")][[1]]
# 			invs = m[paste(cls,"invs",sep="")][[1]]
# 			dets = m[paste(cls,"dets",sep="")][[1]]
# 			#rms = m[paste(cls,"rm",sep="")][[1]]
# 			tx = x[i,]
# 			#tx[tx>0] = 1
# 			pr = bayesClass.evalLL(tx, u, invs, dets)
# 
# 			if(pr>curP){
# 				curP = pr
# 				curC = cls
# 			}
# 
# 			probs[cls] = pr
# 		}
# 
# 		y = c(y, as.numeric(curC))
# 	}
# 	y
# }
# 
# l2pi = .5*log(2*pi)
# bayesClass.evalLL = function(x, u, invs, dets){
# 	k = length(u)
# 	-.5*(t(x-u)%*%invs%*%(x-u)) -  (k*l2pi + .5*log(dets))
# }
# 
