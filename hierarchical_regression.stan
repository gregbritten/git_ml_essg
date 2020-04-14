data{
	int P;	
	int p; //extract sample size parameter from data
	int N; 
	int pi[P*p+1]; //extract linear indices for station data
	int n[P*p];
	vector[N] y; //extract y variable
	vector[N] x; //extract x variable
}
parameters{
	real beta1_u;
	real beta0_u;
	real<lower=1E-15,upper=3> beta1_g_sd;
	real<lower=1E-15,upper=3> beta0_g_sd;
	real beta1_g[P];
	real beta0_g[P];
	real<lower=1E-15,upper=3> beta1_i_sd;
	real<lower=1E-15,upper=3> beta0_i_sd;
	real beta1_i[P*p];
	real beta0_i[P*p];
	real<lower=1E-15> sigma_y;
}
model{
	int k=1;
	
	for(i in 1:P){
		beta1_g[i] ~ normal(beta1_u,beta1_g_sd);
		beta0_g[i] ~ normal(beta0_u,beta0_g_sd);
		for(j in 1:p){
			beta1_i[k] ~ normal(beta1_g[i],beta1_i_sd);
			beta0_i[k] ~ normal(beta0_g[i],beta0_i_sd);
			y[(pi[k]+1):pi[k+1]] ~ normal(beta0_i[k] + beta1_i[k]*x[(pi[k]+1):pi[k+1]], sigma_y); 
			k = k + 1;
		}
	}
}
// generated quantities{
	// vector[N] y_pred;
	// int j;
	// j = 1;
	// for(i in 1:p){
		// for(k in 1:n[i]){
			// y_pred[j] = normal_lpdf(y[j] |beta0[i] + beta1*x[j] , sigma);
			// j = j+1;
		// }
	// }
// }

