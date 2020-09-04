%load data
load('REM_data.mat')

frq = 100:43:10000;


for idx = 1:length(data)
idx 
new_data(idx,:) = ThirdOctSmoothing(data(idx,:),frq,1/3);

end


csvwrite('Smoothed_rem.csv',new_data)




