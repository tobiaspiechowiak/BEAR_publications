
%load data
path_ = '../../Data/REM/WP4/'; 
data = importdata(strcat(path_,'REM_data_WP4.txt'));

frq = 86:43:9976; 

for idx = 1:length(data)
idx
new_data(idx,:) = ThirdOctSmoothing(data(idx,:),frq,1/3);

end

csvwrite(strcat(path_,'Smoothed_REM_WP4.csv'),new_data)




