libname hw 'C:\Users\johnb\OneDrive\Documents\MSA\Fall 2\Data Mining\HW1';

data hw.restaurantWide2;
	infile 'C:\Users\johnb\OneDrive\Documents\MSA\Fall 2\Data Mining\HW1\restaurant_wide.csv' dsd;
	length order $200;
	input orderID $ order $;
run;
