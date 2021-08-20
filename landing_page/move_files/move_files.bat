cd a0137_vulnerable_user_risk_network_tool/
git checkout beta_revisions
git pull https://github.com/tooledesign/a0137_vulnerable_user_risk_network_tool.git

cp /home/ubuntu/favicon.ico /var/www/html
cp -R /home/ubuntu/assets /var/www/html
cp -R /home/ubuntu/images /var/www/html
cp -R /home/ubuntu/public_datasets /var/www/html
 cp /home/ubuntu/index.html /var/www/html

scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/landing_page/index.html ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/var/www/html
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/landing_page/assets ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/var/www/html
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/landing_page/images ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/var/www/html
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/public_datasets ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/var/www/html/public_datasets

 scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem -r /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/landing_page/index.html ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/home/ubuntu
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem -r /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/landing_page/assets/   ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/home/ubuntu/ 
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem -r /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/landing_page/images/  ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/home/ubuntu/ 
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem -r  /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/public_datasets/  ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/home/ubuntu/
scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem -r  /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/public_datasets/  ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/home/ubuntu/

 scp -i /Users/Bikingman/Desktop/usdot_vul_usr_2021.pem -r /Users/Bikingman/Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/USDOTvulusrTOOL/inst/app/www/favicon.ico ubuntu@ec2-44-197-101-110.compute-1.amazonaws.com:/var/www/html

