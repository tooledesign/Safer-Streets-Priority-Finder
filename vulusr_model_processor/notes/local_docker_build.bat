ECHO navigate to dir with DOCKERFILE
cd Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/vulusr_model_processor/

ECHO building image
docker build -t vulusr_beta_processor_v1 . 

ECHO tag file
docker tag processor tooledesign/vulusr_beta_processor_v1

ECHO push to docker repo
docker push tooledesign/vulusr_beta_processor_v1

ECHO prune 
docker container prune 
docker image prune 