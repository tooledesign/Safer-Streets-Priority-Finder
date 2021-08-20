ECHO navigate to dir with DOCKERFILE
cd Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/safer_streets_priority_finder/

ECHO building image
docker build -t vulusr_beta .

ECHO namespacing 
docker tag  tooledesign/vulusr_beta vulusr_beta

ECHO push to docker repo
docker push tooledesign/vulusr_beta

ECHO prune 
docker container prune -f
docker image prune -f
