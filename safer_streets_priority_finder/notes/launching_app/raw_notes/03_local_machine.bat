ECHO navigate to dir with DOCKERFILE
cd Documents/GitHub/a0137_vulnerable_user_risk_network_tool/99x_main_tool_dev/safer_streets_priority_finder/

ECHO building image
docker build -t minifying . 
docker build -t processor . 
docker build -t vulusr_beta .
ECHO tag file
docker tag minifying bikingman/minifying
docker tag processor bikingman/processor
docker tag vulusr_beta bikingman/vulusr_beta
ECHO push to docker repo
docker push bikingman/minifying
docker push bikingman/processor
docker push bikingman/vulusr_beta
ECHO prune 
docker container prune 
docker image prune
