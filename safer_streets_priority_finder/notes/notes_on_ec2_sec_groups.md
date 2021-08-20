

## Current AWS security groups

### sg-33c29939 - default
This security group is intended to manage the inbound and outbound rules for the PostgreSQL basebase(s) for the Tool. Only EC2 instances that manage the application and the model processor, as well as the developers local IP address should be allowed inbound access. 

### sg-0d0fa4b879fea11e6 - launch-wizard-2
Manages in/outbound rules for the EC2 instance that holds the Tool. Public inbound access is allowed via an HTTP call, and developers are allowed inbound access via an SSH tunnel. 






