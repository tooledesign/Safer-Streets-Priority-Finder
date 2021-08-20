# The Safer Streets Priority Finder Model Processor

## What's here?
This application is a background processor that manages the model estimation for the Safer Streets Priority Finder.
 
### How it works
1. The model processor runs continuously and checks the accounts table every minute for a 'model_needed' value under the 'model_status' column in the accounts table.
2. If the processor finds an account that has requested a model, it stops, collects the account information, and prepares for model production. 
3.  At this point, the processor updates the 'model_status' for the scenario to 'model_currently_running,' which can be checked in the public-facing application.
4. After the model estimation is done, the account's 'model_status' value is updated to 'model_estimation_completed.' At this point, the user can access their results via a visualization within the application or download the data for their own post-processing.

Unless you adjust this part of the script, a docker container will run only a single model. The developers of this application found that a single container was not stable for subsequent models runs and that a single container could handle only a handful of models. It's recommended to start a fresh new container for each model. 
 
 To launch the model processor, [see these notes](https://github.com/tooledesign/Safer-Streets-Priority-Finder/blob/main/README.md. 