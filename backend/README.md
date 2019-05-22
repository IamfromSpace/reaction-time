# Training App

## Deploy

### First time deploy:
1. Create the four stacks in backend/cloudformation.  Name them `training-${env}-${filename with dashes instead of underscores}`
1. To complete Custom Domain Name/Certificate stack, add the validation DNS record (listed in the cloudformation stack events)
1. The certificate stack outputs a value named RegionalDomainName, create a DNS CNAME from your custom domain to this domain.
1. Build the lambda code via `stack build --copy-bins && zip -j training-${env}-api.zip ~/.local/bin/bootstrap`
1. Push the zip file to the newly created bucket
1. Get the version string of the file
1. Deploy the backend/template.yaml stack as `training-${env}-api`, using the version of the zip in S3 and the names of all previous stacks.
1. Deploy the frontend/cloudformation/bucket.yaml stack as `training-${env}-browser-app-code`
1. Inject the clientId (exported by the user-pool stack) and the custom domain into frontend/src/Main.elm
1. Build the browser app via `node_modules/elm/bin/elm make src/Main.elm --output index.js`
1. Create an `index.html` file based on the `template.html` file with the generated js inside the appropriate script tag.
1. Push the resulting `index.html` frontend build into the newly created s3 stack
1. Deploy the `frontend/cloudformation/api_s3_proxy.yaml` as `training-${env}-api-s3-proxy` using the previously created stacks as parameters.
1. Within the console, navidate to the Api Gateway name `training-${env}-api` and re-deploy the `default` stage.
1. Create users in the Cognito console (Manage User Pools > training-${env}-user-pool > Users and Groups > Create user)
