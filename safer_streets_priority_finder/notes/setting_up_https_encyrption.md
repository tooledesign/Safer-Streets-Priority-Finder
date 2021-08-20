

## Encrypting a Dockerized Shiny Application

This walkthrough is an adaptation of the information found [here](https://www.r-bloggers.com/2021/05/host-shiny-apps-with-docker/). 

#### What you'll need:
1. A domain name that points to your application. 
2. Access to the command line where the dockerized Shiny application is hosted. 
3. sudo privileges within the EC2 instance
4. A running dockerized shiny application (accessible via http call). Not tested, but I would avoid mapping the application to port 80. That is generally reserved for http calls. In this tutorial we'll use port ```9000```. 
<br>

### Step 1. Sudo User 
The first step is to log into the EC2 instance. Once your in there, sudo up with ```sudo su```. 

### Step 2. Ensure your applocation is running
Run the following to print out the running containers. 
<br>``` docker ps --format 'table {{.Names}}\t{{.Ports}}'```

### Step 3. Install Caddy 
If you haven't already installed Caddy, you'll need to for https encryption. Caddy is a free and open-source technology for serving encrypted websites. You can read more about Caddy [here](https://caddyserver.com/docs/).

```
apt install -y debian-keyring debian-archive-keyring apt-transport-https
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo apt-key add -
apt-add-repository -r ppa:certbot/certbot
apt update
apt install caddy
```

### Step 4. Check Domain 
Once you've installed Caddy, check that your installation was successful. You can do this by checking the version with, ```-version caddy```. You can also check your domain, which in my case was ```https://www.highriskstreetsfinder.com```.

At this point, your browser should see a Caddy template page with the 'Congratulations!' near the top left.

### Step 5. Edit the Caddy file 
At this step, we need to edit the Caddy file. You can access and edit the file with the nano command. 

```
nano /etc/caddy/Caddyfile
```

Either delete comment out everything in the current Caddyfile and add the following: 

```
{
    email your.name@example.com
}
test.analythium.net {
    root * /var/www/html
    reverse_proxy 0.0.0.0:9000
    file_server
}
```

You'll want to edit the email address found in the global block to communicate if there are any problems with the [ACME](https://en.wikipedia.org/wiki/Automated_Certificate_Management_Environment) (Automated Certificate Management Environment). 

You'll also notice the reverse_proxy is mapping to port ```9000```. If you've mapped your Shiny application to a different port number, you'll need to update that value. 

### Step 6. Reload Caddy

Now that you've updated your Caddyfile, you'll need to reload Caddy. You can do this with: 

```
systemctl reload caddy
```

### Step 7. Setup your firewall 
You can easily setup your firewall with the uncomplicated firewall (```ufw```) command.

Run the following, and your application should be encrypted. 

```
ufw default deny incoming
ufw default allow outgoing
ufw allow ssh
ufw allow https
ufw --force enable
```
