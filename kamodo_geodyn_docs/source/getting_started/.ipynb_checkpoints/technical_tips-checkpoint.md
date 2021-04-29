# Technical Tips

## Navigating AWS

First access the AWS bastian server and enter your password:  

[comment]: <> ( - ``ssh username@52.1.42.186 `` )

[comment]: <> ( - ``ssh username@172.31.96.209``)

To access the shared geodyn server:  

 - ``sudo -u m_geodyn -s ``

Most of this project exists in the `data` directory.

### Copying files from the inner server.  

Let's say we want to copy the `RUNS` directory to you home directory on `[m_geodyn@172.31.96.209]172.31.96.209`. 

 - Move the file to /tmp:  ``[username@aws_bastion]mv run.tar /tmp``   
- Scp the file in /tmp to bastion host. ``[username@aws_bastion]scp 172.31.96.209:/tmp/run.tar``  
- Both command will copy the run.tar from bastion to your local system. ``[your_local_system]scp run.tar <your_local_system>``  


### Installing miniconda3

Miniconda is the same as anaconda but it’s only the barebones. It doesn’t have the hundreds of data science packages already installed so you will need to install all packages yourself. 
*Install miniconda3 as the user who will be using it.*

- Download the Miniconda3:  
```curl -O https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh```

- Install the download:  
```bash Miniconda3-latest-Linux-x86_64.sh```

You may need to re-open your terminal for miniconda to be installed. When it works, you will see (base) next to your username and the command ``conda`` will work.



### Port Forwarding Jupyter from to a local machine through tunneled ssh:

Prerequisites:  
 - Install miniconda3:  
 - ``Conda install jupyter notebook``  


 1. In one terminal (windows 10 powershell):  
```ssh user@52.1.42.186```  
```ssh 172.31.96.209```  
```jupyter notebook --no-browser --port 1234```  

 2. In a 2nd terminal (windows 10 powershell):  
```ssh -L 1234:localhost:1234 user@52.1.42.186```  
```ssh -L 1234:localhost:1234 user@172.31.96.209```

 3. Open link to jupyter (through the provided link on the first terminal) in new browser on local machine




### Install Kamodo:  

Prerequisites:  
 - Install miniconda3
 - Install jupyter notebook
 - ``Conda install git``  
 - ``Conda install pip`` 
 
 Install Kamodo: ``pip install git+git://github.com/nasa/Kamodo.git``

### Miscellaneous Tips
- Running a script: `./script`

## Using Git

Pushing Documents to Github:  
 `git status`  -- shows which files are not hosted on github  
 `git add .`   -- adds all files to github  
 `git commit -m "message"` -- commits files to be pushed and also adds a message  
 `git push origin master`  -- pushes the files to github to the master branch

To display the Sphinx HTML on Github:
 - add a docs folder that contains all the html files.
    - can add a feature to the make.bat file to saves htmls to docs with a command
 - add a .nojekyll file to the file to the docs folder
 - turn on Pages in the settings under github and make the root directory docs



































