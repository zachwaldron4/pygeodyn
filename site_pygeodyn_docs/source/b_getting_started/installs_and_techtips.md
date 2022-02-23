# Installations and Technical Tips


***Under Construction!***

Most of the installations detailed here should not need to happen again on the AWS server, but are documented nonetheless.

## Installing miniconda3 :
Miniconda is the same as anaconda but it’s only the barebones. It doesn’t have the hundreds of data science packages already installed so you will need to install all packages yourself.

- Install miniconda3 to any machine
 `curl -O https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh`
 `bash Miniconda3-latest-Linux-x86_64.sh`

    - You may need to re-open your terminal for miniconda to be installed. When it works, you will see (base) next to your username and the command `conda` will work.


## Installing Kamodo:

Prerequisites:  
 - Install miniconda3 (see above)
 - Install jupyter notebook: `conda install jupyter`
 - ``Conda install git``  
 - ``Conda install pip`` 
 
 Install Kamodo: ``pip install git+git://github.com/nasa/Kamodo.git``


[comment]: <> (## Using Git)

[comment]: <> (Pushing Documents to Github:  )

[comment]: <> ( `git status`  -- shows which files are not hosted on github  )

[comment]: <> ( `git add .`   -- adds all files to github  )

[comment]: <> ( `git commit -m "message"` -- commits files to be pushed and also adds a message  )

[comment]: <> ( `git push origin master`  -- pushes the files to github to the master branch)

[comment]: <> (To display the Sphinx HTML on Github:)

[comment]: <> ( - add a docs folder that contains all the html files.)

[comment]: <> (    - can add a feature to the make.bat file to saves htmls to docs with a command)

[comment]: <> ( - add a .nojekyll file to the file to the docs folder)

[comment]: <> ( - turn on Pages in the settings under github and make the root directory docs)



































