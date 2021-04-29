# Accessing AWS

## Access the AWS Server
1. SSH into the AWS server and enter your username and password:<br>
 `ssh username@[server1]` <br>
`ssh username@[server2]`  <br>  
 

2. Access the shared geodyn account :<br>
 ``sudo -u [shared_account_username] -s ``<br> 

   
3. Activate the `pygeodyn` conda environment: <br>
 ``conda activate pygeodyn``
   

## Activating a Jupyter environment (set port forwarding):

In order to view the Jupyter notebook environment on a local machine, we forward the port that jupyter is running on through both servers.

 1. In a command terminal (I do this using Windows 10 powershell):  
```ssh -L 1234:localhost:1234 [username]@[server1]```  
```ssh -L 1234:localhost:1234 [username]@[server2]```  
```sudo -u [shared_account_username] -s ```  
```cd /```  
```conda activate pygeodyn```  
```jupyter notebook --no-browser --port 1234```  
    
  
 2. This will activate something that looks similar to the following script. Copy the bottom link and paste in a browser of your choice. I typically use Chrome.  If all goes according to plan you should be able to see the internal server's directories from the jupyter file browser.  
```text
(base) [########@####### /]$ jupyter notebook --no-browser --port 1234
[I 22:02:02.049 NotebookApp] Serving notebooks from local directory: /
[I 22:02:02.049 NotebookApp] Jupyter Notebook 6.1.4 is running at:
[I 22:02:02.049 NotebookApp] http://localhost:1234/?token=c7675d4550c88b8e69e158d059556801e8152ad35730e7b0
[I 22:02:02.050 NotebookApp]  or http://[########@####### :1234/?token=c7675d4550c88b8e69e158d059556801e8152ad35730e7b0
[I 22:02:02.050 NotebookApp] Use Control-C to stop this server and shut down all kernels (twice to skip confirmation).
[C 22:02:02.054 NotebookApp]

    To access the notebook, open this file in a browser:
        file:///home/[########/.local/share/jupyter/runtime/nbserver-5842-open.html
    Or copy and paste one of these URLs:
        http://localhost:1234/?token=c7675d4550c88b8e69e158d059556801e8152ad35730e7b0
     or http://[####### :1234/?token=c7675d4550c88b8e69e158d059556801e8152ad35730e7b0
     
```





























