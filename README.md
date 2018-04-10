# SAP-Secure-Script

The reason for building the secure script framework was due to the strict security requirements ascribed by the client. One of the challenges is that the SAP BW System had several programs which allowed the support users to upload files to the file system, thus in theory they could upload malicious scripts and then execute them on the server / environment.
Although, in theory we could use the security roles to limit access to these programs in order to achieve this requirement of ensring that any script that SAP BW is executing is secure. There was an additional need to ensure proper ITSM principles and procedures by providing an audit trail. (i.e. Transports, Remedy Service Request, Change Approval's, Testing, etc)
This solution was built specifically in mind to be deployed on a SAP BW System landscape, but this does not have to be limited to SAP BW, it could be deployed onto any SAP Systems if required. 


*In essence this framework does the following :*

  * The ability of creating a logical execution script wrapped in a python virtual environment
  
  * The ability of ensuring that the script that gets transported into PROD is the one that was tested and approved

  * The ability of adhering to ITSM principles by providing an logical audit trail of testing of the script, who approved it, business approval, CAB approval, etc

*How does this framework achieve this :*

  * It allows you to create a logical definition of a script/ scripting virtual environment

  * Based on the md5/sha sum of the file, it will ensure that the script is not tampered with as it moves through the SAP landscape

*Common use cases :*

  * Regardless of the security and ITSM use case requirements, below is a list of some common use cases for this secure script framework

  * SAP BW on HANA/ BW4HANA already has several use cases in which you can extend your data and leverage your existing R scripts. But there is not allot of use cases in which you can reuse some of your python scripts which can include various machine learning models, etc. This solution can enable you to call your python scripts in a secure fashion

  * You can securely call any other script, for the purposes on this project python was used, but you can call any script. For Example: Bash script, Perl Script, R Script, Java Program, etc

*Usage of the secure script framework :*

For more information on the usage and the steps required to make use of the secure script framework, kindly refer to the [Wiki Secure Script](https://github.com/jacintod/SAP-Secure-Script/wiki "SAP Secure Script Framework")
