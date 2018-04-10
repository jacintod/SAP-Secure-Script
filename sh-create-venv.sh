#!/usr/bin/env bash
# This script will be a shell script for the creation of the new virtualenv's

clear
echo "Secure Script Framework Configuration Shell Script"
echo "Execution Timestamp : $(date)"

# Local Variables used in the script
VIRENV="$2" # virtualenv name
VIRENVPATH="$1" # Path of the virtualenv

echo "Step 1 - Pre-Reqs of Creation of new virtualenv :" $VIRENV
# Check to see if the python packages are installed on the node / server
echo "........ Checking user executing script belongs to SAPSYS"

if ! id -nG "$USER" | grep -qw "SAPSYS"; then
    # Check to see if pip is installed on the server
    echo "........ Checking if pip and virtualenv is installed"
    if  pip --version | grep -qw "pip"; then
        echo "........ pip is installed, checking virtualenv is installed"
        # Check to see if the virtualenv package is installed
        if  python -c "help('modules')" | grep -qw "virtualenv"; then
            echo "........ virtualenv is installed, continuing to Step 2 - Creating virtualenv"
                echo "Step 2 - Creating new virtualenv called :" $VIRENV
                echo "........ virtualenv path = " $VIRENVPATH
                # Go to the destination directory for the virtualenv
                cd $VIRENVPATH
                # Go to the working directory of the virtualenv
                if [[ $PWD = $VIRENVPATH ]] ; then
                    echo "........ current script working directory " $VIRENVPATH
                    # This command works on my development environment ubuntu, but on a SLES environment it doesnt work and you have to link the libraries
                    echo "Creating the new virtualenv with --always-copy and --no-download options"
                    virtualenv --always-copy --no-download $VIRENV

                    if [ $? -eq 0 ];then
                        echo "Succesfully created new virtualenv called " $VIRENV
                        virtualenv --relocatable $VIRENV
                        if [ $? -eq 0 ];then
                            echo "Succesfully made" $VIRENV "relocatable"
                        else
                            echo "ERROR: Making the virtualenv relocatable"
                            echo >&2
                        fi
                    else
                        echo "ERROR: Creating virtualenv"
                        echo >&2
                        echo "Start the linking process for the lib64 modules"

                        cd $VIRENV/lib64/python2.6/lib-dynload/
                        # Do the symbolic linking process
                        if [ $? -eq 0 ];then
                            ln -s ../../../lib/python2.6/lib-dynload/* .
                        else
                            echo "ERROR: No Lib64 folder exists"
                            echo >&2
                        fi

                        echo "Start the linking process for the config modules"
                        cd $VIRENV/lib64/python2.6/

                        # Do the symbolic linking process
                        if [ $? -eq 0 ];then
                            ln -s ../../lib/python2.6/config .
                        else
                            echo "ERROR: No Lib64 folder exists"
                            echo >&2
                        fi

                        # Once the symbolic linking process has been done, restart the virtualenv process
                        cd $VIRENVPATH

                        if [ $? -eq 0 ];then
                            virtualenv --always-copy --no-download $VIRENV

                            if [ $? -eq 0 ]; then
                                virtualenv --relocatable $VIRENV
                            else
                                echo "ERROR: Making the virtualenv relocatable"
                                echo >&2
                            fi

                        else
                            echo "ERROR: Recreating the virtualenv"
                            echo >&2
                        fi

                    fi

                else
                    echo "ERROR: Invalid working directory, are you sure that the directory exists ??"
                fi


        else
            echo "ERROR: virtualenv is not installed"
            echo "ERROR: Execution Terminated, install virtualenv as 'root' user"
        fi
    else
        echo "ERROR: pip is not installed"
        echo "ERROR: Execution Terminated, install pip and setuptools as 'root' user"
    fi
else
   echo "ERROR:'$USER' executing the script does not belong to 'SAPSYS'."
   echo "ERROR: Execution Terminated, User needs to belong to SAPSYS Group"
fi
