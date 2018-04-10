#!/usr/bin/env bash
# This script will be a shell script for the creation of the new virtualenv's

clear
echo "Secure Script Framework Verification Shell Script"
echo "Execution Timestamp : $(date)"

# Local Variables used in the script
VIRENV="$2" # virtualenv name
VIRENVPATH="$1" # Path of the virtualenv

echo "Verify virtual environment :" $VIRENV
source $VIRENVPATH/$VIRENV/bin/activate

if [ $? -eq 0 ];then
    echo "Succesfully activated new virtualenv called " $VIRENV
    which python
    if [ $? -eq 0 ];then
        echo "Succesfully verified python executable"
    else
        echo "ERROR: Unable to verify python executable"
        echo >&2
    fi
else
    echo "ERROR:Unable to activate virtualenv" $VIRENV
    echo >&2
fi
