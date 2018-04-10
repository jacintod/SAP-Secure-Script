#!/bin/bash
echo 'Executing VirtualEnv activate script'
source /usr/sap/SAPBW/intf/py_scripts/xlsx_conv/bin/activate
echo 'Verify that the virtualenv is active'
which python
python /usr/sap/SAPBW/intf/py_scripts/xlsx_conv/xlsx_convert.py -l /usr/sap/SAPBW/intf/SBIB/in/ -s /usr/sap/SAPBW/intf/SBIB/in/ -d /usr/sap/SAPBW/intf/SBIB/out/


