# Created by Zach Waldron
# on 3/12/21
echo "This script runs the geodyn script for all ARCS of a SINGLE density model"
#
#
#
#
#
FIND_DENIN_LINE=IN_DEN_DIR
DENIN_LINE="IN_DEN_DIR=msis" 
sed -i "s/^${FIND_DENIN_LINE}.*/${DENIN_LINE}/"  geodyn_st.sh

FIND_DEN_LINE=DEN_DIR
DEN_LINE="DEN_DIR=msis00" 
sed -i "s/^${FIND_DEN_LINE}.*/${DEN_LINE}/"  geodyn_st.sh

FIND_GDYN_line=GDYN_version
GDYN_line="GDYN_version=_msis00_f90" 
sed -i "s/^${FIND_GDYN_line}.*/${GDYN_line}/"  geodyn_st.sh

#######################################################################

#FIND_ARC_line=ARC_name=
#which_arc="ARC_name=${1:-st030914_2wk}"
#sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh
#
#echo "Edit the script to run, ARC1 of 8"
#bash geodyn_st.sh 2> output_text.txt
#
#######################################################################
#
#FIND_ARC_line=ARC_name=
#which_arc="ARC_name=${1:-st030928_2wk}"
#sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh
#
#echo "Edit the script to run, ARC2 of 8"
#bash geodyn_st.sh 2> output_text.txt
#
#######################################################################
#
#FIND_ARC_line=ARC_name=
#which_arc="ARC_name=${1:-st031012_2wk}"
#sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh
#
#echo "Edit the script to run, ARC3 of 8"
#bash geodyn_st.sh 2> output_text.txt
#
#######################################################################
#
#FIND_ARC_line=ARC_name=
#which_arc="ARC_name=${1:-st031026_2wk}"
#sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh
#
#echo "Edit the script to run, ARC4 of 8"
#bash geodyn_st.sh 2> output_text.txt

#######################################################################

FIND_ARC_line=ARC_name=
which_arc="ARC_name=${1:-st031109_2wk}"
sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh

echo "Edit the script to run, ARC5 of 8"
bash geodyn_st.sh 2> output_text.txt



#######################################################################

FIND_ARC_line=ARC_name=
which_arc="ARC_name=${1:-st031123_2wk}"
sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh

echo "Edit the script to run, ARC6 of 8"
bash geodyn_st.sh 2> output_text.txt

#######################################################################

FIND_ARC_line=ARC_name=
which_arc="ARC_name=${1:-st031207_2wk}"
sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh

echo "Edit the script to run, ARC7 of 8"
bash geodyn_st.sh 2> output_text.txt


#######################################################################

FIND_ARC_line=ARC_name=
which_arc="ARC_name=${1:-st031221_2wk}"
sed -i "s/^${FIND_ARC_line}.*/${which_arc}/"  geodyn_st.sh

echo "Edit the script to run, ARC8 of 8"
bash geodyn_st.sh 2> output_text.txt
















