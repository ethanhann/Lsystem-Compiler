#!/bin/bash
#
# Primary Author: Michael Eng (mse2124)
#
#Run this file with the command:
#bash test.sh
#
#Three phases: Compiles and runs computational test files in Test/, attempts to compile erroneous test files in Test/Semantic, and compiles, runs, then validates image output data for test files in Test/Draw.
################################
make
FILES="Test/*.ls"
ACTION="-c"
TESTACTION="-t"
EXECUTABLE="./lsystem"
finalarr=()
echo "---------"
echo "Stage 1: Compiling computational programs in Test directory to Java"
echo "---------"
arr=()
for f in $FILES #Iterate through Test/, compile each
do	
	#shortname= ${f:5}
	noex=${f%.ls}
	shortname=${noex:5}
	echo -ne "Compiling $shortname.ls..." #-ne means no newline
	$EXECUTABLE $ACTION $f $TESTACTION
	wait
	if [ -e "$shortname.java" ]
	then
		echo ""
	else
		arr+=($f)
		finalarr+=($f)
		#echo "Adding $shortname to array"
	fi
done
fails=${#arr[@]}
if [ $fails != 0 ]
then
	echo "${#arr[@]} test file(s) did not compile properly:" #Output list of files that did not compile to Java as expected
	for var in "${arr[@]}"
		do
		echo "${var}"
		done
else
	echo "All test files compiled properly to Java."
fi

####################################################
#Check that each output java file has a .class file#
####################################################
echo "--------"
echo "Stage 2: Checking that each Java file has a corresponding class file"
echo "--------"
FILESF="./*.java" #Change to (pwd)/*.java later?
ACTION="javac "
arr2=()
for f in $FILESF
do
	shortname=${f:2}
	noex=${shortname%.java}
	echo -ne "Checking that $shortname has a corresponding .class file- "
	#$ACTION $shortname
	if [ -e "$noex.class" ]
	then
		echo "$noex.class exists"
	else
		arr2+=($f)
		finalarr+=($f)
		echo "Error compiling $shortname"
	fi
done
fails2=${#arr2[@]} #output list of files that did not compile from Java to class files as expected
if [ $fails2 != 0 ]
then
	echo "${#arr2[@]} java file(s) did not compile properly:"
	for var in "${arr2[@]}"
		do
		echo "${var}"
		done
else
	echo "All compiled ls files were compiled to Java class files."
fi


##########################################################################################
#Execute script to start comparing compute test .class files to expected output for each.#
##########################################################################################
echo "--------"
echo "Stage 3: Executing computational java files and comparing against expected output:"
echo "--------"
declare -A expected
arr3=()
while read line
do
	IFS='~' read -ra ADDR <<< "$line"
#	expected["${ADDR[0]}"]="${ADDR[1]}"
	expected+=( ["${ADDR[0]}"]="${ADDR[1]}" )
done < Test/expected.txt
for x in "${!expected[@]}"
do
	#echo "$x: ${expected["$x"]}"
	if [ -e "$x.class" ]
	then
		compare=${expected["$x"]}
		if [ $x = "longprint" ] #Hacky fix.  Couldn't embed the newlines into a line of text in the expected.txt file.
		then
			compare="
n
n
n
n
n
s
w
e
r
t
y
Hello world"
		fi
		echo "----"
		echo "Running $x, expected output is $compare"
		actual=`java $x`
		wait
		rm -f $x.txt
		if [ "$actual" != "$compare" ]
		then
			echo "Error comparing $x: $actual != $compare"
			arr3+=(Test/$x.ls)
			finalarr+=($x)
		else
			echo "Match for $x: $actual = $compare"
		fi
	else
		echo "$x was not successfully compiled into Java byte code, skipping it..." #Files in this state were already added to the report in Stage 2
	fi
done


fails3=${#arr3[@]} #Output list of files that did not execute properly
if [ $fails3 != 0 ]
then
	echo "${#arr3[@]} java file(s) did not execute properly or did not compile from Java into Java bytecode:"
	for var in "${arr3[@]}"
		do
		echo "${var}"
		done
else
	echo "All computational test files executed as expected."
fi

#######################################################################################################################################
#Attempt to compile files in Semantic subdirectory.  They should all generate compiler errors and not create corresponding java files.#
#######################################################################################################################################
SEMANTICFILES="Test/Semantic/*.ls"
echo ""
echo ""
echo "--------"
echo "Stage 4: Compiling semantic test files, these should all cause compiler errors and fail to create Java code:"
echo "--------"
SEMANTICACTION="./lsystem -c"
semanticarr=()
for s in $SEMANTICFILES #Iterate through Test/, compile each
do	
	#shortname= ${f:5}
	noex=${s%.ls}
	shortname=${noex:14}
	echo -ne "Compiling $shortname.ls..." #-ne means no newline
	$SEMANTICACTION $s
	wait
	if [ -e "$shortname.java" ]
	then
		echo ""
		echo "$shortname.java exists- test program did not fail as expected"
		echo ""
		semanticarr+=($s)
		finalarr+=($s)
	else
		echo "Compiler error encountered, program fails as expected."
		#echo "Adding $shortname to array"
	fi
	echo ""
done
semanticfails=${#semanticarr[@]} #Output list of files that did not fail to compile as expected
if [ $semanticfails != 0 ]
then
	echo "${#semanticarr[@]} test file(s) did not fail properly:"
	for var in "${semanticarr[@]}"
		do
		echo "${var}"
		done
else
	echo "All files failed as expected."
fi

#Clean out generated java and class files
CLEAN="rm *.java"
$CLEAN
CLEAN="rm *.class"
$CLEAN

########################################################################
#Compile files in Draw subdirectory.								   #
#Then run each, get its resulting image bitstring.					   #
#Then compare to an expected bitstring (get from file in subdirectory).#
########################################################################
echo "----------"
echo "Stage 5: Compile and run drawing test classes, compare resulting image data to expected results"
echo "----------"
FILESF="Test/Draw/*.ls"
arr5=()
arr6=()
arr7=()
for f in $FILESF
do
	shortname=${f:10}
	noex=${shortname%.ls}
	echo "Compiling $shortname..."
	./lsystem -c $f -t
	wait
	if [ -e "$noex.java" ]
	then
		echo "$shortname compiled successfully to Java file"
	else
		echo "$shortname failed to compile to a Java file"
		arr5+=($f)
	fi
	if [ -e "$noex.class" ]
	then
		echo "$shortname compiled successfully to an executable class file"
	else
		echo "$shortname failed to compile into a class file"
		arr6+=($f)
	fi
	java $noex
	wait
	if [ -e "$noex.txt" ]
	then
		echo "Image bitstring output file generated, comparing to expected result..."
		DIFF=$(diff -q $noex.txt Test/Draw/Expected/$noex.txt)
		wait
		if [ "$DIFF" != "" ]
		then
			echo "Error- differences found in image data.  Recompile $noex.ls without the -t flag and run to visually verify correctness"
			arr7+=($f)
		else
			echo "Image bitstring output matches for $noex"
		fi
	else
		echo "An error has occurred and the bitstring output file couldn't be found"
		arr7+=($f)
	fi
	wait
	rm $noex.txt
	echo ""
done
drawfails=${#arr5[@]} #List of drawing files that failed to compile to Java
let "drawfails += ${#arr6[@]}" #List of drawing files that failed to compile from Java to a class file
let "drawfails += ${#arr7[@]}" #List of drawing files that ran and output different image output than expected
if [ $drawfails != 0 ]
then
	echo "$drawfails test file(s) did not behave as expected in this stage:"
	for var in "${arr5[@]}"
		do
		finalarr+=($var)
		echo "${var}"
		done
	for var in "${arr6[@]}"
		do
		finalarr+=($var)
		echo "${var}"
		done
	for var in "${arr7[@]}"
		do
		finalarr+=($var)
		echo "${var}"
		done
else
	echo "All draw test files compiled and ran as expected."
fi





numfails=${#finalarr[@]}
if [ $numfails == 0 ]
then
	echo "-------------"
	echo "All test cases passed."
else
	echo "-------------"
	echo "The following test cases did not perform as expected:"
	echo "-------------"
	for var in "${arr[@]}"
	do
		echo "$var failed to compile into a Java file."
	done
	for var in "${arr2[@]}"
	do
		echo "$var failed to compile from a Java file into a class file."
	done
	for var in "${arr3[@]}"
	do
		echo "$var did not execute as expected (either the output result was wrong or a runtime error occurred)."
	done
	for var in "${semanticarr[@]}"
	do
		echo "$var did not fail to compile, as expected."
	done
	for var in "${arr5[@]}"
	do
		echo "$var, a drawing test program, did not compile into a Java file."
	done
	for var in "${arr6[@]}"
	do
		echo "$var, a drawing test program, did not compile from a Java file into a class file."
	done
	for var in "${arr7[@]}"
	do
		echo "$var did not draw the expected output image, please recompile it without the -t flag and run to visually verify image integrity."
	done
fi

######################################
#Clean generated java and class files#
######################################
CLEAN="rm *.java"
$CLEAN
CLEAN="rm *.class"
$CLEAN
CLEAN="rm *.txt"
$CLEAN
wait
