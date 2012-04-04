#!/bin/bash
#  Copyright (C) 2012  Alessandro Bruni
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details. 
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#export JAVA_OPTS="-Xmx256M -Xms32M -Xprof"

unfolder="scala -cp bin it.unipd.math.cpnunf.Unfolder "
model_deadlock="modelcheck/modelchecking.lp"

function usage {
echo "${0} [-convert|-unfold|-deadlock] <path>
or ${0} -build

Parameters:

-convert   converts the nets found in <path>
-unfold    unfolds such nets [default option]
-deadlock  checks for deadlock on the unfolding

-build     builds the package
"
}

function compile {
    cd bin
    scalac ../src/it/unipd/math/cpnunf/*.scala
    cd ..
}

function main {
    opt=$1
    path=$2
    for input_file in `find ${path} -name "*.ll_net"`; do

	prefix=`echo ${input_file} | sed s/.ll_net\$//`
	input_file_dot="${prefix}-input.dot"
	input_file_pdf="${prefix}-input.pdf"
	output_file_dot="${prefix}-output.dot"
	output_file_pdf="${prefix}-output.pdf"
	output_file_asp="${prefix}-output.asp"

	if [ $opt = "-convert" ]; then
	    $unfolder -convert $input_file -o $input_file_dot &&
	    dot -Tpdf < $input_file_dot > $input_file_pdf
	elif [ $opt = "-unfold" ]; then
	    $unfolder $input_file -o $output_file_dot &&
	    dot -Tpdf < $output_file_dot > $output_file_pdf
	elif [ $opt = "-deadlock" ]; then
	    $unfolder -asp $input_file -o $output_file_asp
	    cat $output_file_asp $model_deadlock | clingo -n 0
	fi

    done
}

if [ $# -eq 2 ]; then
    main $1 $2
elif [ $# -eq 1 ]; then
    if [ $1 = "-build" ]; then
	compile
    else
	main -unfold $1
    fi
else
    usage
fi
