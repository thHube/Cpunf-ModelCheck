<h1>Model checking through contextual unfolding</h1>
<p>
    This piece of software is an experimentation of model checking through
    contextual petri nets (petri nets with read-arc). Here we use a piece of 
    software (the unfolder) that was previously developed by Alessandro Bruni
    in his master thesis. All copyrights are given in the source code. 
    See "LICENSE" file for license agreement. 
</p>
<p>
To compile the source code into a jar you will need to have the scala compiler
and ant installed. Then just type <code>ant</code> from the project root 
directory. In the <code>dist</code> directory a jar will be built, you can 
run that jar. 
</p>
<p>
There are a couple of sample programs in the <b>samples</b> folder. These are 
fully functional program that gets compiled and works. The language came from
a modification of the one in <i>A model of cooperative thread</i> by Martin 
Abadi, Microsoft research. We added locks and atomic sections, you find how to 
use those in the samples. 
</p>


