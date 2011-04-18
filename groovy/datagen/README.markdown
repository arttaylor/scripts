#DataGen

##Intro

datagen.groovy is a simple template engine, created primarily to
synthesize XML messages for some ESB interface testing.  It's
"designed" for easy keyword extensibility by adding a
compose_*keyword* function that takes the current accumulator
(GString) and the rest of the instruction stack as arguments.

##Running

datagen.groovy _n_, where _n_ is the number of output files desired.
The script will iterate over the input files, looping and repeating as
necessary to create _n_ outputs.

##Directory Layout

###inputFiles/

Put input files here.

###outputFiles/

Output files go here.

###dataFiles/

Data files to use for template replacements go here.  One record per line.

##Template language

A substitution directive is included in ${}.  The container and all
contents are replaced by a retrieved and/or constructed value.

The language is a polish-notation-based simple stack language with a
single string accumulator.  There are no variables or user-defined
functions.

The tokens available in the language are:

###index

Required argument: none

Replace the accumulator with the current output file number.  This
increases monotonically during a script run.

###file

Required argument: filename (string, no spaces)

Replaces the accumulator with the contents of the next line in the given filename.

###field

Required argument: field number (integer > 0)

Splits the accumulator as a CSV and replaces it with the 1-indexed
field corresponding to the argument.

###random

Argument: none

Replaces the accumulator with a random integer.  Control length with
_length_ token.

###length

Required argument: length (integer)
Modifier: pad [space|random|padding value]

Modifies the accumulator by ensuring that it conforms to the desired
_length_.  If the length of the accumulator is greater than _length_,
the accumulator is truncated from the left.  If it is shorter than
_length_, it is padded with a space at the front.

The _pad_ token modifier can be used to control the values used for
padding.  "space" will cause a space to be prepended (the default
behavior), and "random" will cause a random number to be prepended.
Any other value is appended literally.
