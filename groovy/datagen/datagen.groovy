#!/usr/bin/env groovy

import java.io.File;

if (0 == this.args.length) {
    println("Usage: groovy datagen.groovy <number of scripts>\n" +
            "input templates in inputFiles/, data files in dataFiles/\n" +
            "output written to outputFiles/\n");
    System.exit(0);
}

Integer dataCount = Integer.parseInt(this.args[0])

inputFiles = new File('inputFiles').listFiles()

dataFiles = [:]
dataFilePointers = [:]
dataFilesVisitedThisIteration = []

rand = new Random()

index = 0

dataCount.times {
    index = it
    File file = inputFiles[index % inputFiles.size()]
    outputFile = new File("outputFiles/" + file.name + "-" + index);
    outputFile.createNewFile()
    file.eachLine {line ->
        if (line =~ /\$\{/) {
            outputFile.append(
                    line.replaceAll(/\$\{[^\}]*\}/,
                            {composeReplacement(it)})
            )
        } else {
            outputFile.append(line)
        }
        outputFile.append("\n")
    }
    dataFilesVisitedThisIteration.each {
        dataFilePointers[it] =
            (dataFilePointers[it] + 1) % dataFiles[it].size()
    }
    dataFilesVisitedThisIteration = []
}

def composeReplacement(x) {
    cmd = x[2..-2].trim()
    args = cmd.split(/[ ,\/]/).collect {it}.reverse()
    return dispatch("", args as List)
}

def dispatch(String val, List args) {
    if (args.size() == 0) {
        return val
    }
    return "compose_${args.pop()}"(val, args)
}

def compose_index(String val, List args) {
    return dispatch("${index}", args)
}

def compose_file(String val, List args) {
    dataFileName = args.pop()

    if (dataFiles[dataFileName] == null) {
        dataFiles[dataFileName] = new File("dataFiles/" + dataFileName).readLines()
        dataFilePointers[dataFileName] = 0;
    }

    dataFilesVisitedThisIteration << dataFileName
    dataFile = dataFiles[dataFileName]
    dataLine = (String) dataFile[dataFilePointers[dataFileName]]

    return dispatch(dataLine, args)
}

def compose_field(String val, List args) {
    j = Integer.parseInt(args.pop());
    return dispatch(val.split(/,/)[j - 1], args)
}

def compose_random(String val, List args) {
    dispatch("${rand.nextInt()}", args)
}

def compose_length(String val, List args) {
    len = Integer.parseInt(args.pop())

    pad = "space"
    if (args.size() > 0) {
        n = args.pop();
        if (!(n =~ /(?i)pad/)) {
            args.push(n);
        } else {
            pad = args.pop() as String;
        }
    }

    while ( val.length() < len ) {
        if ( pad =~ /(?i)space/ ) {
            val = " ${val}"
        } else if (pad =~ /(?i)random/) {
            val = "${rand.nextInt()}${val}"
        } else {
            val = "${pad}${val}"
        }
    }

    if (val.length() > len) {
        val = val.reverse().substring(0,len).reverse()
    }

    dispatch("${val}", args)
}