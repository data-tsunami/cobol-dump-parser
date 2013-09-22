cobol-dump-parser
=================

Parser for Cobol dumps in fixed-line-width format

## How to build

To build using Maven:

    $ mvn clean package

To install to the local repository:

    $ mvn install


## Example

Suppose you have the following Cobol structure:

```cobol
000110   ITEMID      PIC 9(6).
000120   CODE        PIC X(5).
000130   DESCRIPTION PIC X(15).
000140   PRICE       PIC S9(5)V99.
000150   INDEX       PIC 9(3)V999.
```

and you dump the data to a plain file, one line per Cobol record, fixed width without separator. Each line will have 40 characters, for example:

    002541PTRYYFilm 8mm x 7mm 0007199+001500

To read that line, you need to build a parser instance and populate the parser with the fields:

```java
// Create a parser instance
CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());

// Populate the fields
cp.add(new LongField(6, "ItemID"));
cp.add(new StringField(5, "Code"));
cp.add(new StringField(15, "Description"));
cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
cp.add(new FloatBasedDecimalField(6, "Index", 3, false));
```

To parse a line and receive a Map, with the field name as keys and the Java object as values, you can use `cp.getValuesAsMap()`:

```java
// Parse the line and get the values
Map<String, Object> map = cp.getValuesAsMap(line);

// Print the values
System.out.println(" + The item ID is: " + map.get("ItemID"));
System.out.println(" + The code is: " + map.get("Code"));
```

You can see this and other examples in [SimpleTestFromFile.java](src/test/java/ar/com/datatsunami/bigdata/cobol/SimpleTestFromFile.java).

<!--

## Hadoop

To avoid converting to String(), and use Hadoop's Text instances:

	// Taken from src/test/java/ar/com/datatsunami/bigdata/cobol/linehandler/PositionalLineHandlerForHadoopTest.java

	// setup() { ... }
	
	Text OUTPUT_KEY = new Text();
	Text OUTPUT_VALUE = new Text();
	
	int cobolFieldWithKey = cp.getFieldIndexFromFieldName("Code");
	int cobolFieldWithValue = cp.getFieldIndexFromFieldName("Description");
	
	// map() { ... }
	
	Text inputValue = LineHandlerTestUtils.line1AsText;
	
	cp.copyValueToText(inputValue, cobolFieldWithKey, OUTPUT_KEY);
	cp.copyValueToText(inputValue, cobolFieldWithValue, OUTPUT_VALUE);

TODO: add example with `PositionalLineHandler` + `CobolDumpParser.getItemsValues()`


## Performance

Using `CobolDumpParser.getItemsWithLabels()` I've processed a dump with 45.000.000 lines (> 8GB) and took 6 minutes.

Using `PositionalLineHandler` and `CobolDumpParser.getItemsValues()` I've processed the same dump (45.000.000 lines) and took 3 minutes 45 seconds.

TODO: check performance of `copyItemsValuesByFieldIndexes(Text text, int[] fieldIndexes, Text[] out)`. That method uses Text's byte[] buffers, avoiding converting to String.

-->

# License

    Copyright (C) 2013 - Horacio G. de Oro <hgdeoro@gmail.com>

    This file is part of cobol-dump-parser.

    cobol-dump-parser is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation version 3.

    cobol-dump-parser is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with cobol-dump-parser.  If not, see <http://www.gnu.org/licenses/>.

