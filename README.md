cobol-dump-parser
=================

Parser for Cobol dumps in fixed-line-width format

## How to build

To build using Maven:

    $ mvn clean package

To install to the local repository:

    $ mvn install

## Performance

Using `CobolDumpParser.getItemsWithLabels()` I've processed a dump with 45.000.000 lines (> 8GB) and took 6 minutes.

Using `PositionalLineHandler` and `CobolDumpParser.getItemsValues()` I've processed the same dump (45.000.000 lines) and took 3 minutes 45 seconds.

TODO: check performance of `copyItemsValuesByFieldIndexes(Text text, int[] fieldIndexes, Text[] out)`. That method uses Text's byte[] buffers, avoiding converting to String.

## Example

If you have the following Cobol structure:

    ITEMID PIC 9(6).
    CODE PIC X(5).
    DESCRIPTION PIC X(15).
    PRICE PIC S9(5)V99.
    INDEX PIC 9(3)V999.

and you dump the data to a plain file, each line will have 40 characters. For example:

    002541PTRYYFilm 8mm x 7mm 0007199+001500

To build a parser for that structure:

    CobolDumpParser cp = new CobolDumpParser();
    cp.add(new Field<Long>(6, "ItemID", new LongFormat()));
    cp.add(new Field<String>(5, "Code"));
    cp.add(new Field<String>(15, "Description"));
    cp.add(new Field<Float>(8, "Price", new DecimalFormat(2, true)));
    cp.add(new Field<Float>(6, "Index", new DecimalFormat(3, false)));

To parse a line:

    Map<String, Object> fields = cp.getItemsWithLabels(line);

To get the data:

    fields.get("ItemID")      -> returns a Long
    fields.get("Description") -> returns a String
    fields.get("Price")       -> returns a Float

To avoid converting to String(), and use Hadoop's Text instances:

	// Taken from src/test/java/ar/com/datatsunami/bigdata/cobol/linehandler/PositionalLineHandlerTest.java

	// setup() { ... }
	Text outputKeyAndValue[] = new Text[] { new Text(), new Text() };

	int fieldIndexes[] = new int[] { cp.getFieldIndexFromFieldName("Code"),
		cp.getFieldIndexFromFieldName("Description") };

	// map() { ... }

	Text inputValue = LineHandlerTestUtils.line1AsText;

	cp.copyItemsValuesByFieldIndexes(inputValue, fieldIndexes, outputKeyAndValue);

	assertEquals("PTRYY", outputKeyAndValue[0].toString().trim());
	assertEquals("Film 8mm x 7mm", outputKeyAndValue[1].toString().trim());

TODO: add example with `PositionalLineHandler` + `CobolDumpParser.getItemsValues()`

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

