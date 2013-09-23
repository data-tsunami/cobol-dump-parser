cobol-dump-parser
=================

Parser for accessing *Cobol* dumps in fixed-line-width format from
[Java](http://www.java.com/), [Hadoop](https://hadoop.apache.org/) and [Pig](https://pig.apache.org/).

##### Quick Java example

```java
CobolDumpParser cp = new CobolDumpParser();
cp.add(new LongField(6, "ItemID"));
cp.add(new StringField(5, "Code"));
cp.add(new StringField(15, "Description"));
cp.add(new FloatBasedDecimalField(8, "Price", 2, true));
cp.add(new FloatBasedDecimalField(6, "Index", 3, false));
// Get a 'line' from somewhere and...
Object objects[] = cp.getValues(line, "ItemID", "Price");
Long itemId = (Long) objects[0];
Float price = (Float) objects[1];
```

##### Quick Pig example

```sql
-- Load fields 1 and 3 (1 -> 'Code', 3 -> 'Price')
records =
  LOAD '/cobol-dump-parser-sample.txt'
  USING ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc(
    'ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig',
    'Code,Price');
expensive_products = FILTER records BY price >= 10;
STORE expensive_products INTO '/expensive_products.txt';
```

## How to build

To build using Maven:

    $ mvn package

To install to the local repository (to be used as library from other java projects):

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

## Pig

You can use the definition of the data from Pig. This means: cobol-dump-parser is *not* used to load the data... it's only used to create the strings required to specify the schema to the UDF, but it's very usefull and avoids code duplication.

Imagine you need to access the same data as the previous example with Pig. You will need to create a static method that returns the instance of CobolDumpParser, something like:

```java
package ar.com.datatsunami.pig;

public class FixedWidthLoaderByStaticFuncTest {
	public static CobolDumpParser cobolDumpParserFactoryForPig() {
		CobolDumpParser cdp = new CobolDumpParser(new PositionalLineHandler());
		cdp.add(new LongField(6, "ItemID"));
		cdp.add(new StringField(5, "Code"));
		cdp.add(new StringField(15, "Description"));
		cdp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cdp.add(new FloatBasedDecimalField(6, "Index", 3, false));
		return cdp;
	}
}
```

That method will be called by Pig to generate the UDF to access the data. For example:

```sql
records =
  LOAD '/cobol-dump-parser-sample.txt'
  USING ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc(
    'ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig',
    'Code,Price');

ILLUSTRATE records;
```

Here we use the `ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc()` UDF. The first parameter is
the reference to the package + class + method that generates the CobolDumpParser instance. The second
parameter are the index of required fields (1 and 3 are the fields 'Code' and 'Price').

The output of ILLUSTRATE would be:

```
-------------------------------------------------------------------------------------------------
| records     | code:chararray   | price:long   | price_decimal:long   | price_sign:chararray   | 
-------------------------------------------------------------------------------------------------
|             | MOUSE            | 14           | 99                   | +                      | 
-------------------------------------------------------------------------------------------------
```

To filter the expensive products (assuming 'expensive' as products with price >= 10), we whoud do:

```sql
expensive_products = FILTER records BY price >= 10;
DUMP expensive_products;
```

The output of DUMP would be:

```
(PTRYY,71,99,+)
(MOUSE,14,99,+)
(KBD_X,20,99,+)
(PROM1,10,0,-)
```

Here are the [java](src/test/java/ar/com/datatsunami/pig/FixedWidthLoaderByStaticFuncTest.java)
and [Pig](src/test/pig/sample_03_dump_expensive_products.pig) code used in this example.

### How to run the sample Pig scripts

You will need to run `mvn jar:test-jar` before run the Pig scripts:

```shell
mvn package jar:test-jar
```

Also, you will need to setup the `PATH` to `pig` and `HADOOP_CONF_DIR`.

```shell
export HADOOP_CONF_DIR=/path/to/hadoop/conf
export PATH=/path/to/pig-0.11.1/bin:$PATH
```

Now you can run Pig, from the base directory of the project:

```shell
pig --debug WARN -f src/test/pig/sample_01.pig
```

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

