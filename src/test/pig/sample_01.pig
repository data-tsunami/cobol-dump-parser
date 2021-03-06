/*
 *
 * To run this Pig script you need run 'package' and build the jar for the tests:
 *
 * $ mvn package jar:test-jar
 *
 * Setup environment variables:
 *
 * $ export HADOOP_CONF_DIR=/path/to/hadoop/conf
 * $ export PATH=/path/to/pig-0.11.1/bin:$PATH
 *
 * And run Pig:
 *
 * $ pig --debug WARN -f src/test/pig/sample_01.pig
 *
 */

--
-- Upload the sample data
--

copyFromLocal src/test/resources/cobol-dump.txt	/cobol-dump-parser-sample.txt;

--
-- Register the JARs
--

register cobol-dump-parser-0.0.1-SNAPSHOT-tests.jar;
register target/cobol-dump-parser-0.0.1-SNAPSHOT-custom-jar-with-dependencies.jar;

--
-- Then, load the records, using 'ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc' as the UDF.
-- You must specify 2 parameters:
--  1) The static Java method that returns the CobolDumpParser instance to use
--  2) The indexes of the fields to use (0 based) or the field names as defined in CobolDumpParser
--

records =
  LOAD '/cobol-dump-parser-sample.txt'
  USING ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc(
    'ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig',
    'ItemID,Code,Description,Price,Index');

--
-- Check the data
--

ILLUSTRATE records;

/*
 * ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 * | records     | itemid:long   | code:chararray   | description:chararray   | price:long   | price_decimal:long   | price_sign:chararray   | index:long   | index_decimal:long   | 
 * ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 * |             | 2541          | PTRYY            | Film 8mm x 7mm          | 71           | 99                   | +                      | 1            | 500                  | 
 * ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 */ 

bag_of_floats = FOREACH records GENERATE ar.com.datatsunami.pig.UnsignedDecimalToFloat(price, price_decimal, 2);

ILLUSTRATE bag_of_floats;
