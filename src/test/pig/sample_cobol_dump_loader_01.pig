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
  USING ar.com.datatsunami.pig.CobolDumpLoader('ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig', 'Code')
  AS (code:chararray);

--
-- Check the data
--

ILLUSTRATE records;
