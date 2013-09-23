-- Upload the sample data
copyFromLocal src/test/resources/cobol-dump.txt	/cobol-dump-parser-sample.txt;

-- Register the JARs
register cobol-dump-parser-0.0.1-SNAPSHOT-tests.jar;
register target/cobol-dump-parser-0.0.1-SNAPSHOT-custom-jar-with-dependencies.jar;

-- Load the data (only the field 'Code' and 'Description')
records =
  LOAD '/cobol-dump-parser-sample.txt'
  USING ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc(
    'ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig',
    'Code,Description');

DUMP records;
