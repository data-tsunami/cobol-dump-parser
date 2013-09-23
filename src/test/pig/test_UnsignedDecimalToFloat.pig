copyFromLocal src/test/resources/cobol-dump.txt	/cobol-dump-parser-sample.txt;
register cobol-dump-parser-0.0.1-SNAPSHOT-tests.jar;
register target/cobol-dump-parser-0.0.1-SNAPSHOT-custom-jar-with-dependencies.jar;
records =
  LOAD '/cobol-dump-parser-sample.txt'
  USING ar.com.datatsunami.pig.FixedWidthLoaderByStaticFunc(
    'ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig',
    'ItemID,Code,Description,Price,Index');

ILLUSTRATE records;

-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- | records     | itemid:long   | code:chararray   | description:chararray   | price:long   | price_decimal:long   | price_sign:chararray   | index:long   | index_decimal:long   | 
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- |             | 0             | PROM1            | Discount u$s10          | 10           | 0                    | -                      | 0            | 0                    | 
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

bag_of_floats = FOREACH records
	GENERATE ar.com.datatsunami.pig.UnsignedDecimalToFloat(price, price_decimal, 2);

ILLUSTRATE bag_of_floats;

-- ---------------------------------
-- | bag_of_floats     | :float    | 
-- ---------------------------------
-- |                   | 10.0      | 
-- ---------------------------------

DUMP bag_of_floats;

-- (71.99)
-- (14.99)
-- (20.99)
-- (10.0)
-- (0.0)
