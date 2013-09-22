package ar.com.datatsunami.pig;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserForHadoop;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserTestUtils;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class FixedWidthLoaderByStaticFuncTest {

	public static CobolDumpParser cobolDumpParserFactory() {
		final CobolDumpParser cdp = new CobolDumpParser(new PositionalLineHandler());
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cdp);
		return cdp;
	}

	public static CobolDumpParser cobolDumpParserFactoryForHadoop() {
		final CobolDumpParser cdp = new CobolDumpParserForHadoop(new PositionalLineHandler());
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cdp);
		return cdp;
	}

	// cp.add(new LongField(6, "ItemID"));
	// cp.add(new StringField(5, "Code"));
	// cp.add(new StringField(15, "Description"));
	// cp.add(new DecimalField(8, "Price", 2, true));
	// cp.add(new DecimalField(6, "Index", 3, false));
	// cp.add(new LongBasedDecimalField(4, "Value", 1, false));

	@Test
	public void testFixedWidthLoaderByStaticFunc() {
		String columnSpecAndSchemaStr[] = FixedWidthLoaderByStaticFunc.get(
				"ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactory", "0");
		assertEquals("1-6", columnSpecAndSchemaStr[0]);
		assertEquals("itemid:long", columnSpecAndSchemaStr[1]);
	}

	@Test
	public void testFixedWidthLoaderByStaticFuncCDPForHadoop() {
		String columnSpecAndSchemaStr[] = FixedWidthLoaderByStaticFunc.get(
				"ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForHadoop",
				"1");
		assertEquals("7-11", columnSpecAndSchemaStr[0]);
		assertEquals("code:chararray", columnSpecAndSchemaStr[1]);
	}

}
