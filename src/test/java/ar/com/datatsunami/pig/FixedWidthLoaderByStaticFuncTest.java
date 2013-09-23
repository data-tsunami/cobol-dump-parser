package ar.com.datatsunami.pig;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.net.URISyntaxException;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserForHadoop;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserTestUtils;
import ar.com.datatsunami.bigdata.cobol.field.FloatBasedDecimalField;
import ar.com.datatsunami.bigdata.cobol.field.LongField;
import ar.com.datatsunami.bigdata.cobol.field.StringField;
import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class FixedWidthLoaderByStaticFuncTest {

	/**
	 * This method creates a CobolDumpParser. It's public and static by design.
	 * 
	 * This is used in the samples Pig scripts, to instantiate the UDF
	 * <code>FixedWidthLoader</code>.
	 * 
	 */
	public static CobolDumpParser cobolDumpParserFactoryForPig() {
		/*
		 * Create the instance
		 */
		CobolDumpParser cdp = new CobolDumpParser(new PositionalLineHandler());

		/*
		 * Populate the fields
		 */
		cdp.add(new LongField(6, "ItemID"));
		cdp.add(new StringField(5, "Code"));
		cdp.add(new StringField(15, "Description"));
		cdp.add(new FloatBasedDecimalField(8, "Price", 2, true));
		cdp.add(new FloatBasedDecimalField(6, "Index", 3, false));

		return cdp;
	}

	/*
	 * This method isn't used directly by the Java code, but IT IS used for
	 * testing FixedWidthLoaderByStaticFunc()
	 */
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
				"ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig", "0");
		assertEquals("1-6", columnSpecAndSchemaStr[0]);
		assertEquals("itemid:long", columnSpecAndSchemaStr[1]);
	}

	@Test
	public void testFixedWidthLoaderByStaticFuncByFieldNameNotInt() {
		String factory = "ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForPig";
		String columnSpecAndSchemaStr[] = FixedWidthLoaderByStaticFunc.get(factory, "ItemID");
		assertEquals("1-6", columnSpecAndSchemaStr[0]);
		assertEquals("itemid:long", columnSpecAndSchemaStr[1]);

		columnSpecAndSchemaStr = FixedWidthLoaderByStaticFunc.get(factory, "Code");
		assertEquals("7-11", columnSpecAndSchemaStr[0]);
		assertEquals("code:chararray", columnSpecAndSchemaStr[1]);
	}

	@Test
	public void testFixedWidthLoaderByStaticFuncCDPForHadoop() {
		String columnSpecAndSchemaStr[] = FixedWidthLoaderByStaticFunc.get(
				"ar.com.datatsunami.pig.FixedWidthLoaderByStaticFuncTest.cobolDumpParserFactoryForHadoop",
				"1");
		assertEquals("7-11", columnSpecAndSchemaStr[0]);
		assertEquals("code:chararray", columnSpecAndSchemaStr[1]);
	}

	@Test
	public void testCobolDumpParserFactoryForPigWithSampleDump() throws Exception {
		CobolDumpParser cp = cobolDumpParserFactoryForPig();
		BufferedReader reader = this.getBufferedReader();
		cp.getValuesAsMap(reader.readLine());
		cp.getValuesAsMap(reader.readLine());
		cp.getValuesAsMap(reader.readLine());
		cp.getValuesAsMap(reader.readLine());
		cp.getValuesAsMap(reader.readLine());
	}

	private BufferedReader getBufferedReader() throws FileNotFoundException, URISyntaxException {
		ClassLoader cl = Thread.currentThread().getContextClassLoader();
		FileReader fs = new FileReader(new File(cl.getResource("cobol-dump.txt").toURI()));
		return new BufferedReader(fs);
	}

}
