package ar.com.datatsunami.bigdata.cobol.linehandler;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class PositionalLineHandlerTest {

	@Test
	public void parseLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		LineHandlerTestUtils.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseShortLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		cp.getValues(LineHandlerTestUtils.shortLine, new String[] { "ItemID", "Price" });
	}

	@Test
	public void testPig() throws ParserException {

		PositionalLineHandler plh = new PositionalLineHandler();
		CobolDumpParser cp = new CobolDumpParser(plh);
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		LineHandlerTestUtils.parse(cp);

		// records = LOAD '/fome-fixed-width-file.txt'
		// USING ar.com.datatsunami.pig.FixedWidthLoader(
		// '19-26,45-55', '',
		// 'id:long,monto:long'
		// );

		// cp.add(new LongField(6, "ItemID")); // 1-6
		// cp.add(new StringField(5, "Code")); // 7-11
		// cp.add(new StringField(15, "Description")); // 12-26
		// cp.add(new DecimalField(8, "Price", 2, true));
		// ........................ 27-34 (sin signo) / 27-33 (con signo)
		// cp.add(new DecimalField(6, "Index", 3, false)); // 35-40 (sin signo)
		// cp.add(new LongBasedDecimalField(4, "Value", 1, false));
		// ........................ 41-44 (sin signo)

		// LongField
		String spec[] = plh.getFixedWidthLoaderSpec(new int[] { 0 });
		assertEquals("1-6", spec[0]);
		assertEquals("itemid:long", spec[1]);

		// StringField
		spec = plh.getFixedWidthLoaderSpec(new int[] { 1 });
		assertEquals("7-11", spec[0]);
		assertEquals("code:chararray", spec[1]);

		// LongField + StringField
		spec = plh.getFixedWidthLoaderSpec(new int[] { 0, 1 });
		assertEquals("1-6,7-11", spec[0]);
		assertEquals("itemid:long,code:chararray", spec[1]);

		// LongBasedDecimalField
		spec = plh.getFixedWidthLoaderSpec(new int[] { 5 });
		assertEquals("41-43,44-44", spec[0]);
		assertEquals("value:long,value_decimal:long", spec[1]);

		// LongField + StringField + LongBasedDecimalField
		spec = plh.getFixedWidthLoaderSpec(new int[] { 0, 1, 5 });
		assertEquals("1-6,7-11,41-43,44-44", spec[0]);
		assertEquals("itemid:long,code:chararray,value:long,value_decimal:long", spec[1]);

		spec = plh.getFixedWidthLoaderSpec(cp.getFieldIndexesFromNames(new String[] { "Code", "ItemID",
				"Value" }));
		assertEquals("7-11,1-6,41-43,44-44", spec[0]);
		assertEquals("code:chararray,itemid:long,value:long,value_decimal:long", spec[1]);

		// with sign
		// cp.add(new DecimalField(8, "Price", 2, true));
		spec = plh.getFixedWidthLoaderSpec(new int[] { 3 });
		assertEquals("27-31,32-33,34-34", spec[0]);
		assertEquals("price:long,price_decimal:long,price_sign:chararray", spec[1]);

		// without sign
		// cp.add(new DecimalField(6, "Index", 3, false));
		spec = plh.getFixedWidthLoaderSpec(new int[] { 4 });
		assertEquals("35-37,38-40", spec[0]);
		assertEquals("index:long,index_decimal:long", spec[1]);

	}
}
