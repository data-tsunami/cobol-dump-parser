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

		String spec[] = plh.getFixedWidthLoaderSpec(new int[] { 0 });
		assertEquals("1-6", spec[0]);
		assertEquals("ItemID:long", spec[1]);

		spec = plh.getFixedWidthLoaderSpec(new int[] { 1 });
		assertEquals("7-11", spec[0]);
		assertEquals("Code:chararray", spec[1]);

		spec = plh.getFixedWidthLoaderSpec(new int[] { 0, 1 });
		assertEquals("1-6,7-11", spec[0]);
		assertEquals("ItemID:long,Code:chararray", spec[1]);

		spec = plh.getFixedWidthLoaderSpec(new int[] { 0, 1, 5 });
		assertEquals("1-6,7-11,41-44", spec[0]);
		assertEquals("ItemID:long,Code:chararray,Value:long", spec[1]);

		spec = plh.getFixedWidthLoaderSpec(cp.getFieldIndexesFromNames(new String[] { "Code", "ItemID",
				"Value" }));
		assertEquals("7-11,1-6,41-44", spec[0]);
		assertEquals("Code:chararray,ItemID:long,Value:long", spec[1]);

	}
}
