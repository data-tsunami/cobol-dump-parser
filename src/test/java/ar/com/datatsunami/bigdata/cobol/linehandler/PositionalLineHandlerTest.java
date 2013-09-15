package ar.com.datatsunami.bigdata.cobol.linehandler;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class PositionalLineHandlerTest {

	@Test
	public void parseLineTextWithPositionalLineHandler() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		LineHandlerTestUtils.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseShortLineTextWithPositionalLineHandler() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new PositionalLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		cp.getItemsValues(LineHandlerTestUtils.shortLine, new String[] { "ItemID", "Price" });
	}

}
