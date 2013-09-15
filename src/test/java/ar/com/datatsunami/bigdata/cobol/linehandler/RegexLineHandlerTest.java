package ar.com.datatsunami.bigdata.cobol.linehandler;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class RegexLineHandlerTest {

	@Test
	public void parseLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser();
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		LineHandlerTestUtils.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	public void parseShortLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser();
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		cp.getItemsWithLabels(LineHandlerTestUtils.shortLine);
	}

}
