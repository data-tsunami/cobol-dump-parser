package ar.com.datatsunami.bigdata.cobol.linehandler;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class RegexLineHandlerTest {

	@Test
	@SuppressWarnings("deprecation")
	public void parseLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new RegexLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		LineHandlerTestUtils.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	@SuppressWarnings("deprecation")
	public void parseShortLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new RegexLineHandler());
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);
		cp.getValuesAsMap(LineHandlerTestUtils.shortLine);
	}

}
