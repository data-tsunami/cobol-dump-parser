package ar.com.datatsunami.bigdata.cobol.linehandler;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserTestUtils;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class RegexLineHandlerTest {

	@Test
	@SuppressWarnings("deprecation")
	public void parseLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new RegexLineHandler());
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cp);
		CobolDumpParserTestUtils.parse(cp);
	}

	@Test(expected = IllegalArgumentException.class)
	@SuppressWarnings("deprecation")
	public void parseShortLine() throws ParserException {
		CobolDumpParser cp = new CobolDumpParser(new RegexLineHandler());
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cp);
		cp.getValuesAsMap(CobolDumpParserTestUtils.shortLine);
	}

}
