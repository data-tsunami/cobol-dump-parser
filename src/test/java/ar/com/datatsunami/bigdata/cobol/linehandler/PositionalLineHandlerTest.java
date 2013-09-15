package ar.com.datatsunami.bigdata.cobol.linehandler;

import static org.junit.Assert.assertEquals;

import org.apache.hadoop.io.Text;
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
		cp.getItemsValues(LineHandlerTestUtils.shortLine, new String[] { "ItemID", "Price" });
	}

	@Test
	public void parseHadoopText() throws ParserException {

		PositionalLineHandler lineHandler = new PositionalLineHandler();
		CobolDumpParser cp = new CobolDumpParser(lineHandler);
		LineHandlerTestUtils.addFieldsToCobolDumpParser(cp);

		Text output = new Text();

		lineHandler.prepareText(LineHandlerTestUtils.line1AsText);

		// cp.add(new Field<String>(15, "Description"));
		lineHandler.copyValue(cp.getFieldIndexFromFieldName("Description"), output);
		assertEquals("Film 8mm x 7mm", output.toString().trim());

		// cp.add(new Field<String>(5, "Code"));
		lineHandler.copyValue(cp.getFieldIndexFromFieldName("Code"), output);
		assertEquals("PTRYY", output.toString().trim());

	}
}
