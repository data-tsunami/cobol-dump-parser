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

		/*
		 * Check using LineHandler
		 */

		// cp.add(new Field<String>(15, "Description"));
		lineHandler.copyValue(cp.getFieldIndexFromFieldName("Description"), output);
		assertEquals("Film 8mm x 7mm", output.toString().trim());

		// cp.add(new Field<String>(5, "Code"));
		lineHandler.copyValue(cp.getFieldIndexFromFieldName("Code"), output);
		assertEquals("PTRYY", output.toString().trim());

		/*
		 * Check using CobolDumpParser
		 */

		// Lets simulate a mapper function

		// setup() { ... }
		Text outputKeyAndValue[] = new Text[] { new Text(), new Text() };

		int fieldIndexes[] = new int[] { cp.getFieldIndexFromFieldName("Code"),
				cp.getFieldIndexFromFieldName("Description") };

		// map() { ... }

		Text inputValue = LineHandlerTestUtils.line1AsText;

		cp.copyItemsValuesByFieldIndexes(inputValue, fieldIndexes, outputKeyAndValue);

		assertEquals("PTRYY", outputKeyAndValue[0].toString().trim());
		assertEquals("Film 8mm x 7mm", outputKeyAndValue[1].toString().trim());

		/*
		 * Now check non-String fields...
		 */

		Text outputs[] = new Text[] { new Text(), new Text() };

		cp.copyItemsValuesByFieldIndexes(
				LineHandlerTestUtils.line1AsText,
				new int[] { cp.getFieldIndexFromFieldName("ItemID"), cp.getFieldIndexFromFieldName("Price") },
				outputs);

		// Long
		assertEquals("002541", outputs[0].toString().trim());
		// Decimal
		assertEquals("0007199+", outputs[1].toString().trim());

	}
}
