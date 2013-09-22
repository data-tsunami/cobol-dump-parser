package ar.com.datatsunami.bigdata.cobol.linehandler;

import static org.junit.Assert.assertEquals;

import org.apache.hadoop.io.Text;
import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.CobolDumpParser;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserForHadoop;
import ar.com.datatsunami.bigdata.cobol.CobolDumpParserTestUtils;
import ar.com.datatsunami.bigdata.cobol.ParserException;

public class PositionalLineHandlerForHadoopTest {

	@Test
	public void testUsingLineHandler() throws ParserException {

		PositionalLineHandler lineHandler = new PositionalLineHandler();
		CobolDumpParser cp = new CobolDumpParserForHadoop(lineHandler);
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cp);

		Text output = new Text();

		lineHandler.prepareText(CobolDumpParserTestUtils.line1AsText);

		/*
		 * Check using LineHandler
		 */

		// cp.add(new Field<String>(15, "Description"));
		lineHandler.copyValue(cp.getFieldIndexFromFieldName("Description"), output);
		assertEquals("Film 8mm x 7mm", output.toString().trim());

		// cp.add(new Field<String>(5, "Code"));
		lineHandler.copyValue(cp.getFieldIndexFromFieldName("Code"), output);
		assertEquals("PTRYY", output.toString().trim());

		assertEquals("PTRYY", lineHandler.getValueForField(cp.getFieldIndexFromFieldName("Code")).trim());

	}

	@Test
	public void testUsingCobolDumpParserForHadoop() throws ParserException {

		CobolDumpParserForHadoop cp = new CobolDumpParserForHadoop(new PositionalLineHandler());
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cp);

		/*
		 * Lets simulate a mapper function
		 */

		// setup() { ... }

		Text OUTPUT_KEY = new Text();
		Text OUTPUT_VALUE = new Text();

		int cobolFieldWithKey = cp.getFieldIndexFromFieldName("Code");
		int cobolFieldWithValue = cp.getFieldIndexFromFieldName("Description");

		// map() { ... }

		Text inputValue = CobolDumpParserTestUtils.line1AsText;

		cp.copyValueToText(inputValue, cobolFieldWithKey, OUTPUT_KEY);
		cp.copyValueToText(inputValue, cobolFieldWithValue, OUTPUT_VALUE);

		// Check...
		assertEquals("PTRYY", OUTPUT_KEY.toString().trim());
		assertEquals("Film 8mm x 7mm", OUTPUT_VALUE.toString().trim());

		/*
		 * Now check non-String fields...
		 */

		Text outputs[] = new Text[] { new Text(), new Text() };

		cp.copyValuesToTextArray(
				CobolDumpParserTestUtils.line1AsText,
				new int[] { cp.getFieldIndexFromFieldName("ItemID"), cp.getFieldIndexFromFieldName("Price") },
				outputs);

		// Long
		assertEquals("002541", outputs[0].toString().trim());

		// Decimal
		assertEquals("0007199+", outputs[1].toString().trim());

	}

}
