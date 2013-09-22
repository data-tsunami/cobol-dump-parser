package ar.com.datatsunami.bigdata.cobol;

import static org.junit.Assert.*;

import org.apache.hadoop.io.Text;
import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.linehandler.PositionalLineHandler;

public class CobolDumpParserForHadoopTest {

	final CobolDumpParserForHadoop cp = new CobolDumpParserForHadoop(new PositionalLineHandler());
	final int cobolFieldWithCode;
	final int cobolFieldWithDescription;
	final int cobolFieldWithItemId;

	final int keyAndValueArray[];

	final String line1Code = "PTRYY";
	final String line1Description = "Film 8mm x 7mm";
	final Long line1itemId = Long.valueOf(2541);

	public CobolDumpParserForHadoopTest() {
		CobolDumpParserTestUtils.addFieldsToCobolDumpParser(cp);
		cobolFieldWithCode = cp.getFieldIndexFromFieldName("Code");
		cobolFieldWithDescription = cp.getFieldIndexFromFieldName("Description");
		cobolFieldWithItemId = cp.getFieldIndexFromFieldName("ItemID");
		keyAndValueArray = new int[] { cobolFieldWithCode, cobolFieldWithDescription };
	}

	@Test
	public void testGetValuesFromText() throws ParserException {
		Object res[] = cp.getValuesFromText(CobolDumpParserTestUtils.line1AsText, keyAndValueArray);
		assertEquals(line1Code, res[0]);
		assertEquals(line1Description, res[1]);

		assertArrayEquals(new Object[] { line1itemId },
				cp.getValuesFromText(CobolDumpParserTestUtils.line1AsText, new int[] { cobolFieldWithItemId }));
	}

	@Test
	public void testGetValueFromText() throws ParserException {
		assertEquals(line1Code, cp.getValueFromText(CobolDumpParserTestUtils.line1AsText, cobolFieldWithCode));
		assertEquals(line1itemId, cp.getValueFromText(CobolDumpParserTestUtils.line1AsText, cobolFieldWithItemId));
	}

	@Test
	public void testCopyValuesToTextArray() throws ParserException {

		Text out[] = new Text[] { new Text(), new Text() };
		cp.copyValuesToTextArray(CobolDumpParserTestUtils.line1AsText, keyAndValueArray, out);
		assertEquals(line1Code, out[0].toString().trim());
		assertEquals(line1Description, out[1].toString().trim());
	}

	@Test
	public void testCopyValueToText() throws ParserException {
		Text out = new Text();

		cp.copyValueToText(CobolDumpParserTestUtils.line1AsText, cobolFieldWithDescription, out);
		assertEquals(line1Description, out.toString().trim());

		cp.copyValueToText(CobolDumpParserTestUtils.line1AsText, cobolFieldWithItemId, out);
		assertEquals("002541", out.toString().trim());
	}

	@Test
	public void testCopyValuesToObjectArray() throws ParserException {
		Object out[] = new Object[2];
		cp.copyValuesToObjectArray(CobolDumpParserTestUtils.line1AsText, keyAndValueArray, out);
		assertEquals(line1Code, out[0]);
		assertEquals(line1Description, out[1]);

	}
}
