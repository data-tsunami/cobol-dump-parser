package ar.com.datatsunami.bigdata.cobol;

import static org.junit.Assert.*;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.field.LongField;

public class CobolDumpParserTest {

	@Test()
	public void testDuplicatedFields() {
		CobolDumpParser cobolDumpParser = new CobolDumpParser();
		cobolDumpParser.add(new LongField(6, "ItemID"));

		try {
			cobolDumpParser.add(new LongField(6, "ItemID"));
			fail("Duplicated field added successfuly");
		} catch (RuntimeException e) {
		}

		try {
			cobolDumpParser.add(new LongField(6, "itemid"));
			fail("Duplicated field added successfuly");
		} catch (RuntimeException e) {
		}

		try {
			cobolDumpParser.add(new LongField(6, " itemid"));
			fail("Duplicated field added successfuly");
		} catch (RuntimeException e) {
		}

		try {
			cobolDumpParser.add(new LongField(6, "itemid "));
			fail("Duplicated field added successfuly");
		} catch (RuntimeException e) {
		}

		try {
			cobolDumpParser.add(new LongField(6, " itemid "));
			fail("Duplicated field added successfuly");
		} catch (RuntimeException e) {
		}

		cobolDumpParser.freeze();
	}

}
