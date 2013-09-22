package ar.com.datatsunami.bigdata.cobol.field;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;
import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchemaEntry;

public class PigSchemaTest {

	@Test
	public void testLongField() {
		LongField field = new LongField(7, "SomeF");
		PigSchema pigSchema = field.getPigSchema();
		List<PigSchemaEntry> entries = pigSchema.getEntries();
		assertEquals(1, entries.size());

		/*
		 * PigSchemaEntry
		 */
		PigSchemaEntry entry = entries.get(0);
		assertEquals(7, entry.getLength());
		assertEquals("somef", entry.getName());
		assertEquals("long", entry.getType());

		StringBuffer sbColumnSpec = new StringBuffer();
		StringBuffer sbSchema = new StringBuffer();
		entry.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-7", sbColumnSpec.toString());
		assertEquals("somef:long", sbSchema.toString());

		/*
		 * PigSchema
		 */

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		pigSchema.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-7", sbColumnSpec.toString());
		assertEquals("somef:long", sbSchema.toString());

	}

	@Test
	public void testStringField() {
		StringField field = new StringField(7, "SomeF");
		PigSchema pigSchema = field.getPigSchema();
		List<PigSchemaEntry> entries = pigSchema.getEntries();
		assertEquals(1, entries.size());

		/*
		 * PigSchemaEntry
		 */
		PigSchemaEntry entry = entries.get(0);
		assertEquals(7, entry.getLength());
		assertEquals("somef", entry.getName());
		assertEquals("chararray", entry.getType());

		StringBuffer sbColumnSpec = new StringBuffer();
		StringBuffer sbSchema = new StringBuffer();
		entry.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-7", sbColumnSpec.toString());
		assertEquals("somef:chararray", sbSchema.toString());

		/*
		 * PigSchema
		 */

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		pigSchema.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-7", sbColumnSpec.toString());
		assertEquals("somef:chararray", sbSchema.toString());

	}

	@Test
	public void testLongBasedDecimalFieldWithoutSign() {

		LongBasedDecimalField fieldWithoutSign = new LongBasedDecimalField(10, "somEf", 2, false);
		PigSchema pigSchema = fieldWithoutSign.getPigSchema();
		List<PigSchemaEntry> entries = pigSchema.getEntries();
		assertEquals(2, entries.size());

		/*
		 * PigSchemaEntry -> integer part
		 */
		PigSchemaEntry entry1 = entries.get(0);
		assertEquals(8, entry1.getLength());
		assertEquals("somef", entry1.getName());
		assertEquals("long", entry1.getType());

		StringBuffer sbColumnSpec = new StringBuffer();
		StringBuffer sbSchema = new StringBuffer();
		entry1.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-8", sbColumnSpec.toString());
		assertEquals("somef:long", sbSchema.toString());

		/*
		 * PigSchemaEntry -> decimal part
		 */
		PigSchemaEntry entry2 = entries.get(1);
		assertEquals(2, entry2.getLength());
		assertEquals("somef_decimal", entry2.getName());
		assertEquals("long", entry2.getType());

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		entry2.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("9-10", sbColumnSpec.toString());
		assertEquals("somef_decimal:long", sbSchema.toString());

		/*
		 * PigSchema
		 */

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		pigSchema.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-8,9-10", sbColumnSpec.toString());
		assertEquals("somef:long,somef_decimal:long", sbSchema.toString());

	}

	@Test
	public void testLongBasedDecimalFieldWithSign() {

		LongBasedDecimalField fieldWithSign = new LongBasedDecimalField(11, "somEf", 2, true);
		PigSchema pigSchema = fieldWithSign.getPigSchema();
		List<PigSchemaEntry> entries = pigSchema.getEntries();
		assertEquals(3, entries.size());

		/*
		 * PigSchemaEntry [0] -> integer part
		 */
		PigSchemaEntry entry1 = entries.get(0);
		assertEquals(8, entry1.getLength());
		assertEquals("somef", entry1.getName());
		assertEquals("long", entry1.getType());

		StringBuffer sbColumnSpec = new StringBuffer();
		StringBuffer sbSchema = new StringBuffer();
		entry1.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-8", sbColumnSpec.toString());
		assertEquals("somef:long", sbSchema.toString());

		/*
		 * PigSchemaEntry [1] -> decimal part
		 */
		PigSchemaEntry entry2 = entries.get(1);
		assertEquals(2, entry2.getLength());
		assertEquals("somef_decimal", entry2.getName());
		assertEquals("long", entry2.getType());

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		entry2.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("9-10", sbColumnSpec.toString());
		assertEquals("somef_decimal:long", sbSchema.toString());

		/*
		 * PigSchemaEntry [2] -> sign part
		 */
		PigSchemaEntry entry3 = entries.get(2);
		assertEquals(1, entry3.getLength());
		assertEquals("somef_sign", entry3.getName());
		assertEquals("chararray", entry3.getType());

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		entry3.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("11-11", sbColumnSpec.toString());
		assertEquals("somef_sign:chararray", sbSchema.toString());

		/*
		 * PigSchema
		 */

		sbColumnSpec = new StringBuffer();
		sbSchema = new StringBuffer();
		pigSchema.fillStringBuffers(1, sbColumnSpec, sbSchema, true);
		assertEquals("1-8,9-10,11-11", sbColumnSpec.toString());
		assertEquals("somef:long,somef_decimal:long,somef_sign:chararray", sbSchema.toString());

	}

}
