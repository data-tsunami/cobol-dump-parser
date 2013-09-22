package ar.com.datatsunami.bigdata.cobol.field.pig;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represent the fields types/names/length to access a field from Pig.
 * 
 * @author Horacio G. de Oro
 * 
 */
public class PigSchema {

	public static String LONG = "long";
	public static String INT = "int";
	public static String CHARARRAY = "chararray";

	private List<PigSchemaEntry> entries = new ArrayList<PigSchemaEntry>();

	/**
	 * Constructor
	 */
	public PigSchema() {
	}

	/**
	 * Constructor
	 */
	public PigSchema(String type, String name, int length, int offset) {
		this.entries.add(new PigSchemaEntry(type, name, length, offset));
	}

	public void add(String type, String name, int length, int offset) {
		this.entries.add(new PigSchemaEntry(type, name, length, offset));
	}

	/**
	 * Add to the SBs the references.
	 * 
	 * 'startPos' is 1 based, and the generated values are to be used with
	 * 'FixedWidthLoader'.
	 * 
	 */
	public void fillStringBuffers(int startPos, StringBuffer sbColumnSpec, StringBuffer sbSchema,
			boolean isTheFirst) {

		for (PigSchemaEntry entry : this.entries) {
			entry.fillStringBuffers(startPos, sbColumnSpec, sbSchema, isTheFirst);
			isTheFirst = false;
		}

	}

	/*
	 * Getter
	 */
	public List<PigSchemaEntry> getEntries() {
		return Collections.unmodifiableList(this.entries);
	}

}
