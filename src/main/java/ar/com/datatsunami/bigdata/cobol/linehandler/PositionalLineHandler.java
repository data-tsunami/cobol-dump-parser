package ar.com.datatsunami.bigdata.cobol.linehandler;

import java.nio.charset.CharacterCodingException;
import java.util.List;

import org.apache.hadoop.io.Text;

import ar.com.datatsunami.bigdata.cobol.field.Field;

/**
 * Handles the parsing and extracting of data from a line using substring()
 * 
 * @author Horacio G. de Oro
 * 
 */
public class PositionalLineHandler implements LineHandler {

	List<Field<?, ?>> fields;

	int lineWidth = 0;

	int[] startPositions = null;

	int[] fieldSizes = null;

	String line = null;

	Text text = null;

	public PositionalLineHandler() {
	}

	public PositionalLineHandler(List<Field<?, ?>> fields) {
		this.fields = fields;
	}

	private String getLineAsString() {
		if (this.line != null)
			return this.line;
		return this.text.toString();
	}

	private void setupPositions() {
		/*
		 * If the fields' boudaries were not calculated, we do that first
		 */
		if (startPositions == null) {
			startPositions = new int[this.fields.size()];
			fieldSizes = new int[this.fields.size()];
			for (int i = 0; i < startPositions.length; i++) {
				fieldSizes[i] = this.fields.get(i).getWidth();
				startPositions[i] = lineWidth;
				lineWidth += fieldSizes[i];
			}
		}
	}

	@Override
	public void prepareText(Text text) {
		this.setupPositions();

		if (text.getLength() != lineWidth) {
			String msg = "Line length in bytes didn't matched!\n";
			msg += " - Line: '" + text + "'\n";
			msg += " - Line width: '" + text.getLength() + "'\n";
			msg += " - Expected line width: " + this.lineWidth + "\n";
			throw new IllegalArgumentException(msg);
		}

		this.line = null;
		this.text = text;
	}

	@Override
	public void prepareLine(String line) {

		this.setupPositions();

		/*
		 * Now check if the line is valid. To make this check fast, only the
		 * line width is checked.
		 * 
		 * DANGER! This will work for ASCII and encodings that have 1byte for
		 * each character!
		 */
		if (line.length() != lineWidth) {
			String msg = "Line didn't matched!\n";
			msg += " - Line: '" + line + "'\n";
			msg += " - Line width: '" + line.length() + "'\n";
			msg += " - Expected line width: " + this.lineWidth + "\n";
			throw new IllegalArgumentException(msg);
		}

		this.line = line;
		this.text = null;
	}

	@Override
	public String getValueForField(int field) {
		if (field < 0 || field >= this.fieldSizes.length)
			throw new IllegalArgumentException("Invalid field index: " + field);
		// This creates a new String object. We should look for some way to
		// avoid creating instances, and inject the data directly to the
		// instance of Hadoop's writeables

		if (this.text != null) {
			try {
				return Text.decode(this.text.getBytes(), startPositions[field], fieldSizes[field]);
			} catch (CharacterCodingException e) {
				throw new RuntimeException("CharacterCodingException detected "
						+ "when trying to get string for field '" + field + "'");
			}
		} else {
			return this.getLineAsString().substring(startPositions[field],
					startPositions[field] + fieldSizes[field]);
		}
	}

	@Override
	public void setFields(List<Field<?, ?>> fields) {
		this.fields = fields;
	}

	@Override
	public void copyValue(int field, Text output) {
		output.set(this.text.getBytes(), startPositions[field], fieldSizes[field]);
	}

	public String[] getFixedWidthLoaderSpec() {
		this.setupPositions();

		int fieldIndexes[] = new int[this.fields.size()];
		for (int i = 0; i < fieldIndexes.length; i++)
			fieldIndexes[i] = i;
		return getFixedWidthLoaderSpec(fieldIndexes);
	}

	/**
	 * Generate the two required string to instantiate FixedWidthLoader.
	 * 
	 * @param fieldIndexes
	 * @return
	 */
	public String[] getFixedWidthLoaderSpec(int fieldIndexes[]) {
		this.setupPositions();

		// records = LOAD '/fome-fixed-width-file.txt'
		// USING ar.com.datatsunami.pig.FixedWidthLoader(
		// '19-26,45-55', '',
		// 'id:long,monto:long'
		// );

		// String columnSpec, String skipHeaderStr, String schemaStr

		String spec[] = new String[2];
		StringBuffer sbColumnSpec = new StringBuffer();
		StringBuffer sbSchemaStr = new StringBuffer();

		for (int i = 0; i < fieldIndexes.length; i++) {
			Field<?, ?> field = this.fields.get(fieldIndexes[i]);
			field.getPigSchema().fillStringBuffers(this.startPositions[fieldIndexes[i]] + 1, sbColumnSpec,
					sbSchemaStr, (i == 0));
		}

		spec[0] = sbColumnSpec.toString();
		spec[1] = sbSchemaStr.toString();
		return spec;
	}
}
