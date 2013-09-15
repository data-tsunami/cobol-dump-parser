package ar.com.datatsunami.bigdata.cobol.linehandler;

import java.util.List;

import ar.com.datatsunami.bigdata.cobol.Field;

/**
 * Handles the parsing and extracting of data from a line using substring()
 * 
 * @author Horacio G. de Oro
 * 
 */
public class PositionalLineHandler implements LineHandler {

	List<Field<?>> fields;

	int lineWidth = 0;

	int[] startPositions = null;

	int[] fieldSizes = null;

	String line = null;

	public PositionalLineHandler() {
	}

	public PositionalLineHandler(List<Field<?>> fields) {
		this.fields = fields;
	}

	@Override
	public void prepareLine(String line) {
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

		/*
		 * Now check if the line is valid. To make this check fast, only the
		 * line width is checked.
		 */
		if (line.length() != lineWidth) {
			String msg = "Line didn't matched!\n";
			msg += " - Line: '" + line + "'\n";
			msg += " - Line width: '" + line.length() + "'\n";
			msg += " - Expected line width: " + this.lineWidth + "\n";
			throw new IllegalArgumentException(msg);
		}

		this.line = line;
	}

	@Override
	public String getValueForField(int field) {
		if (field < 0 || field >= this.fieldSizes.length)
			throw new IllegalArgumentException("Invalid field index: " + field);
		// This creates a new String object. We should look for some way to
		// avoid creating instances, and inject the data directly to the
		// instance of Hadoop's writeables
		return this.line.substring(startPositions[field], startPositions[field] + fieldSizes[field]);
	}

	@Override
	public void setFields(List<Field<?>> fields) {
		this.fields = fields;
	}

}
