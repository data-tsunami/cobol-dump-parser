package ar.com.datatsunami.bigdata.cobol.linehandler;

import java.util.List;

import ar.com.datatsunami.bigdata.cobol.Field;

public class PositionalLineHandler implements LineHandler {

	List<Field<?>> fields;

	int lineWidth = 0;

	int[] startPositions = null;

	int[] fieldSizes = null;

	String line = null;

	public PositionalLineHandler(List<Field<?>> fields) {
		this.fields = fields;
	}

	@Override
	public void prepareLine(String line) {
		if (startPositions == null) {
			startPositions = new int[this.fields.size()];
			fieldSizes = new int[this.fields.size()];
			for (int i = 0; i < startPositions.length; i++) {
				fieldSizes[i] = this.fields.get(i).getWidth();
				startPositions[i] = lineWidth;
				lineWidth += fieldSizes[i];
			}

		}
		this.line = line;
	}

	@Override
	public String getValueForField(int field) {
		return this.line.substring(startPositions[field], fieldSizes[field]);
	}

}
