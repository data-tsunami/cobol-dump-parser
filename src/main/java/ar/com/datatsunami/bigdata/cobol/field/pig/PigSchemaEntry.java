package ar.com.datatsunami.bigdata.cobol.field.pig;

/**
 * Represents a field for Pig
 * 
 * @author Horacio G. de Oro
 * 
 */
public class PigSchemaEntry {

	/** Field type in Pig */
	String type;

	/** Field name in Pig */
	String name;

	/** Field length in the TXT line */
	int length;

	/**
	 * Offset where this field starts, in positions from the start of the Field.
	 * This is only used with fields that must handle decimal places: the
	 * integer part will have an offset of 0, but the deciamal part will have an
	 * offset != 0.
	 * 
	 * For example: '0983928' with 4 integer places and 2 decimal places:
	 * 
	 * 1) integer part: length = 5, offset = 0 -> '09839'
	 * 
	 * 2) decimal part: length = 2, offset = 5 -> '28'
	 * 
	 */
	int offset;

	public PigSchemaEntry(String type, String name, int length, int offset) {
		this.type = type;
		this.name = name;
		this.length = length;
		this.offset = offset;
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

		// records = LOAD '/fome-fixed-width-file.txt'
		// USING ar.com.datatsunami.pig.FixedWidthLoader(
		// '19-26,45-55', '',
		// 'id:long,monto:long'
		// );

		if (!isTheFirst) {
			sbColumnSpec.append(",");
			sbSchema.append(",");
		}

		startPos = startPos + this.offset;

		sbColumnSpec.append("" + startPos + "-" + (startPos + this.length - 1));
		sbSchema.append(this.name + ":" + this.type);
	}

	/*
	 * Getter
	 */
	public String getType() {
		return type;
	}

	/*
	 * Getter
	 */
	public String getName() {
		return name;
	}

	/*
	 * Getter
	 */
	public int getLength() {
		return length;
	}

	/*
	 * Getter
	 */
	public int getOffset() {
		return offset;
	}

}
