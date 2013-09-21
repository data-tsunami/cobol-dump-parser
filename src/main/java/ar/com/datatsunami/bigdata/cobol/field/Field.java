package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.Format;

/**
 * Represents a field to parse.
 * 
 * A Field has an associated Format instance and the count of spaces it uses.
 * The format represents some way to interpret the text, and allows converting
 * that text to a Java object.
 * 
 * @author Horacio G. de Oro
 * 
 * @param <I>
 */
public abstract class Field<I> {

	/**
	 * Width in characters.
	 */
	public final int width;

	/**
	 * Label to identify the field.
	 */
	public final String label;

	/**
	 * The associated format.
	 */
	public final Format<I> format;

	/**
	 * Count of errors associated with this field
	 */
	protected long errorCount = 0;

	/**
	 * Returns the pig type associated with this field.
	 * 
	 * Some valid values are: int, long, float, double, chararray, bytearray,
	 * boolean, datetime.
	 * 
	 * As a reference, see:
	 * https://pig.apache.org/docs/r0.11.1/basic.html#schema-simple
	 * 
	 * @return
	 */
	public abstract String getPigType();

	public Field(int cantidadLugares, String label, Format<I> format) {
		this.width = cantidadLugares;
		this.label = label;
		this.format = format;
	}

	public String genRegex() {
		return "(.{" + width + "})";
	}

	public String toString() {
		String formatName = this.format.getClass().getSimpleName();
		return formatName + "[" + this.width + "] (" + this.label + ")";
	}

	public int getWidth() {
		return width;
	}

	public void incrErrorCount() {
		this.errorCount++;
	}

	public long getErrorCount() {
		return this.errorCount;
	}

	public int getStartFieldOffsetForPig() {
		return 0;
	}

	public int getEndFieldOffsetForPig() {
		return 0;
	}

}
