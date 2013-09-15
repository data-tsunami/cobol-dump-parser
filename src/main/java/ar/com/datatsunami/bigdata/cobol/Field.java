package ar.com.datatsunami.bigdata.cobol;

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
public class Field<I> {

	/**
	 * Width in characters.
	 */
	int width = -1;

	/**
	 * Label to identify the field.
	 */
	String label = null;

	/**
	 * The associated format.
	 */
	final Format<I> format;

	/**
	 * Count of errors associated with this field
	 */
	long errorCount = 0;

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

}
