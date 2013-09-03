package ar.com.datatsunami.bigdata.cobol;

import ar.com.datatsunami.bigdata.cobol.format.Format;
import ar.com.datatsunami.bigdata.cobol.format.StringFormat;

/**
 * Represents a field to parse.
 * 
 * A Field has an associated Format instance and the count of spaces it uses.
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
	Format<I> format;

	public Field(int cantidadLugares, String label) {
		this.width = cantidadLugares;
		this.label = label;
		this.format = null;
	}

	public Field(int cantidadLugares, String label, Format<I> format) {
		this.width = cantidadLugares;
		this.label = label;
		this.format = format;
	}

	public String genRegex() {
		return "(.{" + width + "})";
	}

	public String toString() {
		String formatName = this.format != null ? this.format.getClass().getSimpleName()
				: StringFormat.DEFAULT.getClass().getSimpleName();
		return formatName + "[" + this.width + "] (" + this.label + ")";
	}
}
