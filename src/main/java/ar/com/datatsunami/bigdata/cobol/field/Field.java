package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.converter.CobolFieldToJavaConverter;
import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;

/**
 * Represents a field to parse.
 * 
 * A Field has an associated converter instance and the count of spaces it uses.
 * The converter represents some way to interpret the text, and allows converting
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
	 * The associated converter.
	 */
	public final CobolFieldToJavaConverter<I> converter;

	/**
	 * Count of errors associated with this field
	 */
	protected long errorCount = 0;

	/**
	 * Returns a schema object used to map the field to Pig field (or fields).
	 * 
	 * A field here can result in many fields of Pig, for example:
	 * <code>FixedWidthLoader</code>, which is expresed in at least 2 Pig
	 * fields: one for the integer part, other for the decimal part (and,
	 * optionally, one thirth Pig field for the sign).
	 * 
	 * This is a workaround to avoid implementing a custom UDF, and use
	 * <code>FixedWidthLoader</code>. This method returns a PigSchema instance,
	 * used to configure <code>FixedWidthLoader</code>.
	 * 
	 * @return
	 */
	public abstract PigSchema getPigSchema();

	public Field(int cantidadLugares, String label, CobolFieldToJavaConverter<I> converter) {
		this.width = cantidadLugares;
		this.label = label;
		this.converter = converter;
	}

	public String genRegex() {
		return "(.{" + width + "})";
	}

	public String toString() {
		String converterName = this.converter.getClass().getSimpleName();
		return converterName + "[" + this.width + "] (" + this.label + ")";
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

}
