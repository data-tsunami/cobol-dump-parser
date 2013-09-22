package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;
import ar.com.datatsunami.bigdata.cobol.format.LongFormat;

/**
 * Represents an unsigned long.
 * 
 * // FIXME: maybe should be renamed to UnsignedLong?
 * 
 * @author Horacio G. de Oro
 * 
 */
public class LongField extends Field<Long> {

	public LongField(int cantidadLugares, String label) {
		super(cantidadLugares, label, new LongFormat());
	}

	public LongField(int cantidadLugares, String label, Long valueForEmpty) {
		super(cantidadLugares, label, new LongFormat(valueForEmpty));
	}

	@Override
	public PigSchema getPigSchema() {
		return new PigSchema(PigSchema.LONG, label.toLowerCase(), this.width, 0);
	}

}
