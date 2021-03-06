package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.converter.LongConverter;
import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;

/**
 * Represents an unsigned long.
 * 
 * // FIXME: maybe should be renamed to UnsignedLong?
 * 
 * @author Horacio G. de Oro
 * 
 */
public class LongField extends Field<Long, LongConverter> {

	public LongField(int cantidadLugares, String label) {
		super(cantidadLugares, label, new LongConverter());
	}

	public LongField(int cantidadLugares, String label, Long valueForEmpty) {
		super(cantidadLugares, label, new LongConverter(valueForEmpty));
	}

	@Override
	public PigSchema getPigSchema() {
		return new PigSchema(PigSchema.LONG, label.toLowerCase(), this.width, 0);
	}

}
