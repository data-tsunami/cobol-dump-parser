package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.Field;
import ar.com.datatsunami.bigdata.cobol.format.LongFormat;

public class LongField extends Field<Long> {

	public LongField(int cantidadLugares, String label) {
		super(cantidadLugares, label, new LongFormat());
	}

	public LongField(int cantidadLugares, String label, Long valueForEmpty) {
		super(cantidadLugares, label, new LongFormat(valueForEmpty));
	}

}
