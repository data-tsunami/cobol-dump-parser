package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.Format;

public class StringField extends Field<String> {

	public StringField(int cantidadLugares, String label) {
		super(cantidadLugares, label, Format.DEFAULT_FORMAT);
	}

}
