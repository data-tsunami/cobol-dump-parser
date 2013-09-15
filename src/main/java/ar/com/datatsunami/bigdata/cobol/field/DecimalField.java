package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.DecimalFormat;

public class DecimalField extends Field<Float> {

	public DecimalField(int cantidadLugares, String label, int decimalPlaces) {
		super(cantidadLugares, label, new DecimalFormat(decimalPlaces));
	}

	public DecimalField(int cantidadLugares, String label, int decimalPlaces, boolean withSign) {
		super(cantidadLugares, label, new DecimalFormat(decimalPlaces, withSign));
	}

}
