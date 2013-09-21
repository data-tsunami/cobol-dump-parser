package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.LongBasedDecimalFormat;

public class LongBasedDecimalField extends Field<Long> {

	public LongBasedDecimalField(int cantidadLugares, String label, int decimalPlaces) {
		super(cantidadLugares, label, new LongBasedDecimalFormat(decimalPlaces));
	}

	public LongBasedDecimalField(int cantidadLugares, String label, int decimalPlaces, boolean withSign) {
		super(cantidadLugares, label, new LongBasedDecimalFormat(decimalPlaces, withSign));
	}

	@Override
	public String getPigType() {
		return "long";
	}

}
