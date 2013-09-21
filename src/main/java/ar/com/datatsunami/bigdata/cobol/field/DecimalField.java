package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.DecimalFormat;

public class DecimalField extends Field<Float> {

	public DecimalField(int cantidadLugares, String label, int decimalPlaces) {
		super(cantidadLugares, label, new DecimalFormat(decimalPlaces));
	}

	public DecimalField(int cantidadLugares, String label, int decimalPlaces, boolean withSign) {
		super(cantidadLugares, label, new DecimalFormat(decimalPlaces, withSign));
	}

	@Override
	public String getPigType() {
		/*
		 * This should return 'float'... But since we are not using a custom Pig
		 * Loader, it's only possible to use that number as a long.
		 * 
		 * // FIXME: add support for the SIGN!
		 */
		return "long";
	}

	public int getEndFieldOffsetForPig() {
		if (((DecimalFormat) this.format).withSign)
			return 1;
		else
			return 0;
	}

}
