package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.DecimalFormat;

public class FloatBasedDecimalField extends BaseDecimalField<Float> {

	public FloatBasedDecimalField(int width, String label, int decimalPlaces) {
		super(width, label, new DecimalFormat(decimalPlaces, true), decimalPlaces, true);
	}

	public FloatBasedDecimalField(int width, String label, int decimalPlaces, boolean withSign) {
		super(width, label, new DecimalFormat(decimalPlaces, withSign), decimalPlaces, withSign);
	}

}
