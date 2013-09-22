package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.DecimalToFloatConverter;

public class FloatBasedDecimalField extends BaseDecimalField<Float> {

	public FloatBasedDecimalField(int width, String label, int decimalPlaces) {
		super(width, label, new DecimalToFloatConverter(decimalPlaces, true), decimalPlaces, true);
	}

	public FloatBasedDecimalField(int width, String label, int decimalPlaces, boolean withSign) {
		super(width, label, new DecimalToFloatConverter(decimalPlaces, withSign), decimalPlaces, withSign);
	}

}
