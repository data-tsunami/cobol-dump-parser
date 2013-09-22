package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.converter.DecimalToFloatConverter;

public class FloatBasedDecimalField extends BaseDecimalField<Float, DecimalToFloatConverter> {

	public FloatBasedDecimalField(int width, String label, int decimalPlaces) {
		super(width, label, new DecimalToFloatConverter(decimalPlaces, true));
	}

	public FloatBasedDecimalField(int width, String label, int decimalPlaces, boolean withSign) {
		super(width, label, new DecimalToFloatConverter(decimalPlaces, withSign));
	}

}
