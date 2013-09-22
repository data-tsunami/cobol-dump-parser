package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.format.DecimalToLongConverter;

public class LongBasedDecimalField extends BaseDecimalField<Long> {

	public LongBasedDecimalField(int width, String label, int decimalPlaces) {
		super(width, label, new DecimalToLongConverter(decimalPlaces, true), decimalPlaces, true);
	}

	public LongBasedDecimalField(int width, String label, int decimalPlaces, boolean withSign) {
		super(width, label, new DecimalToLongConverter(decimalPlaces, withSign), decimalPlaces, withSign);
	}

}
