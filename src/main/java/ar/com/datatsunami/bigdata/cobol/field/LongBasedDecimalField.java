package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.converter.DecimalToLongConverter;

public class LongBasedDecimalField extends BaseDecimalField<Long, DecimalToLongConverter> {

	/**
	 * Creates a decimal field based in longs, by defalut WITH SIGN.
	 * 
	 * @param width
	 * @param label
	 * @param decimalPlaces
	 */
	public LongBasedDecimalField(int width, String label, int decimalPlaces) {
		super(width, label, new DecimalToLongConverter(decimalPlaces, true));
	}

	public LongBasedDecimalField(int width, String label, int decimalPlaces, boolean withSign) {
		super(width, label, new DecimalToLongConverter(decimalPlaces, withSign));
	}

}
