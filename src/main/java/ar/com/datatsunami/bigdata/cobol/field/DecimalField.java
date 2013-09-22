package ar.com.datatsunami.bigdata.cobol.field;

import org.apache.commons.lang.NotImplementedException;

import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;
import ar.com.datatsunami.bigdata.cobol.format.DecimalFormat;

public class DecimalField extends Field<Float> {

	public static final String DECIMAL_FIELD_PREFIX_FOR_PIG = "_decimal";

	public static final String SIGN_FIELD_PREFIX_FOR_PIG = "_sign";

	public DecimalField(int cantidadLugares, String label, int decimalPlaces) {
		super(cantidadLugares, label, new DecimalFormat(decimalPlaces));
	}

	public DecimalField(int cantidadLugares, String label, int decimalPlaces, boolean withSign) {
		super(cantidadLugares, label, new DecimalFormat(decimalPlaces, withSign));
	}

	@Override
	public PigSchema getPigSchema() {
		throw new NotImplementedException();
	}

}
