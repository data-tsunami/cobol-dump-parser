package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;
import ar.com.datatsunami.bigdata.cobol.format.LongBasedDecimalFormat;

public class LongBasedDecimalField extends Field<Long> {

	public static final String DECIMAL_FIELD_PREFIX_FOR_PIG = "_decimal";

	public static final String SIGN_FIELD_PREFIX_FOR_PIG = "_sign";

	public LongBasedDecimalField(int cantidadLugares, String label, int decimalPlaces) {
		super(cantidadLugares, label, new LongBasedDecimalFormat(decimalPlaces));
	}

	public LongBasedDecimalField(int cantidadLugares, String label, int decimalPlaces, boolean withSign) {
		super(cantidadLugares, label, new LongBasedDecimalFormat(decimalPlaces, withSign));
	}

	@Override
	public PigSchema getPigSchema() {
		PigSchema schema = new PigSchema();
		LongBasedDecimalFormat df = (LongBasedDecimalFormat) this.format;
		String intFieldName = this.label.toLowerCase();
		String decFieldName = this.label.toLowerCase() + DECIMAL_FIELD_PREFIX_FOR_PIG;

		if (!df.withSign) {

			/*
			 * Without sign
			 */

			int integerPartWidth = this.width - df.decimalPlaces;

			// add integer part
			schema.add(PigSchema.LONG, intFieldName, integerPartWidth, 0);

			// add decimal part
			schema.add(PigSchema.LONG, decFieldName, df.decimalPlaces, this.width - df.decimalPlaces /* offset */);

		} else {

			/*
			 * With sign
			 */

			int integerPartWidth = this.width - df.decimalPlaces - 1;
			String fieldName = this.label.toLowerCase() + SIGN_FIELD_PREFIX_FOR_PIG;

			// add integer
			schema.add(PigSchema.LONG, intFieldName, integerPartWidth, 0);

			// add decimal part
			schema.add(PigSchema.LONG, decFieldName, df.decimalPlaces, integerPartWidth);

			// add sign
			schema.add(PigSchema.CHARARRAY, fieldName, 1, this.width - 1);

		}
		return schema;
	}

}
