package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.converter.BaseDecimalConverter;
import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;

public abstract class BaseDecimalField<I, J extends BaseDecimalConverter<I>> extends Field<I, J> {

	public static final String DECIMAL_FIELD_PREFIX_FOR_PIG = "_decimal";

	public static final String SIGN_FIELD_PREFIX_FOR_PIG = "_sign";

	public BaseDecimalField(int width, String label, J converter) {
		super(width, label, converter);
	}

	@Override
	public PigSchema getPigSchema() {
		PigSchema schema = new PigSchema();
		String intFieldName = this.label.toLowerCase();
		String decFieldName = this.label.toLowerCase() + DECIMAL_FIELD_PREFIX_FOR_PIG;

		if (!this.isWithSign()) {

			/*
			 * Without sign
			 */

			int integerPartWidth = this.width - getDecimalPlaces();

			// add integer part
			schema.add(PigSchema.LONG, intFieldName, integerPartWidth, 0);

			// add decimal part
			schema.add(PigSchema.LONG, decFieldName, getDecimalPlaces(), this.width - getDecimalPlaces());

		} else {

			/*
			 * With sign
			 */

			int integerPartWidth = this.width - getDecimalPlaces() - 1;
			String fieldName = this.label.toLowerCase() + SIGN_FIELD_PREFIX_FOR_PIG;

			// add integer
			schema.add(PigSchema.LONG, intFieldName, integerPartWidth, 0);

			// add decimal part
			schema.add(PigSchema.LONG, decFieldName, getDecimalPlaces(), integerPartWidth);

			// add sign
			schema.add(PigSchema.CHARARRAY, fieldName, 1, this.width - 1);

		}
		return schema;
	}

	public int getDecimalPlaces() {
		return this.converter.decimalPlaces;
	}

	public boolean isWithSign() {
		return this.converter.withSign;
	}
}
