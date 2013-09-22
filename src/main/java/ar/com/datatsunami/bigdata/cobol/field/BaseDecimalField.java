package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;
import ar.com.datatsunami.bigdata.cobol.format.CobolFieldToJavaConverter;

public abstract class BaseDecimalField<I> extends Field<I> {

	public static final String DECIMAL_FIELD_PREFIX_FOR_PIG = "_decimal";

	public static final String SIGN_FIELD_PREFIX_FOR_PIG = "_sign";

	protected final int decimalPlaces;

	protected final boolean withSign;

	public BaseDecimalField(int width, String label, CobolFieldToJavaConverter<I> format, int decimalPlaces,
			boolean withSign) {
		super(width, label, format);
		this.decimalPlaces = decimalPlaces;
		this.withSign = withSign;
	}

	@Override
	public PigSchema getPigSchema() {
		PigSchema schema = new PigSchema();
		String intFieldName = this.label.toLowerCase();
		String decFieldName = this.label.toLowerCase() + DECIMAL_FIELD_PREFIX_FOR_PIG;

		if (!this.withSign) {

			/*
			 * Without sign
			 */

			int integerPartWidth = this.width - decimalPlaces;

			// add integer part
			schema.add(PigSchema.LONG, intFieldName, integerPartWidth, 0);

			// add decimal part
			schema.add(PigSchema.LONG, decFieldName, decimalPlaces, this.width - decimalPlaces /* offset */);

		} else {

			/*
			 * With sign
			 */

			int integerPartWidth = this.width - decimalPlaces - 1;
			String fieldName = this.label.toLowerCase() + SIGN_FIELD_PREFIX_FOR_PIG;

			// add integer
			schema.add(PigSchema.LONG, intFieldName, integerPartWidth, 0);

			// add decimal part
			schema.add(PigSchema.LONG, decFieldName, decimalPlaces, integerPartWidth);

			// add sign
			schema.add(PigSchema.CHARARRAY, fieldName, 1, this.width - 1);

		}
		return schema;
	}
}
