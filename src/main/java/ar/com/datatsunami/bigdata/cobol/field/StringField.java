package ar.com.datatsunami.bigdata.cobol.field;

import ar.com.datatsunami.bigdata.cobol.converter.CobolFieldToJavaConverter;
import ar.com.datatsunami.bigdata.cobol.converter.StringConverter;
import ar.com.datatsunami.bigdata.cobol.field.pig.PigSchema;

public class StringField extends Field<String, StringConverter> {

	public StringField(int cantidadLugares, String label) {
		super(cantidadLugares, label, (StringConverter)CobolFieldToJavaConverter.DEFAULT_CONVERTER);
	}

	@Override
	public PigSchema getPigSchema() {
		return new PigSchema(PigSchema.CHARARRAY, label.toLowerCase(), this.width, 0);
	}

}
