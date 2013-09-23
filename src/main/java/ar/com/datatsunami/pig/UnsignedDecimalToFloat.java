package ar.com.datatsunami.pig;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.pig.EvalFunc;
import org.apache.pig.FuncSpec;
import org.apache.pig.data.DataType;
import org.apache.pig.data.Tuple;
import org.apache.pig.impl.logicalLayer.FrontendException;
import org.apache.pig.impl.logicalLayer.schema.Schema;

/**
 * This UDF receives the integer and decimal part of a number, and the number of
 * decimal places, and returns a Float.
 * 
 * For example:
 * 
 * - integer part = 934
 * 
 * - decimal part = 71
 * 
 * - decimal places = 5
 * 
 * - float returned: 934.00071
 * 
 */
public class UnsignedDecimalToFloat extends EvalFunc<Float> {

	Log logger = LogFactory.getLog(UnsignedDecimalToFloat.class);

	public UnsignedDecimalToFloat() {
		logger.debug("Instantiating...");
	}

	@Override
	public Float exec(Tuple input) throws IOException {

		// int_part, decimal_part, decimal_places
		if (input == null || input.size() != 3) {
			if (logger.isDebugEnabled())
				logger.debug("Returning null because we don't received 3 fields");
			return null;
		}

		long intPart;
		long decimalPart;
		long decimalPlaces;

		try {
			intPart = Utils.getLong(input.get(0));
			decimalPart = Utils.getLong(input.get(1));
			decimalPlaces = Utils.getLong(input.get(2));
		} catch (NumberFormatException nfe) {
			logger.debug("Returning null because getLong() raised NumberFormatException");
			return null;
		}

		return Utils.toFloat(intPart, decimalPart, decimalPlaces);
	}

	@Override
	public List<FuncSpec> getArgToFuncMapping() throws FrontendException {
		List<FuncSpec> funcList = new ArrayList<FuncSpec>();
		Schema schema;

		schema = new Schema();
		schema.add(new Schema.FieldSchema(null, DataType.LONG));
		schema.add(new Schema.FieldSchema(null, DataType.LONG));
		schema.add(new Schema.FieldSchema(null, DataType.INTEGER));
		funcList.add(new FuncSpec(this.getClass().getName(), schema));

		schema = new Schema();
		schema.add(new Schema.FieldSchema(null, DataType.LONG));
		schema.add(new Schema.FieldSchema(null, DataType.LONG));
		schema.add(new Schema.FieldSchema(null, DataType.LONG));
		funcList.add(new FuncSpec(this.getClass().getName(), schema));

		return funcList;
	}
}
