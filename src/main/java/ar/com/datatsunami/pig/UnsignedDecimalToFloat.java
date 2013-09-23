package ar.com.datatsunami.pig;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.pig.EvalFunc;
import org.apache.pig.data.Tuple;

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

}
