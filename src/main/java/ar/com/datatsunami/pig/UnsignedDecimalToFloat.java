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
			intPart = getLong(input.get(0));
			decimalPart = getLong(input.get(1));
			decimalPlaces = getLong(input.get(2));
		} catch (NumberFormatException nfe) {
			logger.debug("Returning null because getLong() raised NumberFormatException");
			return null;
		}

		return toFloat(intPart, decimalPart, decimalPlaces);
	}

	/*
	 * Transform 3 long to a float
	 */
	public static float toFloat(long intPart, long decimalPart, long decimalPlaces) {
		if (decimalPart == 0) {
			return (float) intPart;
		}
		long divisor = (long) Math.pow(10, decimalPlaces);
		return ((float) intPart) + (((float) decimalPart) / ((float) divisor));
	}

	/*
	 * Try to get a long from the object
	 */
	public long getLong(Object object) throws NumberFormatException {

		if (Number.class.isAssignableFrom(object.getClass()))
			return (long) ((Number) object).longValue();

		if (logger.isDebugEnabled())
			logger.debug("getLong(): object isn't Number: " + object.getClass());

		String string;
		try {
			string = object.toString();
		} catch (Exception e) {
			if (logger.isDebugEnabled())
				logger.debug("object.toString() raised an exception", e);
			throw new NumberFormatException("object.toString() raised the exception: " + e);
		}

		try {
			return Long.parseLong(string);
		} catch (NumberFormatException nfe) {
			if (logger.isDebugEnabled())
				logger.debug("Couldn't get a valid long from the object of type '" + object.getClass()
						+ "', represented by string: '" + string + "'");
			throw nfe;
		}
	}

}
