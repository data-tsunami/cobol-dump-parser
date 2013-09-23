package ar.com.datatsunami.pig;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.pig.EvalFunc;
import org.apache.pig.data.Tuple;

/**
 * This UDF receives the integer and decimal part of a number, the sign, and the
 * number of decimal places, and returns a Float.
 * 
 * For example:
 * 
 * - integer part = 934
 * 
 * - decimal part = 71
 * 
 * - sign = '-'
 * 
 * - decimal places = 5
 * 
 * - float returned: -934.00071
 * 
 */
public class SignedDecimalToFloat extends EvalFunc<Float> {

	Log logger = LogFactory.getLog(SignedDecimalToFloat.class);

	public SignedDecimalToFloat() {
		logger.debug("Instantiating...");
	}

	@Override
	public Float exec(Tuple input) throws IOException {

		// int_part, decimal_part, decimal_places
		if (input == null || input.size() != 4) {
			if (logger.isDebugEnabled())
				logger.debug("Returning null because we don't received 3 fields");
			return null;
		}

		long intPart;
		long decimalPart;
		long decimalPlaces;
		float sign; // +1 or -1

		try {
			intPart = getLong(input.get(0));
			decimalPart = getLong(input.get(1));
			decimalPlaces = getLong(input.get(3));
		} catch (NumberFormatException nfe) {
			logger.debug("Returning null because getLong() raised NumberFormatException");
			return null;
		}

		sign = this.getSign(input.get(2));
		if (sign == 0) {
			logger.debug("Returning null because couldn't get a valid sign");
			return null;
		}

		return sign * toFloat(intPart, decimalPart, decimalPlaces);
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

	/*
	 * Try to get a long representing the sign (+1 or -1).
	 * 
	 * Returns '+1' if the sign is '+'.
	 * 
	 * Returns '-1' if the sign is '-'.
	 * 
	 * Returns '0' if can't get a sign.
	 */
	public float getSign(Object object) throws NumberFormatException {

		if (String.class.isAssignableFrom(object.getClass())) {
			//
			// Object is a String, check value and return
			//
			if ("+".equals(object))
				return 1f;
			if ("-".equals(object))
				return -1f;
			return 0f;
		}

		if (logger.isDebugEnabled())
			logger.debug("getSign(): object isn't the string '+' nor '-' - class: '" + object.getClass()
					+ "' - object: '" + object + "'");

		logger.warn("Object for sign isn't a String instance");

		//
		// Object isn't a String. If equals to '+' or '-', return
		//
		if ("+".equals(object))
			return 1f;
		if ("-".equals(object))
			return -1f;

		//
		// Try to get a String representing the object
		//
		String string;
		try {
			string = object.toString();
		} catch (Exception e) {
			if (logger.isDebugEnabled())
				logger.debug("object.toString() raised an exception", e);
			throw new NumberFormatException("object.toString() raised the exception: " + e);
		}

		if ("+".equals(string))
			return 1f;
		if ("-".equals(string))
			return -1f;

		return 0f;
	}

}
